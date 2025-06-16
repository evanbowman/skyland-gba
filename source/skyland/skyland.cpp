////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "skyland.hpp"
#include "eternal/eternal.hpp"
#include "globals.hpp"
#include "graphics/overlay.hpp"
#include "macrocosmEngine.hpp"
#include "number/random.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "player/playerP1.hpp"
#include "room_metatable.hpp"
#include "save.hpp"
#include "scene/notificationScene.hpp"
#include "scene/readyScene.hpp"
#include "script/lisp.hpp"
#include "serial.hpp"
#include "sound.hpp"
#include "timeStreamEvent.hpp"
#include "weather/storm.hpp"
extern "C" {
// FIXME! CMake target_link_libraries isn't working for some reason?
#include "heatshrink/heatshrink_encoder.c"
}
#include "latency.hpp"



namespace skyland
{



void init_clouds();



Player& player()
{
    return APP.player();
}



static const char* const hidden_rooms_file = "/save/hidden.txt";
static const char rooms_hidden_delim = ',';



void load_hidden_rooms()
{
    Vector<char> data;
    flash_filesystem::read_file_data_binary(hidden_rooms_file, data);

    StringBuffer<64> parse_buf;

    auto it = data.begin();
    for (; it not_eq data.end(); ++it) {
        if (*it == '\0') {
            break;
        } else if (*it == rooms_hidden_delim) {
            auto mti = metaclass_index(parse_buf.c_str());
            if (mti) {
                room_set_hidden(mti, true);
            }
            parse_buf.clear();
        } else {
            parse_buf.push_back(*it);
        }
    }
}



void store_hidden_rooms()
{
    Vector<char> data;

    const auto [mts, mt_count] = room_metatable();
    for (int i = 0; i < mt_count; ++i) {
        if (room_hidden(i)) {
            auto name = mts[i]->name();
            while (*name not_eq '\0') {
                data.push_back(*name);
                ++name;
            }
            data.push_back(rooms_hidden_delim);
        }
    }

    flash_filesystem::store_file_data_binary(
        hidden_rooms_file, data, {.use_compression_ = true});
}



bool skip_crash_report_ = false;



void skip_crash_report()
{
    skip_crash_report_ = true;
}



void create_crash_report(const char* error_text)
{
    if (skip_crash_report_) {
        return;
    }

    Vector<char> text;
    auto append = [&text](const char* str) {
        while (*str not_eq '\0') {
            text.push_back(*str);
            ++str;
        }
        text.push_back('\n');
    };

    append("error:");
    append(error_text);

    append("");

    append("lisp stack:");
    lisp::l_foreach(lisp::stacktrace(), [&](lisp::Value* v) {
        append(lisp::val_to_string<96>(v).c_str());
    });

    append("");

    flash_filesystem::store_file_data("/crash/report.txt", text);
}



App* __app__;


App::App(bool clean_boot)
    : level_timer_(0), stat_timer_(0),
      world_state_(allocate_dynamic<WorldState>("env-buffer",

                                                Layer::map_0_ext,
                                                5,
                                                player())),
      current_scene_(null_scene()), next_scene_(null_scene()),
      backup_(allocate_dynamic<save::EmergencyBackup>("emergency-backup"))
{
    __app__ = this;

    player_.emplace<PlayerP1>();

    init_clouds();


    current_scene_ = initial_scene(clean_boot);
    next_scene_ = initial_scene(clean_boot);

    custom_flag_image_.load();

    if (not save::load_global_data(gp_)) {
        info("global data not found");
        for (auto& score : gp_.highscores_.values_) {
            score.set(0);
        }

        // Hidden by default. Just as an incentive to the player to figure out
        // how the room hide menu works.
        room_set_hidden(metaclass_index("sunflower"), true);
        store_hidden_rooms();
    }

    load_hidden_rooms();

    // On unrecoverrable errors: try to store a backup, and flush the system log
    // to sram.
    PLATFORM.on_unrecoverrable_error([this](const char* error_text) {
        store_backup();
        if (is_developer_mode()) {
            PLATFORM.logger().flush();
        }
        // After a crash, someone might keep playing the game, and if I ask for
        // their save file, information useful for reproducing the crash might
        // already be gone. So copy the player's progress at the time of the
        // crash to a crash directory, so that I can investigate it more easily.
        flash_filesystem::copy_file("/save/adventure.dat",
                                    "/crash/adventure.dat");

        flash_filesystem::copy_file("/save/adventure.lisp",
                                    "/crash/adventure.lisp");

        create_crash_report(error_text);
    });

    // If the platform runs out of scratch buffers, try to do anything that we
    // can to free up non-essential or potentially unused buffers. NOTE: I
    // technically haven't tested this code, because the application still has
    // at least 120kb of unused RAM in the worst case, so I'm not on the verge
    // of running out.
    set_scratch_buffer_oom_handler([this] {
        lisp::gc();
        if (time_stream_.has_multiple_buffers()) {
            time_stream_.free_single_buffer();
        } else {
            time_stream_.clear();
        }
    });

    const auto sb = StateBit::remote_console_force_newline;
    state_bit_store(sb, true);

    info("initialized application...");
}



Coins App::terrain_cost(Island& island)
{
    Coins terrain_cost_table[Island::Terrain::capacity()] = {
        200,
        300,
        400,
        500,
        600,
        800,
        1200,
        1400,
        1800,
        2000,
        2400,
        2800,
        3200,
        // Terrain wider than 13 tiles isn't supported in most game modes!
        99999999,
        99999999,
        99999999,
    };

    return terrain_cost_table[island.terrain().size() - 1];
}



bool App::has_backup()
{
    return backup_->valid_;
}



void App::store_backup()
{
    if (backup_->valid_) {
        backup_->store();
    }
}



void App::restore_backup()
{
    if (not backup_->valid_) {
        Platform::fatal("restore from invalid backup!");
    }

    player_island().projectiles().clear();

    persistent_data_ = backup_->persistent_data_;
    rng::critical_state = backup_->rng_state_;

    invoke_script("/scripts/reset_hooks.lisp");

    lisp::VectorCharSequence seq(*backup_->lisp_data_);
    lisp::read(seq);
    lisp::eval(lisp::get_op(0));

    auto arg = lisp::get_op(0); // result of eval()

    auto fn = invoke_script("/scripts/restore_save.lisp");
    if (fn->type() == lisp::Value::Type::function) {
        lisp::push_op(arg); // pass save data buffer on stack
        safecall(fn, 1);    // one argument (the save data)
        lisp::pop_op();     // funcall result
    } else {
        PLATFORM.fatal("not function!");
    }

    lisp::pop_op(); // result of eval() (1)
    lisp::pop_op(); // result of read() (0)

    player_island().fires_extinguish();

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            PLATFORM.set_tile(player_island().layer(), x, y, 0);
        }
    }

    current_world_location() = backup_->next_world_location_;
    ++world_graph().storm_depth_;
}



void App::create_backup(const BackupContext& ctx)
{
    backup_->init();
    backup_->next_world_location_ = ctx.next_world_location_;
}



save::EmergencyBackup* App::get_backup()
{
    if (not has_backup()) {
        return nullptr;
    }

    return &*backup_;
}



void App::delete_backup()
{
    backup_->valid_ = false;
    backup_->lisp_data_.reset();
}



void write_custom_graphics()
{
    vram_write_flag(APP.custom_flag_image_, Layer::map_0_ext);
}



// clang-format off
static constexpr const char* console_header =
"\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"|  Skyland OS                                                                  |\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"Options: (s: simple console, l: lisp repl)\r\n";
// clang-format on



void App::start_console()
{
    PLATFORM.remote_console().start();
    PLATFORM.remote_console().printline(console_header);
}



#if not MAPBOX_ETERNAL_IS_CONSTEXPR
#error "NON-Constexpr lookup table!"
#endif



static bool is_gui_sound(const char* sound_name)
{
    MAPBOX_ETERNAL_CONSTEXPR const auto gui_sound_table =
        mapbox::eternal::map<mapbox::eternal::string, int>(
            {{"click", 0},
             {"drone_beep", 0},
             {"openbag", 0},
             {"beep_error", 0},
             {"click_wooden", 0},
             {"button_wooden", 0},
             {"click_negative", 0},
             {"cursor_tick", 0},
             {"seagull_1.raw", 0},
             {"seagull_2.raw", 0},
             {"weapon_target", 0}});

    return gui_sound_table.find(sound_name) not_eq gui_sound_table.end();
}



static int scratch_buffer_highwater = 0;



void App::update(Time delta)
{
    const auto previous_rng = rng::critical_state;
    const auto previous_score = score().get();

    if (next_scene_) {
        next_scene_->enter(*current_scene_);

        current_scene_ = std::move(next_scene_);
    }

    Sound::update_all(delta);

    auto line = PLATFORM.remote_console().readline();
    if (UNLIKELY(static_cast<bool>(line))) {
        if (not console_state_) {
            console_state_.emplace(allocate_dynamic<ConsoleState>("console"));
        }
        (*console_state_)->impl_->on_text((*console_state_)->impl_, *line);
    }

    rumble_.update(delta);

    if (game_speed() not_eq GameSpeed::stopped) {
        for (auto it = deferred_callbacks_.begin();
             it not_eq deferred_callbacks_.end();) {

            it->second -= delta;

            if (not(it->second > 0)) {
                it->first();
                it = deferred_callbacks_.erase(it);
            } else {
                ++it;
            }
        }
    }

    next_scene_ = current_scene_->update(delta);

    if (next_scene_) {
        current_scene_->exit(*next_scene_);
    }

    if (rng::critical_state not_eq previous_rng) {
        time_stream::event::RngChanged e;
        e.previous_state_.set(previous_rng);

        time_stream_.push(level_timer_, e);
    }

    if (score().get() not_eq previous_score) {
        record_score_diff(score().get() - previous_score);
    }

    for (const char* sound : PLATFORM.speaker().completed_sounds()) {
        // Do not play sounds associated with the game's ui.
        if (not is_gui_sound(sound)) {

            if (str_eq(sound, "cannon")) {
                time_stream::event::CannonSoundCompleted e;
                time_stream_.push(level_timer_, e);
            } else if (str_eq(sound, "missile")) {
                time_stream::event::MissileSoundCompleted e;
                time_stream_.push(level_timer_, e);
            } else if (str_eq(sound, "impact")) {
                time_stream::event::HitSoundCompleted e;
                time_stream_.push(level_timer_, e);
            } else {
                time_stream::event::SoundCompleted e;
                e.sound_name_ptr_.set((intptr_t)sound);
                time_stream_.push(level_timer_, e);
            }
        }
    }


    // if (PLATFORM.keyboard().pressed<Key::select>()) {
    //     Platform::fatal(format("% % % % % %",
    //                            t2 - t1,
    //                            t3 - t2,
    //                            t4 - t3,
    //                            t5 - t4,
    //                            t6 - t5,
    //                            t7 - t6)
    //                         .c_str());
    // }

    if (scratch_buffers_in_use() > scratch_buffer_highwater) {
        scratch_buffer_highwater = scratch_buffers_in_use();

        StringBuffer<60> str = "sbr highwater: ";

        str += stringify(scratch_buffer_highwater).c_str();
        str += " (";
        str += stringify(scratch_buffers_remaining());
        str += " left)";

        info(str.c_str());
    }
}



void App::record_score_diff(int diff)
{
    if (diff > 0) {

        int packed_diff = diff;
        int packed_exp = 0;

        for (int i = 0; i < 3; ++i) {
            if (packed_diff % 10 == 0) {
                packed_diff /= 10;
                ++packed_exp;
            } else {
                break;
            }
        }

        if (packed_diff <= (2 << 6) - 1) {
            time_stream::event::ScoreIncreasedSmall__packed e;
            e.amount_ = packed_diff;
            e.mul_10_ = packed_exp;
            time_stream().push(level_timer(), e);
        } else if (diff <= std::numeric_limits<u16>::max()) {
            time_stream::event::ScoreIncreasedLarge e;
            e.amount_.set(diff);
            time_stream().push(level_timer(), e);
        } else {
            time_stream::event::ScoreIncreasedHuge e;
            e.amount_.set(diff);
            time_stream().push(level_timer(), e);
        }
    } else {
        time_stream::event::ScoreDecreased e;
        e.amount_.set(-1 * diff);
        time_stream().push(level_timer(), e);
    }
}



void App::clear_effects_lowpriority()
{
    EntityList<Entity> temp;

    while (auto e = effects().pop_last()) {
        if (not(*e)->entity_oom_deletable()) {
            temp.push(std::move(*e));
        }
    }

    while (auto e = temp.pop_last()) {
        effects().push(std::move(*e));
    }
}



void App::update_parallax(Time delta)
{
    cloud_scroll_1fp_ += 0.00002_fixed * Fixnum::from_integer(delta);
    cloud_scroll_2fp_ += 0.00004_fixed * Fixnum::from_integer(delta);
}



void App::_render_update_scroll()
{
    PLATFORM_EXTENSION(update_parallax_r1, (u8)cloud_scroll_1fp_.as_integer());
    PLATFORM_EXTENSION(update_parallax_r2, (u8)cloud_scroll_2fp_.as_integer());
}



void App::render()
{
    if (not macrocosm()) {
        _render_update_scroll();
    }

    current_scene_->display();
}



void App::set_coins(Coins coins)
{
    time_stream::event::CoinsChanged e;
    e.previous_value_.set(persistent_data_.coins_);
    time_stream_.push(level_timer_, e);

    persistent_data_.coins_ = coins;
}



void init_clouds()
{
    PLATFORM_EXTENSION(enable_parallax_clouds, true);

    for (int i = 0; i < 32; ++i) {
        for (int j = 0; j < 32; ++j) {
            PLATFORM.set_tile(Layer::background, i, j, 4);
        }
    }

    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 2; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 72);
        }
        for (int y = 2; y < 4; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 73);
        }
        for (int y = 4; y < 6; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 74);
        }
        for (int y = 6; y < 8; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 75);
        }
    }

    for (int i = 0; i < 32; ++i) {
        PLATFORM.set_tile(Layer::background, i, 18, 5);
        PLATFORM.set_tile(Layer::background, i, 19, 5);
    }

    auto put_cloud_block = [&](int x, int y, int offset) {
        PLATFORM.set_tile(Layer::background, x, y, offset++);
        PLATFORM.set_tile(Layer::background, x + 1, y, offset++);
        PLATFORM.set_tile(Layer::background, x, y + 1, offset++);
        PLATFORM.set_tile(Layer::background, x + 1, y + 1, offset);
    };

    auto put_fg_cloud_type_n = [&](int x, int type) {
        put_cloud_block(x * 2, 16, 8 + type * 4);
        put_cloud_block(x * 2, 18, 48 + type * 4);
    };

    auto put_bg_cloud_type_n = [&](int x, int type) {
        put_cloud_block(x * 2, 14, 32 + type * 4);
    };

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 6;
        put_fg_cloud_type_n(offset + 0, 0);
        put_fg_cloud_type_n(offset + 1, 1);
        put_fg_cloud_type_n(offset + 2, 2);
        put_fg_cloud_type_n(offset + 3, 3);
        put_fg_cloud_type_n(offset + 4, 4);
        put_fg_cloud_type_n(offset + 5, 5);
    }

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 4;
        put_bg_cloud_type_n(offset + 0, 0);
        put_bg_cloud_type_n(offset + 1, 1);
        put_bg_cloud_type_n(offset + 2, 2);
        put_bg_cloud_type_n(offset + 3, 3);
    }

    // This is just a band of tiles to fill the vertical gap between rows when
    // parallax-scrolling the the cloud layers. The implementation uses raster
    // lines from these rows, scrolled up into the gaps between tiles created by
    // vertical parallax scrolling.
    for (int i = 0; i < 32; ++i) {
        PLATFORM.set_tile(Layer::background, i, 20, 7);
        PLATFORM.set_tile(Layer::background, i, 21, 7);
        PLATFORM.set_tile(Layer::background, i, 22, 7);
        PLATFORM.set_tile(Layer::background, i, 23, 7);
    }
}



lisp::Value* App::invoke_ram_script(const char* ram_fs_path)
{
    if (not is_developer_mode()) {
        return L_NIL;
    }

    Vector<char> buffer;
    if (flash_filesystem::read_file_data_text(ram_fs_path, buffer)) {
        lisp::VectorCharSequence seq(buffer);
        return lisp::dostring(seq, [](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            PLATFORM.fatal(p.data_.c_str());
        });
    }

    return L_NIL;
}



bool App::is_developer_mode()
{
    return gp_.stateflags_.get(GlobalPersistentData::developer_mode);
}



void App::set_developer_mode(bool value)
{
    return gp_.stateflags_.set(GlobalPersistentData::developer_mode, value);
}



bool App::load_file(const char* path, Vector<char>& result)
{
    if (flash_filesystem::read_file_data(path, result)) {
        return true;
    }

    auto fd = Platform::instance().load_file("", path);
    if (fd.second) {
        for (u32 i = 0; i < fd.second; ++i) {
            result.push_back(fd.first[i]);
        }
        return true;
    }

    return false;
}



const char* seek_filename(const char* path)
{
    const auto begin = path;

    while (*path not_eq '\0') {
        ++path;
    }
    while (*path not_eq '/' and path not_eq begin) {
        --path;
    }
    if (path == begin) {
        return begin;
    }
    ++path;
    return path;
}



lisp::Value*
App::invoke_script(const char* path,
                   bool rom_fs_only,
                   Optional<Function<16, void(lisp::Value& err)>> err_handler)
{
    auto on_err = [path](lisp::Value& err) {
        lisp::DefaultPrinter p;
        lisp::format(&err, p);
        auto file = seek_filename(path);
        PLATFORM.fatal(format<256>("%: %", file, p.data_.c_str()));
    };

    if (not err_handler) {
        err_handler = on_err;
    }

    if (is_developer_mode() and not PLATFORM.network_peer().is_connected() and
        game_mode_ not_eq GameMode::tutorial and not rom_fs_only) {

        Vector<char> buffer;
        if (flash_filesystem::read_file_data_text(path, buffer)) {
            lisp::VectorCharSequence seq(buffer);
            auto result = lisp::dostring(seq, *err_handler);
            // In case the script took a bit to execute.
            PLATFORM.delta_clock().reset();
            return result;
        }
    }

    if (path[0] == '/') {
        ++path;
    }

    if (auto contents = PLATFORM.load_file_contents("", path)) {
        lisp::BasicCharSequence seq(contents);
        auto result = lisp::dostring(seq, *err_handler);
        PLATFORM.delta_clock().reset();
        return result;
    } else {
        StringBuffer<100> err("script '");
        err += path;
        err += "' missing";
        PLATFORM.fatal(err.c_str());
    }
}



weather::Environment& App::environment()
{
    return *environment_;
}



Faction& App::faction()
{
    return faction_;
}



macro::EngineImpl& macrocosm()
{
    auto& m = APP.macrocosm();
    if (m) {
        // NOTE: Only macro::EngineImpl derives from macro::State, so this cast
        // is fine. We're just trying to hide the implementation to reduce build
        // times.
        return *static_cast<macro::EngineImpl*>(&**m);
    } else {
        Platform::fatal("access to unbound macrocosm");
    }
}



void state_bit_store(StateBit state_bit, bool value)
{
    APP.state_bits().set((int)state_bit, value);
}



bool state_bit_load(StateBit state_bit)
{
    if (not __app__) {
        return false;
    }
    return APP.state_bits().get((int)state_bit);
}



void parallax_background_task()
{
    if (APP.game_speed() not_eq GameSpeed::stopped) {
        APP.update_parallax(milliseconds(16));
        APP._render_update_scroll();
    }
}



ScenePtr reject_if_friendly()
{
    if (APP.opponent_island() and
        // NOTE: cast should be safe, as a derived instance of Opponent should
        // always be bound to the opponent island.
        (static_cast<Opponent&>(APP.opponent_island()->owner()))
            .is_friendly()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 3);
        auto str = SYSTR(error_friendly);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }

    return null_scene();
}



} // namespace skyland



void logic_error(const char* file, int line)
{
    Platform::fatal(format("logic error, line %, file %", line, file));
}
