////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
#include "script/lisp.hpp"
#include "serial.hpp"
#include "sound.hpp"
#include "timeStreamEvent.hpp"
#include "weather/storm.hpp"
extern "C" {
// FIXME! CMake target_link_libraries isn't working for some reason?
#include "heatshrink/heatshrink_encoder.c"
}



namespace skyland
{



void init_clouds(Platform& pfrm);



Player& player(App& app)
{
    return app.player();
}



App::App(Platform& pfrm, bool clean_boot)
    : level_timer_(0), stat_timer_(0),
      world_state_(allocate_dynamic<WorldState>("env-buffer",
                                                pfrm,
                                                Layer::map_0_ext,
                                                5,
                                                player())),
      current_scene_(null_scene()), next_scene_(null_scene()),
      backup_(allocate_dynamic<save::EmergencyBackup>("emergency-backup"))
{
    player_.emplace<PlayerP1>();

    init_clouds(pfrm);


    current_scene_ = initial_scene(clean_boot);
    next_scene_ = initial_scene(clean_boot);

    custom_flag_image_.load(pfrm, *this);

    if (not save::load_global_data(pfrm, gp_)) {
        info(pfrm, "global data not found");
        for (auto& score : gp_.highscores_.values_) {
            score.set(0);
        }

        // Hidden by default. Just as an incentive to the player to figure out
        // how the room hide menu works.
        gp_.hidden_rooms_.set(metaclass_index("sunflower"), true);
        gp_.hidden_rooms_.set(metaclass_index("crane"), true);
    }

    // On unrecoverrable errors: try to store a backup, and flush the system log
    // to sram.
    pfrm.on_unrecoverrable_error([this](Platform& pfrm) {
        if (backup_->valid_) {
            backup_->store(pfrm);
        }
        if (is_developer_mode()) {
            pfrm.logger().flush();
        }
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
    state_bit_store(*this, sb, true);
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



void App::create_backup(Platform& pfrm)
{
    backup_->init(pfrm, *this);
}



void App::delete_backup()
{
    backup_->valid_ = false;
    backup_->lisp_data_.reset();
}



void write_custom_graphics(Platform& pfrm, App& app)
{
    vram_write_flag(pfrm, app.custom_flag_image_);
}



// clang-format off
static constexpr const char* console_header =
"\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"|  Skyland Console                                                             |\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"Options: (s: simple console, l: lisp repl)\r\n";
// clang-format on



void App::start_console(Platform& pfrm)
{
    pfrm.remote_console().start();
    pfrm.remote_console().printline(console_header);
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
             {"seagull_1", 0},
             {"seagull_2", 0},
             {"weapon_target", 0}});

    return gui_sound_table.find(sound_name) not_eq gui_sound_table.end();
}



void App::update(Platform& pfrm, Microseconds delta)
{
    const auto previous_rng = rng::critical_state;

    if (next_scene_) {
        next_scene_->enter(pfrm, *this, *current_scene_);

        current_scene_ = std::move(next_scene_);
    }

    Sound::update_all(delta);

    auto line = pfrm.remote_console().readline();
    if (UNLIKELY(static_cast<bool>(line))) {
        if (not console_state_) {
            console_state_.emplace(allocate_dynamic<ConsoleState>("console"));
        }
        (*console_state_)
            ->impl_->on_text(pfrm, *this, (*console_state_)->impl_, *line);
    }

    rumble_.update(pfrm, delta);

    if (game_speed() not_eq GameSpeed::stopped) {
        for (auto it = deferred_callbacks_.begin();
             it not_eq deferred_callbacks_.end();) {

            it->second -= delta;

            if (not(it->second > 0)) {
                it->first(pfrm, *this);
                it = deferred_callbacks_.erase(it);
            } else {
                ++it;
            }
        }
    }

    next_scene_ = current_scene_->update(pfrm, *this, delta);

    if (next_scene_) {
        current_scene_->exit(pfrm, *this, *next_scene_);
    }

    if (rng::critical_state not_eq previous_rng) {
        time_stream::event::RngChanged e;
        e.previous_state_.set(previous_rng);

        time_stream_.push(level_timer_, e);
    }

    for (const char* sound : pfrm.speaker().completed_sounds()) {
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
}



void App::update_parallax(Microseconds delta)
{
    cloud_scroll_1_ += 0.00002f * delta;
    cloud_scroll_2_ += 0.00004f * delta;
}



void App::render(Platform& pfrm)
{
    if (not macrocosm()) {
        pfrm.system_call("_prlx7", (void*)(intptr_t)(u8)cloud_scroll_1_);
        pfrm.system_call("_prlx8", (void*)(intptr_t)(u8)cloud_scroll_2_);
    }

    current_scene_->display(pfrm, *this);
}



void App::set_coins(Platform& pfrm, Coins coins)
{
    time_stream::event::CoinsChanged e;
    e.previous_value_.set(persistent_data_.coins_);
    time_stream_.push(level_timer_, e);

    persistent_data_.coins_ = coins;
}



void init_clouds(Platform& pfrm)
{
    pfrm.system_call("parallax-clouds", (void*)true);

    for (int i = 0; i < 32; ++i) {
        for (int j = 0; j < 32; ++j) {
            pfrm.set_tile(Layer::background, i, j, 4);
        }
    }

    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 2; ++y) {
            pfrm.set_tile(Layer::background, x, y, 72);
        }
        for (int y = 2; y < 4; ++y) {
            pfrm.set_tile(Layer::background, x, y, 73);
        }
        for (int y = 4; y < 6; ++y) {
            pfrm.set_tile(Layer::background, x, y, 74);
        }
        for (int y = 6; y < 8; ++y) {
            pfrm.set_tile(Layer::background, x, y, 75);
        }
    }

    for (int i = 0; i < 32; ++i) {
        pfrm.set_tile(Layer::background, i, 18, 5);
        pfrm.set_tile(Layer::background, i, 19, 5);
    }

    auto put_cloud_block = [&](int x, int y, int offset) {
        pfrm.set_tile(Layer::background, x, y, offset++);
        pfrm.set_tile(Layer::background, x + 1, y, offset++);
        pfrm.set_tile(Layer::background, x, y + 1, offset++);
        pfrm.set_tile(Layer::background, x + 1, y + 1, offset);
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
        pfrm.set_tile(Layer::background, i, 20, 7);
        pfrm.set_tile(Layer::background, i, 21, 7);
        pfrm.set_tile(Layer::background, i, 22, 7);
        pfrm.set_tile(Layer::background, i, 23, 7);
    }
}



lisp::Value* App::invoke_ram_script(Platform& pfrm, const char* ram_fs_path)
{
    if (not is_developer_mode()) {
        return L_NIL;
    }

    Vector<char> buffer;
    if (flash_filesystem::read_file_data_text(pfrm, ram_fs_path, buffer)) {
        lisp::VectorCharSequence seq(buffer);
        return lisp::dostring(seq, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.data_.c_str());
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



lisp::Value*
App::invoke_script(Platform& pfrm, const char* path, bool rom_fs_only)
{
    auto on_err = [&pfrm](lisp::Value& err) {
        lisp::DefaultPrinter p;
        lisp::format(&err, p);
        pfrm.fatal(p.data_.c_str());
    };

    if (is_developer_mode() and not pfrm.network_peer().is_connected() and
        game_mode_ not_eq GameMode::tutorial and not rom_fs_only) {

        Vector<char> buffer;
        if (flash_filesystem::read_file_data_text(pfrm, path, buffer)) {
            lisp::VectorCharSequence seq(buffer);
            auto result = lisp::dostring(seq, on_err);
            // In case the script took a bit to execute.
            pfrm.delta_clock().reset();
            return result;
        }
    }

    if (path[0] == '/') {
        ++path;
    }

    if (auto contents = pfrm.load_file_contents("", path)) {
        lisp::BasicCharSequence seq(contents);
        auto result = lisp::dostring(seq, on_err);
        pfrm.delta_clock().reset();
        return result;
    } else {
        StringBuffer<100> err("script '");
        err += path;
        err += "' missing";
        pfrm.fatal(err.c_str());
    }
}



weather::Environment& App::environment()
{
    return *environment_;
}



macro::EngineImpl& macrocosm(App& app)
{
    auto& m = app.macrocosm();
    if (m) {
        // NOTE: Only macro::EngineImpl derives from macro::State, so this cast
        // is fine. We're just trying to hide the implementation to reduce build
        // times.
        return *static_cast<macro::EngineImpl*>(&**m);
    } else {
        Platform::fatal("access to unbound macrocosm");
    }
}



void state_bit_store(App& app, StateBit state_bit, bool value)
{
    app.state_bits().set((int)state_bit, value);
}



bool state_bit_load(App& app, StateBit state_bit)
{
    return app.state_bits().get((int)state_bit);
}



} // namespace skyland



void Text::platform_retain_alphabet(Platform& pfrm)
{
    Text t(pfrm, OverlayCoord{0, calc_screen_tiles(pfrm).y});
    t.assign(
        skyland::loadstr(pfrm, skyland::SystemString::patchfix_retain_alphabet)
            ->c_str());
    t.__detach();
}
