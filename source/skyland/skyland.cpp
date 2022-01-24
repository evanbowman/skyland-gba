#include "skyland.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "platform/ram_filesystem.hpp"
#include "save.hpp"
#include "script/lisp.hpp"
#include "serial.hpp"
#include "sound.hpp"
#include "timeStreamEvent.hpp"



namespace skyland {



void init_clouds(Platform& pfrm);



App::App(Platform& pfrm)
    : player_island_(pfrm, Layer::map_0_ext, 5, player()),
      current_scene_(null_scene()), next_scene_(null_scene()),
      effects_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      birbs_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      level_timer_(0), backup_(allocate_dynamic<save::EmergencyBackup>(pfrm))
{
    current_scene_ = initial_scene();
    next_scene_ = initial_scene();

    init_clouds(pfrm);

    pfrm.screen().fade(1.f);

    if (not save::load_global_data(pfrm, gp_)) {
        load_default_flag(pfrm, *this);
        for (auto& score : gp_.highscores_.values_) {
            score.set(0);
        }
    }

    pfrm.on_unrecoverrable_error([this](Platform& pfrm) {
        if (backup_->valid_) {
            backup_->store(pfrm);
        }
        pfrm.logger().flush();
    });
}


Coins App::terrain_cost()
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
    };

    return terrain_cost_table[player_island_.terrain().size() - 1];
}


void App::create_backup(Platform& pfrm)
{
    backup_->init(pfrm, *this);
}



void App::delete_backup()
{
    backup_->valid_ = false;
}



class RemoteConsoleLispPrinter : public lisp::Printer {
public:
    RemoteConsoleLispPrinter(Platform& pfrm) : pfrm_(pfrm)
    {
    }

    void put_str(const char* str) override
    {
        fmt_ += str;
    }

    Platform::RemoteConsole::Line fmt_;
    Platform& pfrm_;
};



COLD void on_remote_console_text(Platform& pfrm,
                                 const Platform::RemoteConsole::Line& str)
{
    RemoteConsoleLispPrinter printer(pfrm);

    lisp::BasicCharSequence seq(str.c_str());
    lisp::read(seq);
    lisp::eval(lisp::get_op(0));
    format(lisp::get_op(0), printer);

    lisp::pop_op();
    lisp::pop_op();


    pfrm.remote_console().printline(printer.fmt_.c_str());
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
        on_remote_console_text(pfrm, *line);
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

        time_stream_.push(pfrm, level_timer_, e);
    }
}



void App::update_parallax(Microseconds delta)
{
    cloud_scroll_1_ += 0.00002f * delta;
    cloud_scroll_2_ += 0.00004f * delta;
}



void App::render(Platform& pfrm)
{
    pfrm.system_call("_prlx7", (void*)(intptr_t)(u8)cloud_scroll_1_);
    pfrm.system_call("_prlx8", (void*)(intptr_t)(u8)cloud_scroll_2_);

    current_scene_->display(pfrm, *this);
}



void App::set_coins(Platform& pfrm, Coins coins)
{
    time_stream::event::CoinsChanged e;
    e.previous_value_.set(persistent_data_.coins_);
    time_stream_.push(pfrm, level_timer_, e);

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
    }
}



void App::invoke_ram_script(Platform& pfrm, const char* ram_fs_path)
{
    if (not is_developer_mode()) {
        return;
    }

    Vector<char> buffer(pfrm);
    if (ram_filesystem::read_file_data(pfrm, ram_fs_path, buffer)) {
        lisp::VectorCharSequence seq(buffer);
        lisp::dostring(seq, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.fmt_.c_str());
        });
    }
}



bool App::is_developer_mode()
{
    return gp_.flags0_ & GlobalPersistentData::Flags0::developer_mode;
}



void App::set_developer_mode(bool value)
{
    if (value) {
        gp_.flags0_ |= GlobalPersistentData::Flags0::developer_mode;
    } else {
        gp_.flags0_ &= ~GlobalPersistentData::Flags0::developer_mode;
    }
}



lisp::Value*
App::invoke_script(Platform& pfrm, const char* path, bool rom_fs_only)
{
    auto on_err = [&pfrm](lisp::Value& err) {
        lisp::DefaultPrinter p;
        lisp::format(&err, p);
        pfrm.fatal(p.fmt_.c_str());
    };

    if (is_developer_mode() and not pfrm.network_peer().is_connected() and
        game_mode_ not_eq GameMode::tutorial and not rom_fs_only) {

        Vector<char> buffer(pfrm);
        if (ram_filesystem::read_file_data(pfrm, path, buffer)) {
            lisp::VectorCharSequence seq(buffer);
            return lisp::dostring(seq, on_err);
        }
    }

    if (path[0] == '/') {
        ++path;
    }

    if (auto contents = pfrm.load_file_contents("", path)) {
        lisp::BasicCharSequence seq(contents);
        return lisp::dostring(seq, on_err);
    } else {
        StringBuffer<100> err("script '");
        err += path;
        err += "' missing";
        pfrm.fatal(err.c_str());
    }
}



KeyCallbackProcessor key_callback_processor;



} // namespace skyland
