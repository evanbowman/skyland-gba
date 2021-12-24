#include "skyland.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "platform/ram_filesystem.hpp"
#include "save.hpp"
#include "script/lisp.hpp"
#include "serial.hpp"



namespace skyland {



void init_clouds(Platform& pfrm);



App::App(Platform& pfrm)
    : player_island_(pfrm, Layer::map_0_ext, 5, player()),
      current_scene_(null_scene()), next_scene_(null_scene()),
      effects_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      birbs_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      level_timer_(0)
{
    current_scene_ = initial_scene();
    next_scene_ = initial_scene();

    rng::get(rng::critical_state);

    init_clouds(pfrm);

    player_island_.show_flag(true);

    pfrm.screen().fade(1.f);

    if (not save::load_global_data(pfrm, gp_)) {
        load_default_flag(pfrm, *this);
        for (auto& score : gp_.highscores_.values_) {
            score.set(0);
        }
    }
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

    lisp::read(str.c_str());
    lisp::eval(lisp::get_op(0));
    format(lisp::get_op(0), printer);

    lisp::pop_op();
    lisp::pop_op();


    pfrm.remote_console().printline(printer.fmt_.c_str());
}



void App::update(Platform& pfrm, Microseconds delta)
{
    if (next_scene_) {
        next_scene_->enter(pfrm, *this, *current_scene_);

        current_scene_ = std::move(next_scene_);
    }

    auto line = pfrm.remote_console().readline();
    if (UNLIKELY(static_cast<bool>(line))) {
        on_remote_console_text(pfrm, *line);
    }

    rumble_.update(pfrm, delta);

    level_timer_.count_up(delta);

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

    next_scene_ = current_scene_->update(pfrm, *this, delta);

    if (next_scene_) {
        current_scene_->exit(pfrm, *this, *next_scene_);
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
    // parallax-scrolling the the cloud layers.
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

    auto data = pfrm.make_scratch_buffer();
    if (ram_filesystem::read_file_data(pfrm, ram_fs_path, data)) {
        lisp::dostring(data->data_, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.fmt_.c_str());
        });
    }
}



void App::safe_invoke_ram_script(Platform& pfrm,
                                 const char* ram_fs_path,
                                 const char* rom_fs_fallback_path)
{
    if (not is_developer_mode()) {
        lisp::dostring(pfrm.load_file_contents("scripts", rom_fs_fallback_path),
                       [&pfrm](lisp::Value& err) {
                           lisp::DefaultPrinter p;
                           lisp::format(&err, p);
                           pfrm.fatal(p.fmt_.c_str());
                       });
        return;
    }

    ram_filesystem::import_file_from_rom_if_not_exists(
        pfrm, ram_fs_path, rom_fs_fallback_path);

    auto data = pfrm.make_scratch_buffer();
    if (ram_filesystem::read_file_data(pfrm, ram_fs_path, data)) {
        lisp::dostring(data->data_, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.fmt_.c_str());
        });
    } else {
        lisp::dostring(pfrm.load_file_contents("scripts", rom_fs_fallback_path),
                       [&pfrm](lisp::Value& err) {
                           lisp::DefaultPrinter p;
                           lisp::format(&err, p);
                           pfrm.fatal(p.fmt_.c_str());
                       });
    }
}



bool App::is_developer_mode()
{
    // return persistent_data_.flags0_ & PersistentData::Flags0::developer_mode;
    return true;
}



void App::invoke_script(Platform& pfrm, const char* path)
{
    auto on_err = [&pfrm](lisp::Value& err) {
        lisp::DefaultPrinter p;
        lisp::format(&err, p);
        pfrm.fatal(p.fmt_.c_str());
    };

    if (is_developer_mode() and not pfrm.network_peer().is_connected() and
        not tutorial_mode()) {

        auto data = pfrm.make_scratch_buffer();
        if (ram_filesystem::read_file_data(pfrm, path, data)) {
            lisp::dostring(data->data_, on_err);
            return;
        }
    }

    if (path[0] == '/') {
        ++path;
    }

    if (auto contents = pfrm.load_file_contents("", path)) {
        lisp::dostring(contents, on_err);
    } else {
        StringBuffer<100> err("script '");
        err += path;
        err += "' missing";
        pfrm.fatal(err.c_str());
    }
}



void App::conditional_invoke_script(Platform& pfrm,
                                    const char* ram_fs_path,
                                    const char* rom_fs_path,
                                    bool load_from_rom)
{
    if (load_from_rom or not is_developer_mode()) {

        // We still want to make sure that the scripts are loaded, even if
        // we are not going to currently execute them.
        ram_filesystem::import_file_from_rom_if_not_exists(
            pfrm, ram_fs_path, rom_fs_path);

        lisp::dostring(pfrm.load_file_contents("scripts", rom_fs_path),
                       [&pfrm](lisp::Value& err) {
                           lisp::DefaultPrinter p;
                           lisp::format(&err, p);
                           pfrm.fatal(p.fmt_.c_str());
                       });
    } else {
        safe_invoke_ram_script(pfrm, ram_fs_path, rom_fs_path);
    }
}



} // namespace skyland
