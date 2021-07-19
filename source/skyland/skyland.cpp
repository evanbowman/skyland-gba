#include "skyland.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
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
    pfrm.load_tile0_texture("tilesheet");

    auto data = pfrm.extract_tile(Layer::map_0, 105);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            flag_img_.pixels[x][y] = data.data_[x][y + 1];
        }
    }


    pfrm.read_save_data(&highscores_, sizeof highscores_, 0);
    if (highscores_.magic_[0] not_eq 'H' or highscores_.magic_[1] not_eq 'S') {
        highscores_ = Highscores{};
        for (auto& score : highscores_.values_) {
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
    info(pfrm, str.c_str());

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



} // namespace skyland
