#include "globals.hpp"
#include "skyland/skyland.hpp"
#include "transformGroup.hpp"


// clang-format off

const char* console_header =
"\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"|  Skyland LISP Console                                                        |\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"Type (env) to show available symbols";

// clang-format on


void skyland_main_loop(Platform& pf)
{
    pf.remote_console().printline(::console_header);

    globals().emplace<SkylandGlobalData>();
    skyland::room_pool::pool_ =
        &std::get<SkylandGlobalData>(globals()).room_pool_;
    skyland::scene_pool::pool_ =
        &std::get<SkylandGlobalData>(globals()).scene_pool_;

    skyland::App app(pf);

    pf.enable_glyph_mode(true);
    pf.load_overlay_texture("overlay");
    pf.load_sprite_texture("spritesheet");

    while (pf.is_running()) {
        pf.keyboard().poll();

        pf.feed_watchdog();

        app.update(pf, pf.delta_clock().reset());
        pf.screen().clear();
        app.render(pf);
        pf.screen().display();
    }
}


void start(Platform& pf)
{
    return skyland_main_loop(pf);
}
