#include "globals.hpp"
#include "localization.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/save.hpp"
#include "skyland/skyland.hpp"
#include "transformGroup.hpp"
#include "vector.hpp"



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

    std::get<SkylandGlobalData>(globals()).entity_pools_.init(pf);

    skyland::App app(pf);

    app.init_scripts(pf);

    pf.speaker().play_music("shadows", true);

    pf.enable_glyph_mode(true);
    pf.load_overlay_texture("overlay_world_map");
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


void start(Platform& pfrm)
{
    ram_filesystem::initialize(pfrm,
                               sizeof(skyland::save::GlobalSaveData) +
                                   sizeof(skyland::save::SaveData));

    const char* test_file = ";;;\n"
                            ";;; init.lisp\n"
                            ";;;\n"
                            ";;; The game will run this\n"
                            ";;; script upon entering\n"
                            ";;; a game session.\n"
                            ";;; Create scripts in the\n"
                            ";;; mods dir, and load them\n"
                            ";;; here.\n"
                            ";;;\n"
                            "; (key-bind \"u\" repl)\n";

    if (not ram_filesystem::file_exists(pfrm, "/mods/init.lisp")) {
        ram_filesystem::store_file_data(
            pfrm, "/mods/init.lisp", test_file, str_len(test_file));
    }


    return skyland_main_loop(pfrm);
}
