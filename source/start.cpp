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



static inline void skyland_main_loop(Platform& pf)
{
    globals().emplace<SkylandGlobalData>();
    skyland::scene_pool::pool_ =
        &std::get<SkylandGlobalData>(globals()).scene_pool_;

    std::get<SkylandGlobalData>(globals()).entity_pools_.init(pf);
    std::get<SkylandGlobalData>(globals()).room_pools_.init();

    skyland::App app(pf);

    pf.remote_console().printline(::console_header);

    app.init_scripts(pf);

    pf.enable_glyph_mode(true);
    pf.load_overlay_texture("overlay");
    pf.load_background_texture("background");

    while (pf.is_running()) {
        pf.keyboard().poll();

        pf.system_call("feed-watchdog", nullptr);

        app.update(pf, pf.delta_clock().reset());
        pf.screen().clear();
        app.render(pf);
        pf.screen().display();
    }
}



void start(Platform& pfrm)
{
    auto stat =
        ram_filesystem::initialize(pfrm,
                                   sizeof(skyland::save::GlobalSaveData) +
                                       sizeof(skyland::save::SaveData));

    if (stat == ram_filesystem::InitStatus::initialized) {
        const char* user_init_file = ";;;\n"
                                     ";;; init.lisp\n"
                                     ";;;\n"
                                     ";;; The game will run this\n"
                                     ";;; script upon entering\n"
                                     ";;; a game session.\n"
                                     ";;; Create scripts in the\n"
                                     ";;; mods dir, and load them\n"
                                     ";;; here.\n"
                                     ";;;\n"
                                     "\n(key-bind \"du\" 'repl)\n";

        ram_filesystem::store_file_data(
            pfrm, "/mods/init.lisp", user_init_file, str_len(user_init_file));
    }

    return skyland_main_loop(pfrm);
}
