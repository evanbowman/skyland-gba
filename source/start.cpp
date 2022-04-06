#include "globals.hpp"
#include "localization.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/save.hpp"
#include "skyland/skyland.hpp"
#include "transformGroup.hpp"
#include "vector.hpp"
#include "version.hpp"



// clang-format off

const char* console_header =
"\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"|  Skyland Console                                                             |\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n";

// clang-format on



static void console_display_startup_prompt(Platform& pfrm)
{
    StringBuffer<320> msg(console_header);
    auto vn = format("engine version %.%.%.%",
                     PROGRAM_MAJOR_VERSION,
                     PROGRAM_MINOR_VERSION,
                     PROGRAM_SUBMINOR_VERSION,
                     PROGRAM_VERSION_REVISION);

    msg += vn;

    pfrm.remote_console().printline(msg.c_str());

    info(pfrm, vn.c_str());
}



static inline void skyland_main_loop(Platform& pf)
{
    globals().emplace<SkylandGlobalData>();
    skyland::scene_pool::pool_ =
        &std::get<SkylandGlobalData>(globals()).scene_pool_;

    std::get<SkylandGlobalData>(globals()).entity_pools_.init(pf);
    std::get<SkylandGlobalData>(globals()).room_pools_.init();

    skyland::App app(pf);

    console_display_startup_prompt(pf);

    app.init_scripts(pf);

    pf.enable_glyph_mode(true);
    pf.load_overlay_texture("overlay");
    pf.load_background_texture(app.environment().background_texture());

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
