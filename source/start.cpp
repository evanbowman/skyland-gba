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


#include "globals.hpp"
#include "localization.hpp"
#include "memory/malloc.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "qr.hpp"
#include "rot13.hpp"
#include "skyland/achievement.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/bootScene.hpp"
#include "skyland/skyland.hpp"



void operator delete(void*, unsigned int)
{
    Platform::instance().fatal("delete called");
}



namespace skyland
{



static inline int boot_init()
{
    systemstring_bind_file("strings.txt");

    {
        Conf conf;
        if (conf.expect<Conf::String>("profile", "beta") == "yes") {
            if (auto f =
                    PLATFORM.load_file_contents("", "/licenses/user.txt")) {
                PLATFORM.fill_overlay(112);
                enable_text_icon_glyphs(false);
                PLATFORM.load_overlay_texture("overlay");
                PLATFORM.enable_glyph_mode(true);
                StringBuffer<256> msg;
                msg = "This beta test rom was issued to ";
                StringBuffer<27> fmt;
                while (*f not_eq '\0' and *f not_eq '\n') {
                    if (fmt.full()) {
                        msg += fmt;
                        fmt.clear();
                        msg += " ";
                    }
                    fmt.push_back(rot13(*f));
                    ++f;
                }
                msg += fmt;
                msg += ". Please do not distribute!";

                TextView tv;
                tv.assign(msg.c_str(), {1, 1}, {28, 18});

                for (int i = 0; i < 200; ++i) {
                    PLATFORM.screen().clear();
                    PLATFORM.screen().display();
                }
                enable_text_icon_glyphs(true);
                PLATFORM.fill_overlay(0);
            }
        }
    }

    BootScene::init();

    BootScene::message("mount flash filesystem...");

    bool clean_boot = false;

    flash_filesystem::InitConfig c;
    c.offset_ = 8;

    auto stat = flash_filesystem::initialize(c);
    if (stat == flash_filesystem::InitStatus::initialized) {
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
                                     "\n(key-bind \"du\" 'repl)\n"
                                     "(port 1)\n";

        flash_filesystem::store_file_data("/mods/init.lisp",
                                          user_init_file,
                                          str_len(user_init_file),
                                          {.use_compression_ = true});

        clean_boot = true;
    }


    BootScene::message("initializing initializer...");

    scene_pool::pool_ = &globals().scene_pool_;

    return clean_boot;
}



static inline void main_loop()
{
    malloc_compat::Heap heap;

    const bool clean_boot = boot_init();

    BootScene::message("start application...");

    auto app = allocate_dynamic<App>("app-data", clean_boot);

    while (PLATFORM.is_running()) {
        PLATFORM.keyboard().poll();

        PLATFORM.system_call("feed-watchdog", nullptr);

        app->update(PLATFORM.delta_clock().reset());
        PLATFORM.screen().clear();
        app->render();
        PLATFORM.screen().display();
    }
}



} // namespace skyland



void start(Platform& pfrm)
{
    return skyland::main_loop();
}
