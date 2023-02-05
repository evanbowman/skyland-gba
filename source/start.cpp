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



static inline void main_loop(Platform& pf)
{
    systemstring_bind_file("strings.txt");

    {
        Conf conf(pf);
        if (conf.expect<Conf::String>("profile", "beta") == "yes") {
            if (auto f = pf.load_file_contents("", "/licenses/user.txt")) {
                pf.fill_overlay(112);
                enable_text_icon_glyphs(false);
                pf.load_overlay_texture("overlay");
                pf.enable_glyph_mode(true);
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

                TextView tv(pf);
                tv.assign(msg.c_str(), {1, 1}, {28, 18});

                for (int i = 0; i < 200; ++i) {
                    pf.screen().clear();
                    pf.screen().display();
                }
                enable_text_icon_glyphs(true);
                pf.fill_overlay(0);
            }
        }
    }

    BootScene::init(pf);


    BootScene::message(pf, "mount flash filesystem...");

    bool clean_boot = false;

    auto stat = flash_filesystem::initialize(pf, 8);
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

        flash_filesystem::store_file_data(
            pf, "/mods/init.lisp", user_init_file, str_len(user_init_file));

        clean_boot = true;
    }


    BootScene::message(pf, "init memory pools...");

    scene_pool::pool_ = &globals().scene_pool_;

    BootScene::message(pf, "start application...");

    auto app = allocate_dynamic<App>("app-data", pf, clean_boot);

    while (pf.is_running()) {
        pf.keyboard().poll();

        pf.system_call("feed-watchdog", nullptr);

        app->update(pf, pf.delta_clock().reset());
        pf.screen().clear();
        app->render(pf);
        pf.screen().display();
    }
}



} // namespace skyland



void start(Platform& pfrm)
{
    return skyland::main_loop(pfrm);
}
