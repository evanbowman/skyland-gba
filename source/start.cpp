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
#include "platform/ram_filesystem.hpp"
#include "qr.hpp"
#include "skyland/achievement.hpp"
#include "skyland/save.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/bootScene.hpp"



namespace skyland
{



static inline void main_loop(Platform& pf)
{
    BootScene::init(pf);

    BootScene::message(pf, "init memory pools...");

    globals().emplace<SkylandGlobalData>();
    scene_pool::pool_ =
        &std::get<SkylandGlobalData>(globals()).scene_pool_;

    std::get<SkylandGlobalData>(globals()).entity_pools_.init(pf);
    std::get<SkylandGlobalData>(globals()).room_pools_.init();

    BootScene::message(pf, "start application...");

    auto app = allocate_dynamic<App>("app-data", pf);

    while (pf.is_running()) {
        pf.keyboard().poll();

        pf.system_call("feed-watchdog", nullptr);

        app->update(pf, pf.delta_clock().reset());
        pf.screen().clear();
        app->render(pf);
        pf.screen().display();
    }
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
                                     "\n(key-bind \"du\" 'repl)\n"
                                     "(port 1)\n";

        ram_filesystem::store_file_data(
            pfrm, "/mods/init.lisp", user_init_file, str_len(user_init_file));
    }

    return skyland::main_loop(pfrm);
}
