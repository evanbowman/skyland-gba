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


#include "newgameScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "script/lisp.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/workshop.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



ScenePtr<Scene>
NewgameScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    show_island_exterior(pfrm, app, &app.player_island());
    show_island_exterior(pfrm, app, app.opponent_island());

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    switch (app.persistent_data().difficulty_) {
    case PersistentData::Difficulty::beginner:
        app.invoke_script(pfrm, "/scripts/config/easy/score.lisp");
        break;

    case PersistentData::Difficulty::experienced:
        app.invoke_script(pfrm, "/scripts/config/normal/score.lisp");
        break;

    case PersistentData::Difficulty::expert:
        app.invoke_script(pfrm, "/scripts/config/hard/score.lisp");
        break;
    }

    if (save::load(pfrm, app, app.persistent_data())) {
        save::erase(pfrm);
        pfrm.system_call("sram-flash-writeback", nullptr);
    } else {
        app.set_coins(pfrm, 0);

        app.invoke_script(pfrm, "/scripts/newgame.lisp");

        if (app.persistent_data().difficulty_ ==
            PersistentData::Difficulty::beginner) {
            app.set_coins(pfrm, app.coins() + 1000);
        }

        app.current_world_location() = 0;
        app.world_graph().generate();

        app.zone() = 1;

        app.persistent_data().total_pauses_.set(0);
        app.persistent_data().total_seconds_.set(0);
        app.persistent_data().score_.set(0);
        app.persistent_data().state_flags_.set(0);
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    app.player_island().set_position({10, 374});

    return scene_pool::alloc<ZoneImageScene>();
}



} // namespace skyland
