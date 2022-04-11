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


#include "assignWeaponGroupScene.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene>
AssignWeaponGroupScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);

    switch (state_) {
    case State::select_group:
        if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }
        break;

    case State::assign_rooms:
        // if (app.player().key_down(pfrm, Key::action_2)) {
        //     state_ = State::select_group;
        //     break;
        // }

        if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }


        if (app.player().key_down(pfrm, Key::action_1)) {
            if (auto room = app.player_island().get_room(cursor_loc)) {
                if (room->co_op_locked()) {
                    pfrm.speaker().play_sound("beep_error", 2);
                    // TODO: notification
                } else if ((*room->metaclass())->category() ==
                           Room::Category::weapon) {
                    // TODO: select category first in select_group scene, then
                    // assign groups in bulk, rather than cycling through.
                    // if (room->group() == current_group_) {
                    //     room->set_group(Room::Group::none);
                    // } else {
                    //     room->set_group(current_group_);
                    // }
                    auto group = room->group();
                    if ((int)group < (int)Room::Group::three) {
                        group = (Room::Group)((int)group + 1);
                    } else {
                        group = Room::Group::none;
                    }
                    room->set_group(group);
                    app.player_island().repaint(pfrm, app);

                    network::packet::SetWeaponGroup p;
                    p.x_ = cursor_loc.x;
                    p.y_ = cursor_loc.y;
                    p.group_ = (u8)group;
                    network::transmit(pfrm, p);
                }
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < app.player_island().terrain().size()) {
                ++cursor_loc.x;
            }
        }
        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
            }
        }
        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
            }
        }
        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
            }
        }
        break;
    }

    return null_scene();
}



void AssignWeaponGroupScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    msg_.emplace(pfrm,
                 SYSTR(weapon_group_prompt)->c_str(),
                 OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});


    app.player_island().show_groups(true);
    app.player_island().repaint(pfrm, app);
}



void AssignWeaponGroupScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    if (not app.player_island().interior_visible()) {
        // app.player_island().show_groups(false);
    }
    app.player_island().repaint(pfrm, app);
}



void AssignWeaponGroupScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    auto origin = app.player_island().visual_origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    origin.x += cursor_loc.x * 16 + 3;
    origin.y += cursor_loc.y * 16;

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_priority(0);
    sprite.set_texture_index(62);
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_flip({true, false});

    pfrm.screen().draw(sprite);
}



} // namespace skyland
