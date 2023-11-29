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
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



ScenePtr<Scene> AssignWeaponGroupScene::update(Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    auto& cursor_loc = globals().near_cursor_loc_;

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();

    switch (state_) {
    case State::select_group:
        if (APP.player().key_down(Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }
        break;

    case State::assign_rooms:

        if (APP.player().key_down(Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }


        if (APP.player().key_down(Key::action_1)) {
            if (auto room = APP.player_island().get_room(cursor_loc)) {
                if (room->co_op_locked()) {
                    PLATFORM.speaker().play_sound("beep_error", 2);
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

                    time_stream::event::WeaponSetGroup e;
                    e.room_x_ = cursor_loc.x;
                    e.room_y_ = cursor_loc.y;
                    e.prev_group_ = (u8)group;
                    APP.time_stream().push(APP.level_timer(), e);

                    if ((int)group < (int)Room::Group::three) {
                        group = (Room::Group)((int)group + 1);
                    } else {
                        group = Room::Group::none;
                    }
                    room->set_group(group);
                    APP.player_island().repaint();

                    network::packet::SetWeaponGroup p;
                    p.x_ = cursor_loc.x;
                    p.y_ = cursor_loc.y;
                    p.group_ = (u8)group;
                    network::transmit(p);
                }
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < APP.player_island().terrain().size()) {
                ++cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }
        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }
        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }
        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }
        break;
    }

    return null_scene();
}



void AssignWeaponGroupScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    msg_.emplace(SYSTR(weapon_group_prompt)->c_str(),
                 OverlayCoord{0, u8(calc_screen_tiles().y - 1)});


    APP.player_island().repaint();
}



void AssignWeaponGroupScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    if (not APP.player_island().interior_visible()) {
        // APP.player_island().show_groups(false);
    }
    APP.player_island().repaint();
}



void AssignWeaponGroupScene::display()
{
    WorldScene::display();

    auto origin = APP.player_island().visual_origin();

    auto& cursor_loc = globals().near_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16 + 3);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_priority(0);
    sprite.set_texture_index(62);
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_flip({true, false});

    PLATFORM.screen().draw(sprite);
}



} // namespace skyland
