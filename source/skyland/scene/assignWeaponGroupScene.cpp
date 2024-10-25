////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



ScenePtr AssignWeaponGroupScene::update(Time delta)
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
            return make_scene<ReadyScene>();
        }
        break;

    case State::assign_rooms:

        if (APP.player().key_down(Key::action_2)) {
            return make_scene<ReadyScene>();
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
