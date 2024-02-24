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

#pragma once

#include "constructionScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/rooms/targetingComputer.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



void describe_room(Island* island,
                   const RoomCoord& cursor_loc,
                   Optional<Text>& room_description);



void clear_room_description(Optional<Text>& room_description);



class SpectatorScene : public ActiveWorldScene
{
public:
    void enter(Scene& prev)
    {
        ActiveWorldScene::enter(prev);

        for (auto& room : APP.player_island().rooms()) {
            // Hack to turn off targeting computer, as the AI will instead take
            // responsibility for setting weapon targets.
            if (auto t = room->cast<TargetingComputer>()) {
                if (t->enabled()) {
                    t->select({});
                }
            }
        }

        text_.emplace(SYSTR(spectate_msg)->c_str(), OverlayCoord{0, 0});
    }


    void exit(Scene& next)
    {
        ActiveWorldScene::exit(next);
        clear_room_description(room_description_);
        text_.reset();
    }



    ScenePtr<Scene> update(Time delta) override
    {
        if (auto scene = ActiveWorldScene::update(delta)) {
            APP.swap_player<PlayerP1>();
            return scene;
        }

        if (APP.game_speed() not_eq GameSpeed::normal) {
            set_gamespeed(GameSpeed::normal);
        }

        auto test_key = [&](Key k) {
            return player().test_key(k, milliseconds(500), milliseconds(100));
        };


        auto& cursor_loc = is_far_camera() ? globals().far_cursor_loc_
                                           : globals().near_cursor_loc_;


        cursor_anim_timer_ += delta;
        if (cursor_anim_timer_ > milliseconds(200)) {
            cursor_anim_timer_ -= milliseconds(200);
            cursor_anim_frame_ = not cursor_anim_frame_;
        }

        Island* island;
        if (is_far_camera()) {
            island = opponent_island();
        } else {
            island = &player_island();
        }


        player().key_held_distribute();

        if (player().key_down(Key::start)) {
            APP.swap_player<PlayerP1>();
            return scene_pool::alloc<ReadyScene>();
        } else if (player().key_down(Key::action_1) or
                   player().key_down(Key::action_2) or
                   player().key_down(Key::alt_2)) {
            PLATFORM.speaker().play_sound("beep_error", 2);
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            } else if (is_far_camera()) {
                globals().near_cursor_loc_.y = globals().far_cursor_loc_.y;
                globals().near_cursor_loc_.x = player_island().terrain().size();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                near_camera();
                return null_scene();
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < island->terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            } else if (not is_far_camera()) {
                globals().far_cursor_loc_.y = globals().near_cursor_loc_.y;
                globals().far_cursor_loc_.x = 0;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                far_camera();
                return null_scene();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (describe_room_timer_ > 0) {
            describe_room_timer_ -= delta;
            if (describe_room_timer_ <= 0) {
                describe_room_timer_ = milliseconds(500);

                describe_room(island, cursor_loc, room_description_);
            }
        }

        return null_scene();
    }


    void display()
    {
        auto& cursor_loc = is_far_camera() ? globals().far_cursor_loc_
                                           : globals().near_cursor_loc_;

        Island* island;
        if (is_far_camera()) {
            island = opponent_island();
        } else {
            island = &player_island();
        }

        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

        auto origin = island->visual_origin();

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        cursor.set_position(origin);

        PLATFORM.screen().draw(cursor);

        if (auto room = APP.player_island().get_room(cursor_loc)) {
            room->display_on_hover(PLATFORM.screen(), cursor_loc);
        }

        WorldScene::display();
    }


private:
    Time cursor_anim_timer_;
    Time describe_room_timer_ = seconds(1);
    u8 cursor_anim_frame_;
    Optional<Text> room_description_;

    Optional<Text> text_;
};



} // namespace skyland
