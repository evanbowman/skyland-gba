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



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const RoomCoord& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



class SpectatorScene : public ActiveWorldScene
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev)
    {
        ActiveWorldScene::enter(pfrm, app, prev);

        for (auto& room : app.player_island().rooms()) {
            // Hack to turn off targeting computer, as the AI will instead take
            // responsibility for setting weapon targets.
            if (auto t = room->cast<TargetingComputer>()) {
                if (t->enabled()) {
                    t->select(pfrm, app, {});
                }
            }
        }

        text_.emplace(pfrm, SYSTR(spectate_msg)->c_str(), OverlayCoord{0, 0});
    }


    void exit(Platform& pfrm, App& app, Scene& next)
    {
        ActiveWorldScene::exit(pfrm, app, next);
        clear_room_description(pfrm, room_description_);
        text_.reset();
    }



    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
            app.swap_player<PlayerP1>();
            return scene;
        }

        if (app.game_speed() not_eq GameSpeed::normal) {
            set_gamespeed(pfrm, app, GameSpeed::normal);
        }

        auto test_key = [&](Key k) {
            return player(app).test_key(
                pfrm, k, milliseconds(500), milliseconds(100));
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
            island = opponent_island(app);
        } else {
            island = &player_island(app);
        }


        player(app).key_held_distribute(pfrm);

        if (player(app).key_down(pfrm, Key::start)) {
            app.swap_player<PlayerP1>();
            return scene_pool::alloc<ReadyScene>();
        } else if (player(app).key_down(pfrm, Key::action_1) or
                   player(app).key_down(pfrm, Key::action_2) or
                   player(app).key_down(pfrm, Key::alt_2)) {
            pfrm.speaker().play_sound("beep_error", 2);
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
            } else if (is_far_camera()) {
                globals().near_cursor_loc_.y = globals().far_cursor_loc_.y;
                globals().near_cursor_loc_.x =
                    player_island(app).terrain().size();
                pfrm.speaker().play_sound("cursor_tick", 0);
                near_camera();
                return null_scene();
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < island->terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
            } else if (not is_far_camera()) {
                globals().far_cursor_loc_.y = globals().near_cursor_loc_.y;
                globals().far_cursor_loc_.x = 0;
                pfrm.speaker().play_sound("cursor_tick", 0);
                far_camera();
                return null_scene();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (describe_room_timer_ > 0) {
            describe_room_timer_ -= delta;
            if (describe_room_timer_ <= 0) {
                describe_room_timer_ = milliseconds(500);

                describe_room(pfrm, app, island, cursor_loc, room_description_);
            }
        }

        return null_scene();
    }


    void display(Platform& pfrm, App& app)
    {
        auto& cursor_loc = is_far_camera() ? globals().far_cursor_loc_
                                           : globals().near_cursor_loc_;

        Island* island;
        if (is_far_camera()) {
            island = opponent_island(app);
        } else {
            island = &player_island(app);
        }

        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_texture_index(15 + cursor_anim_frame_);

        auto origin = island->visual_origin();

        origin.x += cursor_loc.x * 16;
        origin.y += cursor_loc.y * 16;

        cursor.set_position(origin);

        pfrm.screen().draw(cursor);

        if (auto room = app.player_island().get_room(cursor_loc)) {
            room->display_on_hover(pfrm.screen(), app, cursor_loc);
        }

        WorldScene::display(pfrm, app);
    }


private:
    Microseconds cursor_anim_timer_;
    Microseconds describe_room_timer_ = seconds(1);
    u8 cursor_anim_frame_;
    std::optional<Text> room_description_;

    std::optional<Text> text_;
};



} // namespace skyland
