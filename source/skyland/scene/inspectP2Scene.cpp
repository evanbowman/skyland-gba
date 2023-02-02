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


#include "inspectP2Scene.hpp"
#include "constructionScene.hpp"
#include "fadeOutScene.hpp"
#include "globals.hpp"
#include "keyComboScene.hpp"
#include "lispReplScene.hpp"
#include "modifierKeyHintScene.hpp"
#include "moveRoomScene.hpp"
#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "salvageDroneScene.hpp"
#include "salvageRoomScene.hpp"
#include "selectMenuScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"



namespace skyland
{



void InspectP2Scene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    far_camera();
}



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



void InspectP2Scene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    clear_room_description(pfrm, room_description_);
}



ScenePtr<Scene> update_modifier_keys(Platform& pfrm, App& app);



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const RoomCoord& cursor_loc,
                   std::optional<Text>& room_description);



std::tuple<u8, u8, Island*>
check_island_tapclick(Platform& pfrm, App& app, const Vec2<u32>& pos);



ScenePtr<Scene> player_island_onclick(Platform& pfrm,
                                      App& app,
                                      Microseconds& camera_update_timer,
                                      std::optional<Text>& room_description,
                                      const RoomCoord& pos);



bool tapped_topleft_corner(Platform& pfrm, App& app);



ScenePtr<Scene>
process_exit_condition(Platform& pfrm, App& app, App::ExitCondition c);



ScenePtr<Scene>
InspectP2Scene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }


    const auto exit_cond = app.exit_condition();
    if (exit_cond not_eq App::ExitCondition::none) {
        set_gamespeed(pfrm, app, GameSpeed::normal);
        if (auto next = process_exit_condition(pfrm, app, exit_cond)) {
            return next;
        }
    }


    if (not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto& cursor_loc = globals().far_cursor_loc_;


    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    auto sync_cursor = [&] {
        app.player().network_sync_cursor(
            pfrm, cursor_loc, cursor_anim_frame_, false);
    };



    if (app.player().key_pressed(pfrm, Key::start)) {

        if (app.player().key_down(pfrm, Key::start)) {
            await_start_key_ = true;
        }

        if (app.player().key_held(Key::start, milliseconds(800))) {
            return scene_pool::alloc<ModifierKeyHintScene>();
        }

        if (auto scene = update_modifier_keys(pfrm, app)) {
            return scene;
        }

    } else {

        if (app.player().key_down(pfrm, Key::select)) {
            return scene_pool::alloc<SelectMenuScene>();
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);

                sync_cursor();
                pfrm.speaker().play_sound("cursor_tick", 0);

            } else {
                auto& near_cursor = globals().near_cursor_loc_;

                near_cursor.y = cursor_loc.y;

                app.player().network_sync_cursor(pfrm, near_cursor, 0, true);
                pfrm.speaker().play_sound("cursor_tick", 0);
                return scene_pool::alloc<ReadyScene>();
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < app.opponent_island()->terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
                sync_cursor();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
                sync_cursor();
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                pfrm.speaker().play_sound("cursor_tick", 0);
                sync_cursor();
            }
        }

        if (await_start_key_ and app.player().key_up(pfrm, Key::start) and
            app.game_mode() not_eq App::GameMode::multiplayer and
            app.game_mode() not_eq App::GameMode::co_op) {
            return scene_pool::alloc<StartMenuScene>(0);
        }
    }


    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
        sync_cursor();
    }


    if (app.player().touch_held(milliseconds(200))) {
        if (auto pos = app.player().touch_current(pfrm)) {
            const auto view_offset =
                pfrm.screen().get_view().get_center().cast<s32>();
            auto island_pos = app.opponent_island()->get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);
            island_pos.y -= Fixnum::from_integer(view_offset.y);

            if (Fixnum::from_integer(pos->x) >= island_pos.x and
                Fixnum::from_integer(pos->x) <=
                    island_pos.x +
                        Fixnum::from_integer(
                            app.opponent_island()->terrain().size() * 16)) {

                int x_tile = -((island_pos.x.as_integer() - pos->x) / 16);
                int y_tile = -((island_pos.y.as_integer() - pos->y) / 16);

                y_tile += 31; // FIXME!

                cursor_loc = {(u8)x_tile, (u8)y_tile};
                camera_update_timer_ = milliseconds(500);
            }
        }
    }


    if (app.game_mode() == App::GameMode::sandbox and
        (tapped_topleft_corner(pfrm, app) or
         app.player().key_down(pfrm, Key::alt_2))) {
        return scene_pool::alloc<ConstructionScene>(false);
    } else if (app.player().key_down(pfrm, Key::alt_2)) {

        pfrm.speaker().play_sound("beep_error", 2);

        cursor_loc.x = 0;

        auto& near_cursor = globals().near_cursor_loc_;

        near_cursor.y = cursor_loc.y;

        app.player().network_sync_cursor(pfrm, near_cursor, 0, true);

        return scene_pool::alloc<ReadyScene>();
    }


    if (auto pos = app.player().tap_released(pfrm)) {
        auto [x, y, island] = check_island_tapclick(pfrm, app, *pos);

        if (island == &app.player_island()) {
            if (auto scene = player_island_onclick(pfrm,
                                                   app,
                                                   camera_update_timer_,
                                                   room_description_,
                                                   {x, y})) {
                return scene;
            } else {
                globals().near_cursor_loc_ = {x, y};
                return scene_pool::alloc<ReadyScene>();
            }
        } else if (island == app.opponent_island()) {
            camera_update_timer_ = milliseconds(500);
            globals().far_cursor_loc_ = {x, y};
        } else if (island == nullptr) {
            const auto view_offset =
                pfrm.screen().get_view().get_center().cast<s32>();
            auto island_pos = app.opponent_island()->get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);

            if (Fixnum::from_integer(pos->x) < island_pos.x) {
                globals().near_cursor_loc_ = {0, cursor_loc.y};
                return scene_pool::alloc<ReadyScene>();
            }
        }
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (auto room = app.opponent_island()->get_room(cursor_loc)) {
            if (app.game_mode() == App::GameMode::sandbox or
                room->non_owner_selectable() or
                room->owner() == &app.player_island()) {
                return room->select(pfrm, app, cursor_loc);
            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        } else if (auto drone = app.opponent_island()->get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                return (*drone)->select(pfrm, app);
            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (auto drone = app.opponent_island()->get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                return scene_pool::alloc<SalvageDroneScene>(*drone);
            }
        } else if (app.game_mode() == App::GameMode::sandbox) {
            if (auto room = app.opponent_island()->get_room(cursor_loc)) {
                const auto props = (*room->metaclass())->properties();
                if (not(props & RoomProperties::salvage_disallowed)) {
                    return scene_pool::alloc<SalvageRoomScene>(false);
                } else {
                    pfrm.speaker().play_sound("beep_error", 2);
                    auto msg = SYSTR(salvage_error_disallowed);
                    auto s = scene_pool::make_deferred_scene<InspectP2Scene>();
                    return scene_pool::alloc<NotificationScene>(msg->c_str(),
                                                                s);
                }
            } else if (not pfrm.network_peer().is_connected()) {
                await_b_key_ = true;
            }
        }
    }

    if (await_b_key_ and app.player().key_up(pfrm, Key::action_2)) {
        await_b_key_ = false;
        if (app.game_mode() == App::GameMode::tutorial) {
            return scene_pool::alloc<MoveRoomScene>(app, false);
        }
    }
    if (await_b_key_ and
        app.player().key_held(Key::action_2, milliseconds(400))) {
        return scene_pool::alloc<MoveRoomScene>(app, false);
    }

    if (not is_far_camera()) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (not pfrm.network_peer().is_connected() and
        state_bit_load(app, StateBit::launch_repl)) {
        state_bit_store(app, StateBit::launch_repl, false);
        return scene_pool::alloc<LispReplScene>();
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(pfrm,
                          app,
                          app.opponent_island(),
                          cursor_loc,
                          room_description_);
        }
    }

    return null_scene();
}



void InspectP2Scene::display(Platform& pfrm, App& app)
{
    if (app.opponent_island()) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

        auto origin = app.opponent_island()->visual_origin();

        auto& cursor_loc = globals().far_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        cursor.set_position(origin);

        pfrm.screen().draw(cursor);

        if (auto drone = app.opponent_island()->get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                (*drone)->display_on_hover(pfrm.screen(), app, cursor_loc);
            }
        }
    }

    WorldScene::display(pfrm, app);
}


} // namespace skyland
