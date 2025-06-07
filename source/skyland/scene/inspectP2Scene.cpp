////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
#include "skyland/minimap.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/script_defs.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"



namespace skyland
{



bool InspectP2Scene::displays_minimap()
{
    return true;
}



void InspectP2Scene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    if (APP.opponent_island()) {
        island_checksums_ = island_checksums();
    }

    if (state_bit_load(StateBit::minimap_on)) {
        minimap::repaint({.show_destroyed_rooms_ = true});
        minimap::show();
    }

    far_camera();
}



void clear_room_description(Optional<Text>& room_description);



void InspectP2Scene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    if (state_bit_load(StateBit::minimap_on)) {
        if (not next.displays_minimap()) {
            minimap::hide();
        }
    }

    clear_room_description(room_description_);
}



ScenePtr update_modifier_keys();



void describe_room(Island* island,
                   const RoomCoord& cursor_loc,
                   Optional<Text>& room_description);



std::tuple<u8, u8, Island*> check_island_tapclick(const Vec2<u32>& pos);



ScenePtr player_island_onclick(Time& camera_update_timer,
                               Optional<Text>& room_description,
                               const RoomCoord& pos);



bool tapped_topleft_corner();



ScenePtr process_exit_condition(App::ExitCondition c);



ScenePtr InspectP2Scene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }


    const auto exit_cond = APP.exit_condition();
    if (exit_cond not_eq App::ExitCondition::none) {
        set_gamespeed(GameSpeed::normal);
        if (auto next = process_exit_condition(exit_cond)) {
            return next;
        }
    }


    if (not APP.opponent_island()) {
        return make_scene<ReadyScene>();
    }

    const auto last_checksums = island_checksums_;

    island_checksums_ = island_checksums();

    if (state_bit_load(StateBit::minimap_on) and
        (minimap::needs_repaint() or island_checksums_ not_eq last_checksums)) {
        minimap::repaint({.show_destroyed_rooms_ = true});
        minimap::show();
    }


    auto& cursor_loc = globals().far_cursor_loc_;


    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();


    auto sync_cursor = [&] {
        APP.player().network_sync_cursor(cursor_loc, cursor_anim_frame_, false);
    };



    if (APP.player().key_pressed(Key::start)) {

        if (APP.player().key_down(Key::start)) {
            await_start_key_ = true;
        }

        if (APP.player().key_held(Key::start, milliseconds(800))) {
            return make_scene<ModifierKeyHintScene>();
        }

        if (auto scene = update_modifier_keys()) {
            return scene;
        }

    } else {

        if (APP.player().key_down(Key::select)) {
            return make_scene<SelectMenuScene>();
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);

                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);

            } else {
                auto& near_cursor = globals().near_cursor_loc_;

                near_cursor.y = cursor_loc.y;

                APP.player().network_sync_cursor(near_cursor, 0, true);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                return make_scene<ReadyScene>();
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < APP.opponent_island()->terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                sync_cursor();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                sync_cursor();
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                sync_cursor();
            }
        }

        if (await_start_key_ and APP.player().key_up(Key::start) and
            APP.game_mode() not_eq App::GameMode::multiplayer and
            APP.game_mode() not_eq App::GameMode::co_op) {
            auto next = make_scene<StartMenuScene>(0);
            next->cascade_anim_in_ = true;
            return next;
        }
    }


    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
        sync_cursor();
    }


    if (APP.player().touch_held(milliseconds(200))) {
        if (auto pos = APP.player().touch_current()) {
            const auto view_offset =
                PLATFORM.screen().get_view().get_center().cast<s32>();
            auto island_pos = APP.opponent_island()->get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);
            island_pos.y -= Fixnum::from_integer(view_offset.y);

            if (Fixnum::from_integer(pos->x) >= island_pos.x and
                Fixnum::from_integer(pos->x) <=
                    island_pos.x +
                        Fixnum::from_integer(
                            APP.opponent_island()->terrain().size() * 16)) {

                int x_tile = -((island_pos.x.as_integer() - pos->x) / 16);
                int y_tile = -((island_pos.y.as_integer() - pos->y) / 16);

                y_tile += 31; // FIXME!

                cursor_loc = {(u8)x_tile, (u8)y_tile};
                camera_update_timer_ = milliseconds(500);
            }
        }
    }


    if (APP.game_mode() == App::GameMode::sandbox and
        (tapped_topleft_corner() or APP.player().key_down(Key::alt_2))) {
        return make_scene<ConstructionScene>(false);
    } else if (APP.player().key_down(Key::alt_2)) {

        PLATFORM.speaker().play_sound("beep_error", 2);

        cursor_loc.x = 0;

        auto& near_cursor = globals().near_cursor_loc_;

        near_cursor.y = cursor_loc.y;

        APP.player().network_sync_cursor(near_cursor, 0, true);

        return make_scene<ReadyScene>();
    }


    if (auto pos = APP.player().tap_released()) {
        auto [x, y, island] = check_island_tapclick(*pos);

        if (is_player_island(island)) {
            if (auto scene = player_island_onclick(
                    camera_update_timer_, room_description_, {x, y})) {
                return scene;
            } else {
                globals().near_cursor_loc_ = {x, y};
                return make_scene<ReadyScene>();
            }
        } else if (island == APP.opponent_island()) {
            camera_update_timer_ = milliseconds(500);
            globals().far_cursor_loc_ = {x, y};
        } else if (island == nullptr) {
            const auto view_offset =
                PLATFORM.screen().get_view().get_center().cast<s32>();
            auto island_pos = APP.opponent_island()->get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);

            if (Fixnum::from_integer(pos->x) < island_pos.x) {
                globals().near_cursor_loc_ = {0, cursor_loc.y};
                return make_scene<ReadyScene>();
            }
        }
    }

    if (APP.player().key_down(Key::action_1)) {
        if (auto room = APP.opponent_island()->get_room(cursor_loc)) {
            if (APP.game_mode() == App::GameMode::sandbox or
                room->non_owner_selectable() or
                is_player_island(room->owner())) {
                return room->select(cursor_loc);
            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        } else if (auto drone = APP.opponent_island()->get_drone(cursor_loc)) {
            if (is_player_island((*drone)->parent())) {
                return (*drone)->select();
            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        if (auto drone = APP.opponent_island()->get_drone(cursor_loc)) {
            if (is_player_island((*drone)->parent())) {
                return make_scene<SalvageDroneScene>(*drone);
            }
        } else if (APP.game_mode() == App::GameMode::sandbox) {
            if (auto room = APP.opponent_island()->get_room(cursor_loc)) {
                const auto props = (*room->metaclass())->properties();
                if (not(props & RoomProperties::salvage_disallowed)) {
                    return make_scene<SalvageRoomScene>(false);
                } else {
                    PLATFORM.speaker().play_sound("beep_error", 2);
                    auto msg = SYSTR(salvage_error_disallowed);
                    auto s = make_deferred_scene<InspectP2Scene>();
                    return make_scene<NotificationScene>(msg->c_str(), s);
                }
            } else if (not PLATFORM.network_peer().is_connected()) {
                await_b_key_ = true;
            }
        }
    }

    if (await_b_key_ and APP.player().key_up(Key::action_2)) {
        await_b_key_ = false;
        if (APP.game_mode() == App::GameMode::tutorial) {
            return make_scene<MoveRoomScene>(false);
        }
    }
    if (await_b_key_ and
        APP.player().key_held(Key::action_2, milliseconds(400))) {
        return make_scene<MoveRoomScene>(false);
    }

    if (not is_far_camera()) {
        return make_scene<ReadyScene>();
    }

    if (auto next = process_script_menu_request()) {
        return next;
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(APP.opponent_island(), cursor_loc, room_description_);
        }
    }

    return null_scene();
}



void InspectP2Scene::display()
{
    if (APP.opponent_island()) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

        auto origin = APP.opponent_island()->visual_origin();

        auto& cursor_loc = globals().far_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        cursor.set_position(origin);

        PLATFORM.screen().draw(cursor);

        if (auto room = APP.opponent_island()->get_room(cursor_loc)) {
            if (room->opponent_display_on_hover()) {
                room->display_on_hover(PLATFORM.screen(), cursor_loc);
            }
        } else if (auto drone = APP.opponent_island()->get_drone(cursor_loc)) {
            if (is_player_island((*drone)->parent())) {
                (*drone)->display_on_hover(PLATFORM.screen(), cursor_loc);
            }
        }
    }

    if (state_bit_load(StateBit::minimap_on)) {
        minimap::draw_cursor(false);
    }

    WorldScene::display();
}


} // namespace skyland
