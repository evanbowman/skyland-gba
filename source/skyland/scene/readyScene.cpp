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


#include "readyScene.hpp"
#include "achievementNotificationScene.hpp"
#include "assignWeaponGroupScene.hpp"
#include "boxedDialogScene.hpp"
#include "constructionScene.hpp"
#include "escapeBeaconFadeScene.hpp"
#include "fadeOutScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "itemShopScene.hpp"
#include "keyComboScene.hpp"
#include "levelCompleteOptionsScene.hpp"
#include "lispReplScene.hpp"
#include "modifierKeyHintScene.hpp"
#include "moveRoomScene.hpp"
#include "notificationScene.hpp"
#include "platform/platform.hpp"
#include "playerIslandDestroyedScene.hpp"
#include "salvageDroneScene.hpp"
#include "salvageRoomScene.hpp"
#include "selectMenuScene.hpp"
#include "selectTutorialScene.hpp"
#include "selectWeaponGroupScene.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"
#include "worldScene.hpp"



namespace skyland
{



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const RoomCoord& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description)
{
    if (not room_description) {
        return;
    }

    const u8 y = calc_screen_tiles(pfrm).y - 2;

    for (int i = 0; i < room_description->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, y, 0);
    }
    room_description.reset();
}



std::tuple<u8, u8, Island*>
check_island_tapclick(Platform& pfrm, App& app, const Vec2<u32>& pos)
{
    const auto view_offset = pfrm.screen().get_view().get_center().cast<s32>();

    {
        auto island_pos = app.player_island().get_position();
        island_pos.x -= Fixnum::from_integer(view_offset.x);
        island_pos.y -= Fixnum::from_integer(view_offset.y);

        if (Fixnum::from_integer(pos.x) >= island_pos.x and
            Fixnum::from_integer(pos.x) <=
                island_pos.x + Fixnum::from_integer(
                                   app.player_island().terrain().size() * 16)) {

            int x_tile = -((island_pos.x.as_integer() - pos.x) / 16);
            int y_tile = -((island_pos.y.as_integer() - pos.y) / 16);

            y_tile += 31; // FIXME!

            return std::make_tuple(
                (u8)x_tile, (u8)y_tile, &app.player_island());
        }
    }

    if (app.opponent_island()) {
        auto island_pos = app.opponent_island()->get_position();
        island_pos.x -= Fixnum::from_integer(view_offset.x);
        island_pos.y -= Fixnum::from_integer(view_offset.y);

        if (Fixnum::from_integer(pos.x) >= island_pos.x and
            Fixnum::from_integer(pos.x) <=
                island_pos.x +
                    Fixnum::from_integer(
                        app.opponent_island()->terrain().size() * 16)) {

            int x_tile = -((island_pos.x.as_integer() - pos.x) / 16);
            int y_tile = -((island_pos.y.as_integer() - pos.y) / 16);

            y_tile += 31; // FIXME!

            return std::make_tuple(
                (u8)x_tile, (u8)y_tile, app.opponent_island());
        }
    }

    return std::make_tuple(0, 0, nullptr);
}



ScenePtr<Scene> player_island_onclick(Platform& pfrm,
                                      App& app,
                                      Microseconds& camera_update_timer,
                                      std::optional<Text>& room_description,
                                      const RoomCoord& pos)
{
    if (auto room = app.player_island().get_room(pos)) {
        if (room->co_op_locked()) {
            pfrm.speaker().play_sound("beep_error", 2);
            // TODO: display notification: co-op player editing room.
            return null_scene();
        }
        if (auto scene = room->select(pfrm, app, pos)) {
            return scene;
        } else if (auto db = room->cast<DroneBay>()) {
            if (auto drone = db->drone()) {
                // If a user selects a drone bay with a drone already
                // attached, jump the cursor to the drone's location.
                camera_update_timer = milliseconds(500);
                clear_room_description(pfrm, room_description);
                if ((*drone)->destination() == &app.player_island()) {
                    globals().near_cursor_loc_ = (*drone)->position();
                } else {
                    globals().far_cursor_loc_ = (*drone)->position();
                    return scene_pool::alloc<InspectP2Scene>();
                }
            }
        } else {
            return null_scene();
        }
    } else if (auto drone = app.player_island().get_drone(pos)) {
        if ((*drone)->parent() == &app.player_island()) {
            return (*drone)->select(pfrm, app);
        }
    }

    return null_scene();
}



bool tapped_topleft_corner(Platform& pfrm, App& app)
{
    if (auto pos = app.player().tap_released(pfrm)) {
        if (pos->x < 36 and pos->y < 36) {
            return true;
        }
    }
    return false;
}



class AutoassignCharactersScene : public ActiveWorldScene
{
public:
    Buffer<CharacterId, 40> local_chrs_;
    Buffer<CharacterId, 40> boarded_chrs_;


    void enter(Platform& pfrm, App& app, Scene& prev)
    {
        ActiveWorldScene::enter(pfrm, app, prev);

        pfrm.speaker().play_sound("drone_beep", 1);

        if (not app.opponent_island()) {
            return;
        }

        for (auto& room : app.player_island().rooms()) {
            for (auto& chr : room->characters()) {
                if (chr->owner() == &app.player() and not chr->co_op_locked()) {
                    local_chrs_.push_back(chr->id());
                }
            }
        }

        for (auto& room : app.opponent_island()->rooms()) {
            for (auto& chr : room->characters()) {
                if (chr->owner() == &app.player() and not chr->co_op_locked()) {
                    boarded_chrs_.push_back(chr->id());
                }
            }
        }
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
            return scene;
        }

        if (not app.opponent_island()) {
            return scene_pool::alloc<ReadyScene>();
        }

        if (not local_chrs_.empty()) {
            auto current = local_chrs_.back();
            local_chrs_.pop_back();

            auto info = app.player_island().find_character_by_id(current);
            if (info.first) {
                EnemyAI::assign_local_character(pfrm,
                                                app,
                                                *info.first,
                                                &app.player(),
                                                &app.player_island(),
                                                app.opponent_island());
            }
        } else if (not boarded_chrs_.empty()) {
            auto current = boarded_chrs_.back();
            boarded_chrs_.pop_back();

            auto info = app.opponent_island()->find_character_by_id(current);
            if (info.first) {
                EnemyAI::assign_boarded_character(pfrm,
                                                  app,
                                                  *info.first,
                                                  &app.player(),
                                                  &app.player_island(),
                                                  app.opponent_island());
            }
        } else {
            return scene_pool::alloc<ReadyScene>();
        }

        return null_scene();
    }
};



class SkipTutorialScene : public Scene
{
public:
    static constexpr const auto sel_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};


    void enter(Platform& pfrm, App&, Scene& prev)
    {
        pfrm.screen().schedule_fade(1.f);
        pfrm.fill_overlay(0);

        msg_.emplace(pfrm, SYSTR(exit_tutorial)->c_str(), OverlayCoord{1, 1});
        yes_text_.emplace(pfrm, OverlayCoord{2, 3});
        no_text_.emplace(pfrm, SYSTR(no)->c_str(), OverlayCoord{2, 5});

        yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
    }


    void exit(Platform& pfrm, App&, Scene& next)
    {
        pfrm.screen().schedule_fade(0);
        pfrm.fill_overlay(0);

        msg_.reset();
        yes_text_.reset();
        no_text_.reset();
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta)
    {
        if (key_down<Key::up>(pfrm)) {
            selection_ = true;
            yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
            no_text_->assign(SYSTR(no)->c_str());
        }

        if (key_down<Key::down>(pfrm)) {
            selection_ = false;
            yes_text_->assign(SYSTR(yes)->c_str());
            no_text_->assign(SYSTR(no)->c_str(), sel_colors);
        }

        if (key_down<Key::action_1>(pfrm)) {
            if (selection_) {
                return scene_pool::alloc<SelectTutorialScene>();
            } else {
                return scene_pool::alloc<ReadyScene>();
            }
        }

        return null_scene();
    }


private:
    bool selection_ = true;

    std::optional<Text> msg_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
};



ScenePtr<Scene> update_modifier_keys(Platform& pfrm, App& app)
{
    if (app.player().key_down(pfrm, Key::alt_2)) {
        return scene_pool::alloc<KeyComboScene>(true);
    } else if (app.player().key_down(pfrm, Key::action_2) or
               app.player().key_down(pfrm, Key::down)) {
        return scene_pool::alloc<AssignWeaponGroupScene>();
    } else if (app.player().key_down(pfrm, Key::up)) {
        for (auto& room : app.player_island().rooms()) {
            if (room->group() == Room::Group::one) {
                if (auto scene = room->select(pfrm, app, room->position())) {
                    return scene;
                }
            }
        }
    } else if (app.player().key_down(pfrm, Key::right)) {
        for (auto& room : app.player_island().rooms()) {
            if (room->group() == Room::Group::two) {
                if (auto scene = room->select(pfrm, app, room->position())) {
                    return scene;
                }
            }
        }
    } else if (app.player().key_down(pfrm, Key::left)) {
        for (auto& room : app.player_island().rooms()) {
            if (room->group() == Room::Group::three) {
                if (auto scene = room->select(pfrm, app, room->position())) {
                    return scene;
                }
            }
        }
    } else if (app.player().key_down(pfrm, Key::action_1)) {
        auto resume = scene_pool::make_deferred_scene<ReadyScene>();
        return scene_pool::alloc<SelectWeaponGroupScene>(resume);
    } else if (app.player().key_down(pfrm, Key::select)) {
        if (not pfrm.network_peer().is_connected()) {
            return scene_pool::alloc<AutoassignCharactersScene>();
        }
    }

    return null_scene();
}



ScenePtr<Scene>
process_exit_condition(Platform& pfrm, App& app, App::ExitCondition c)
{
    app.exit_condition() = App::ExitCondition::none;
    switch (c) {
    case App::ExitCondition::player_fled:
        app.effects().clear();
        app.player_island().projectiles().clear();
        if (app.opponent_island()) {
            app.opponent_island()->projectiles().clear();
        }
        pfrm.speaker().play_sound("bell", 1);
        return scene_pool::alloc<EscapeBeaconFadeScene>(true);

    case App::ExitCondition::opponent_fled:
        app.effects().clear();
        app.player_island().projectiles().clear();
        if (app.opponent_island()) {
            app.opponent_island()->projectiles().clear();
        }
        app.effects().clear();
        pfrm.speaker().play_sound("bell", 1);
        return scene_pool::alloc<EscapeBeaconFadeScene>(false);

    case App::ExitCondition::misc:
        return scene_pool::alloc<FadeOutScene>();

    case App::ExitCondition::victory:
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            app.opponent_island(), true);

    case App::ExitCondition::defeat:
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            &app.player_island(), true);

    case App::ExitCondition::none:
        break;
    }

    return null_scene();
}



ScenePtr<Scene> ReadyScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    const auto exit_cond = app.exit_condition();
    if (exit_cond not_eq App::ExitCondition::none) {
        set_gamespeed(pfrm, app, GameSpeed::normal);
        if (auto next = process_exit_condition(pfrm, app, exit_cond)) {
            return next;
        }
    }

    if (state_bit_load(app, StateBit::open_item_shop)) {
        state_bit_store(app, StateBit::open_item_shop, false);
        return scene_pool::alloc<ItemShopScene>();
    }

    if (app.game_mode() == App::GameMode::adventure or
        app.game_mode() == App::GameMode::skyland_forever) {
        const auto achievement = achievements::update(pfrm, app);
        if (achievement not_eq achievements::Achievement::none) {
            achievements::award(pfrm, app, achievement);

            auto next = scene_pool::make_deferred_scene<ReadyScene>();

            return scene_pool::alloc<AchievementNotificationScene>(achievement,
                                                                   next);
        }
    }

    auto& cursor_loc = globals().near_cursor_loc_;



    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;


    auto sync_cursor = [&] {
        app.player().network_sync_cursor(
            pfrm, cursor_loc, cursor_anim_frame_, true);
    };


    if (not app.player().key_pressed(pfrm, Key::start)) {

        if (tapped_topleft_corner(pfrm, app) or
            app.player().key_down(pfrm, Key::alt_2)) {
            return scene_pool::alloc<ConstructionScene>();
        }

        bool cursor_moved = false;

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;
                sync_cursor();
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < app.player_island().terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;

                sync_cursor();
                pfrm.speaker().play_sound("cursor_tick", 0);

            } else if ( // Do not allow the player to inspect the other island if we're in
                // the multiplayer waiting room.
                app.opponent_island() and
                (mt_prep_seconds == 0 or globals().unhide_multiplayer_prep_)) {

                if (app.world_graph()
                        .nodes_[app.current_world_location()]
                        .type_ == WorldGraph::Node::Type::shop) {
                    return scene_pool::alloc<ItemShopScene>();
                }

                auto& cursor_loc = globals().far_cursor_loc_;

                cursor_loc.x = 0;
                cursor_loc.y = globals().near_cursor_loc_.y;

                app.player().network_sync_cursor(pfrm, cursor_loc, 0, false);

                pfrm.speaker().play_sound("cursor_tick", 0);
                return scene_pool::alloc<InspectP2Scene>();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;
                sync_cursor();
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;
                sync_cursor();
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (cursor_moved) {
            // pfrm.speaker().play_sound("cursor_tick", 0);
        }

        if (await_start_key_ and app.player().key_up(pfrm, Key::start) and
            app.game_mode() not_eq App::GameMode::multiplayer and
            app.game_mode() not_eq App::GameMode::co_op) {
            auto next = scene_pool::alloc<StartMenuScene>(0);
            next->cascade_anim_in_ = true;
            return next;
        }

        if (key_down<Key::start>(pfrm) and
            not app.player().key_down(pfrm, Key::start)) {

            // For tutorial mode: allows the player to raise the start key when
            // the tutorial system has taken control of the player object.
            return scene_pool::alloc<SkipTutorialScene>();
        }

    } else /* start pressed */ {

        if (app.player().key_down(pfrm, Key::start)) {
            await_start_key_ = true;
        }

        if (app.player().key_held(Key::start, milliseconds(800))) {
            return scene_pool::alloc<ModifierKeyHintScene>();
        }

        if (auto scene = update_modifier_keys(pfrm, app)) {
            return scene;
        }
    }

    if (app.player().key_down(pfrm, Key::select)) {
        return scene_pool::alloc<SelectMenuScene>();
    }

    if (is_far_camera()) {
        return scene_pool::alloc<InspectP2Scene>();
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
            auto island_pos = app.player_island().get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);
            island_pos.y -= Fixnum::from_integer(view_offset.y);

            if (Fixnum::from_integer(pos->x) >= island_pos.x and
                Fixnum::from_integer(pos->x) <=
                    island_pos.x +
                        Fixnum::from_integer(
                            app.player_island().terrain().size() * 16)) {

                int x_tile = -((island_pos.x.as_integer() - pos->x) / 16);
                int y_tile = -((island_pos.y.as_integer() - pos->y) / 16);

                y_tile += 31; // FIXME!

                cursor_loc = {(u8)x_tile, (u8)y_tile};
                camera_update_timer_ = milliseconds(500);
            }
        }
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
                cursor_loc = {x, std::min(u8(15), y)};
                camera_update_timer_ = milliseconds(500);
            }
        } else if (island == app.opponent_island()) {
            globals().far_cursor_loc_ = {x, y};
            return scene_pool::alloc<InspectP2Scene>();
        } else if (island == nullptr) {
            const auto view_offset =
                pfrm.screen().get_view().get_center().cast<s32>();
            auto island_pos = app.player_island().get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);

            if (Fixnum::from_integer(pos->x) >=
                island_pos.x +
                    Fixnum::from_integer(
                        app.player_island().terrain().size() * 16 + 32)) {
                globals().far_cursor_loc_ = {0, cursor_loc.y};
                return scene_pool::alloc<InspectP2Scene>();
            }
        }
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (auto scene = player_island_onclick(pfrm,
                                               app,
                                               camera_update_timer_,
                                               room_description_,
                                               cursor_loc)) {
            return scene;
        }
    }

    if (not pfrm.network_peer().is_connected() and
        state_bit_load(app, StateBit::launch_repl)) {
        state_bit_store(app, StateBit::launch_repl, false);
        return scene_pool::alloc<LispReplScene>();
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (auto room = app.player_island().get_room(cursor_loc)) {
            const auto props = (*room->metaclass())->properties();
            if (not(props & RoomProperties::salvage_disallowed)) {

                auto next = scene_pool::make_deferred_scene<SalvageRoomScene>();

                if (app.game_mode() == App::GameMode::co_op) {
                    if (auto await = room->co_op_acquire_lock(pfrm, next)) {
                        return await;
                    } else {
                        pfrm.speaker().play_sound("beep_error", 2);
                        // TODO: notification
                    }
                } else {
                    return next();
                }

            } else {
                pfrm.speaker().play_sound("beep_error", 2);
                auto msg = SYSTR(salvage_error_disallowed);
                auto next = scene_pool::make_deferred_scene<ReadyScene>();
                return scene_pool::alloc<NotificationScene>(msg->c_str(), next);
            }
        } else if (auto drone = app.player_island().get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                return scene_pool::alloc<SalvageDroneScene>(*drone);
            }
        } else if (not pfrm.network_peer().is_connected()) {
            await_b_key_ = true;
        }
    }

    if (await_b_key_ and app.player().key_up(pfrm, Key::action_2)) {
        await_b_key_ = false;
        if (app.game_mode() == App::GameMode::tutorial) {
            return scene_pool::alloc<MoveRoomScene>(app, true);
        }
    }
    if (await_b_key_ and
        app.player().key_held(Key::action_2, milliseconds(400))) {
        return scene_pool::alloc<MoveRoomScene>(app, true);
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(
                pfrm, app, &app.player_island(), cursor_loc, room_description_);
        }
    }

    return null_scene();
}



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const RoomCoord& cursor_loc,
                   std::optional<Text>& room_description)
{
    if (auto room = island->get_room(cursor_loc)) {
        if (not room_description) {
            room_description.emplace(
                pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
        } else if (not room->description_changed() and
                   room->reload_time_remaining() == 0) {
            return;
        }
        room->reset_description_changed();
        if (room->parent() == &app.player_island() or
            (room->description_visible() and not room->visually_cloaked())) {

            int i = 0;
            if (length(room->characters())) {
                room_description->erase();
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() == cursor_loc) {
                        if (i > 0) {
                            room_description->append(",");
                        }
                        Text::OptColors opts;
                        if (chr->owner() == &app.player()) {
                            opts = {custom_color(0xff6675),
                                    ColorConstant::rich_black};
                            if (chr->is_replicant()) {
                                auto str = SYSTR(character_label_replicant);
                                room_description->append(str->c_str(), opts);
                            } else {
                                if (auto n = chr->name()) {
                                    room_description->append("(", opts);
                                    room_description->append(n, opts);
                                    room_description->append(") ", opts);
                                } else {
                                    auto str = SYSTR(character_label_human);
                                    room_description->append(str->c_str(),
                                                             opts);
                                }
                            }
                        } else {
                            opts = {custom_color(0xcf54ff),
                                    ColorConstant::rich_black};
                            auto str = SYSTR(character_label_goblin);
                            room_description->append(str->c_str(), opts);
                        }
                        room_description->append(chr->health() / 10);
                        room_description->append("/");
                        room_description->append(25);
                        ++i;
                    }
                }
            }

            if (i == 0) {
                auto metac = room->metaclass();

                bool skip = false;

                if (str_eq((*metac)->name(), "cargo-bay")) {
                    if (auto cb = room->cast<CargoBay>()) {
                        if (cb->position().y == cursor_loc.y - 1) {
                            room_description->assign(SYSTR(cargo)->c_str());
                            if (*cb->cargo() not_eq '\0') {
                                room_description->append(cb->cargo());
                            } else {
                                room_description->append(SYSTR(none)->c_str());
                            }
                            skip = true;
                        }
                    }
                }

                if (not skip) {
                    StringBuffer<32> desc;
                    desc += "(";
                    desc += (*metac)->ui_name(pfrm)->c_str();
                    room->append_name_suffix(pfrm, desc);
                    desc += ") ";
                    room_description->assign(desc.c_str());
                    room_description->append(room->health());

                    room_description->append("/");
                    room_description->append(room->max_health());
                    room_description->append(" ");
                    room_description->append(room->power_usage(app));
                    room_description->append("`");
                }


                if (auto tm = room->reload_time_remaining()) {
                    if (tm > 0) {
                        StringBuffer<2> temp(" ");
                        temp.push_back(
                            (char)17); // using ascii DC1 for clock img
                        room_description->append(temp.c_str());
                        room_description->append(1 + tm / seconds(1));
                    }
                }

                if (room->group() not_eq Room::Group::none) {
                    const auto st = calc_screen_tiles(pfrm);
                    if (room_description->len() + 1 == st.x) {
                        // If we're running out of room, try to squeeze the
                        // group icon into the last open tile slot.
                        room_description->append(" ");
                    } else {
                        room_description->append("  ");
                    }
                    pfrm.set_tile(Layer::overlay,
                                  room_description->len() - 1,
                                  calc_screen_tiles(pfrm).y - 1,
                                  393 + ((int)room->group() - 1));
                }

                if (room->visually_cloaked()) {
                    const FontColors c{custom_color(0x66fff7),
                                       ColorConstant::rich_black};
                    room_description->append(" ?", c);
                    pfrm.set_tile(Layer::overlay,
                                  room_description->len() - 1,
                                  calc_screen_tiles(pfrm).y - 1,
                                  156);
                }
            }

        } else {
            room_description.emplace(
                pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});

            room_description->assign("(??"); // Split to avoid trigraph
            room_description->append("?) ??");
            room_description->append("?/???");
        }
    } else {
        if (auto drone = island->get_drone(cursor_loc)) {
            room_description.emplace(
                pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
            Text::OptColors opts = {
                {custom_color(0x3d84e7), ColorConstant::rich_black}};
            room_description->append("(", opts);
            room_description->append((*drone)->name(), opts);
            room_description->append(") ", opts);
            room_description->append((*drone)->health());

            if (auto tm = (*drone)->reload_time_remaining()) {
                if (tm > 0) {
                    StringBuffer<2> temp(" ");
                    temp.push_back((char)17); // using ascii DC1 for clock img
                    room_description->append(temp.c_str());
                    room_description->append(1 + tm / seconds(1));
                }
            }
        } else {
            for (auto& bird : app.birds()) {
                if (bird->island(app) == island and
                    bird->coordinate() == cursor_loc) {

                    if (not room_description) {
                        room_description.emplace(
                            pfrm,
                            OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
                    }

                    Text::OptColors opts = {
                        {custom_color(0xbdef84), ColorConstant::rich_black}};

                    auto str = SYSTR(bird_label);
                    room_description->assign(str->c_str(), opts);
                    break;
                }
            }
        }
        if (island->flag_pos() == cursor_loc) {

            if (not room_description) {
                room_description.emplace(
                    pfrm,
                    OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
            }

            auto flag_name = [&]() -> StringBuffer<48> {
                if (island->custom_flag_graphics()) {
                switch (island->custom_flag_graphics()) {
                case 1: // marauder
                    return SYS_CSTR(flag_alt3);
                case 2: // old empire
                    return SYS_CSTR(flag_alt1);
                case 3: // goblin horde
                    return SYS_CSTR(flag_alt4);
                case 4: // second empire
                    return SYS_CSTR(flag_alt2);
                case 5: // banana
                    return SYS_CSTR(flag_banana);
                case 6: // merchant
                    return SYS_CSTR(flag_alt6);
                case 7: // colonial
                    return SYS_CSTR(flag_default);
                }
                return "";
                } else if (island == app.opponent_island()) {
                    return SYS_CSTR(flag_alt4);
                } else {
                    return "";
                }
            }();

            if (flag_name.length()) {
                room_description->assign(format(SYS_CSTR(flag_fmt),
                                                flag_name.c_str()).c_str());
            } else {
                room_description->assign(SYS_CSTR(flag));
            }

        }
    }

    if (room_description) {
        const u8 y = calc_screen_tiles(pfrm).y - 2;

        for (int i = 0; i < calc_screen_tiles(pfrm).x; ++i) {
            pfrm.set_tile(Layer::overlay, i, y, 0);
        }

        for (int i = 0; i < room_description->len(); ++i) {
            pfrm.set_tile(Layer::overlay, i, y, 425);
        }
    }
}



void ReadyScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

    auto origin = app.player_island().visual_origin();

    auto& cursor_loc = globals().near_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    if (not(app.next_scene() and
            app.next_scene()->cast_boxed_dialog_scene_ws())) {
        // Don't draw the cursor if we're going into a dialog box. If we did,
        // the cursor would flicker in and out for one frame during the scene in
        // between two dialog boxes.
        pfrm.screen().draw(cursor);
    }

    if (auto room = app.player_island().get_room(cursor_loc)) {
        room->display_on_hover(pfrm.screen(), app, cursor_loc);
    } else if (auto drone = app.player_island().get_drone(cursor_loc)) {
        if ((*drone)->parent() == &app.player_island()) {
            (*drone)->display_on_hover(pfrm.screen(), app, cursor_loc);
        }
    }

    if (app.world_graph().nodes_[app.current_world_location()].type_ ==
        WorldGraph::Node::Type::shop) {
        Sprite spr;
        spr.set_texture_index(57);
        auto o = app.player_island().origin();
        o.x += Fixnum::from_integer(app.player_island().terrain().size() * 16);
        o.y += 16.0_fixed * 12.0_fixed;
        spr.set_position(o);
        if (cursor_loc.x < app.player_island().terrain().size()) {
            spr.set_alpha(Sprite::Alpha::translucent);
        }
        pfrm.screen().draw(spr);
    }

    WorldScene::display(pfrm, app);
}



void ReadyScene::exit(Platform& pfrm, App&, Scene& next)
{
    clear_room_description(pfrm, room_description_);
}



} // namespace skyland
