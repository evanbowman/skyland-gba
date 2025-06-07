////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
#include "groupSelectionScene.hpp"
#include "inspectP2Scene.hpp"
#include "keyComboScene.hpp"
#include "levelCompleteOptionsScene.hpp"
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
#include "skyland/minimap.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene/scriptHookScene.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/script_defs.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"
#include "worldScene.hpp"



namespace skyland
{



void describe_room(Island* island,
                   const RoomCoord& cursor_loc,
                   Optional<Text>& room_description);



void clear_room_description(Optional<Text>& room_description)
{
    if (not room_description) {
        return;
    }

    auto rdy = room_description->coord().y;
    const auto st = calc_screen_tiles();
    const u8 y = rdy - 1;

    for (int i = 0; i < room_description->len(); ++i) {
        auto xo = room_description->coord().x;
        if (PLATFORM.get_tile(Layer::overlay, xo + i, y) == 425) {
            PLATFORM.set_tile(Layer::overlay, xo + i, y, 0);
        }
    }

    if (auto ws = APP.scene().cast_world_scene()) {
        if (not ws->hide_chr_icon()) {
            for (int x = 0; x < 4; ++x) {
                for (int y = 0; y < 4; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, st.y - y - 1, 0);
                }
            }
        }
    }


    room_description.reset();
}



std::tuple<u8, u8, Island*> check_island_tapclick(const Vec2<u32>& pos)
{
    const auto view_offset =
        PLATFORM.screen().get_view().int_center().cast<s32>();

    {
        auto island_pos = APP.player_island().get_position();
        island_pos.x -= Fixnum::from_integer(view_offset.x);
        island_pos.y -= Fixnum::from_integer(view_offset.y);

        if (Fixnum::from_integer(pos.x) >= island_pos.x and
            Fixnum::from_integer(pos.x) <=
                island_pos.x + Fixnum::from_integer(
                                   APP.player_island().terrain().size() * 16)) {

            int x_tile = -((island_pos.x.as_integer() - pos.x) / 16);
            int y_tile = -((island_pos.y.as_integer() - pos.y) / 16);

            y_tile += 31; // FIXME!

            return std::make_tuple(
                (u8)x_tile, (u8)y_tile, &APP.player_island());
        }
    }

    if (APP.opponent_island()) {
        auto island_pos = APP.opponent_island()->get_position();
        island_pos.x -= Fixnum::from_integer(view_offset.x);
        island_pos.y -= Fixnum::from_integer(view_offset.y);

        if (Fixnum::from_integer(pos.x) >= island_pos.x and
            Fixnum::from_integer(pos.x) <=
                island_pos.x +
                    Fixnum::from_integer(
                        APP.opponent_island()->terrain().size() * 16)) {

            int x_tile = -((island_pos.x.as_integer() - pos.x) / 16);
            int y_tile = -((island_pos.y.as_integer() - pos.y) / 16);

            y_tile += 31; // FIXME!

            return std::make_tuple(
                (u8)x_tile, (u8)y_tile, APP.opponent_island());
        }
    }

    return std::make_tuple(0, 0, nullptr);
}



ScenePtr player_island_onclick(Time& camera_update_timer,
                               Optional<Text>& room_description,
                               const RoomCoord& pos)
{
    if (auto room = APP.player_island().get_room(pos)) {
        if (room->co_op_locked()) {
            PLATFORM.speaker().play_sound("beep_error", 2);
            // TODO: display notification: co-op player editing room.
            return null_scene();
        }
        if (auto scene = room->select(pos)) {
            return scene;
        } else if (auto db = room->cast<DroneBay>()) {
            if (auto drone = db->drone()) {
                // If a user selects a drone bay with a drone already
                // attached, jump the cursor to the drone's location.
                camera_update_timer = milliseconds(500);
                clear_room_description(room_description);
                if (is_player_island((*drone)->destination())) {
                    globals().near_cursor_loc_ = (*drone)->position();
                } else {
                    globals().far_cursor_loc_ = (*drone)->position();
                    return make_scene<InspectP2Scene>();
                }
            }
        } else {
            return null_scene();
        }
    } else if (auto drone = APP.player_island().get_drone(pos)) {
        if (is_player_island((*drone)->parent())) {
            return (*drone)->select();
        }
    }

    return null_scene();
}



bool tapped_topleft_corner()
{
    if (auto pos = APP.player().tap_released()) {
        if (pos->x < 36 and pos->y < 36) {
            return true;
        }
    }
    return false;
}



class SkipTutorialScene : public Scene
{
public:
    static constexpr const auto sel_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};


    void enter(Scene& prev)
    {
        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.fill_overlay(0);

        msg_.emplace(SYSTR(exit_tutorial)->c_str(), OverlayCoord{1, 1});
        yes_text_.emplace(OverlayCoord{2, 3});
        no_text_.emplace(SYSTR(no)->c_str(), OverlayCoord{2, 5});

        yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
    }


    void exit(Scene& next)
    {
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.fill_overlay(0);

        msg_.reset();
        yes_text_.reset();
        no_text_.reset();
    }


    ScenePtr update(Time delta)
    {
        if (key_down<Key::up>()) {
            selection_ = true;
            yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
            no_text_->assign(SYSTR(no)->c_str());
        }

        if (key_down<Key::down>()) {
            selection_ = false;
            yes_text_->assign(SYSTR(yes)->c_str());
            no_text_->assign(SYSTR(no)->c_str(), sel_colors);
        }

        if (key_down<Key::action_1>()) {
            if (selection_) {
                return make_scene<SelectTutorialScene>();
            } else {
                return make_scene<ReadyScene>();
            }
        }

        return null_scene();
    }


private:
    bool selection_ = true;

    Optional<Text> msg_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
};



ScenePtr update_modifier_keys()
{
    if (APP.player().key_down(Key::alt_2)) {
        return make_scene<KeyComboScene>(true);
    } else if (APP.player().key_down(Key::action_2) or
               APP.player().key_down(Key::down)) {
        return make_scene<AssignWeaponGroupScene>();
    } else if (APP.player().key_down(Key::up)) {
        for (auto& room : APP.player_island().rooms()) {
            if (room->group() == Room::Group::one) {
                if (auto scene = room->select(room->position())) {
                    return scene;
                }
            }
        }
    } else if (APP.player().key_down(Key::right)) {
        for (auto& room : APP.player_island().rooms()) {
            if (room->group() == Room::Group::two) {
                if (auto scene = room->select(room->position())) {
                    return scene;
                }
            }
        }
    } else if (APP.player().key_down(Key::left)) {
        for (auto& room : APP.player_island().rooms()) {
            if (room->group() == Room::Group::three) {
                if (auto scene = room->select(room->position())) {
                    return scene;
                }
            }
        }
    } else if (APP.player().key_down(Key::action_1)) {
        auto resume = make_deferred_scene<ReadyScene>();
        return make_scene<SelectWeaponGroupScene>(resume);
    } else if (APP.player().key_down(Key::select)) {
        if (not PLATFORM.network_peer().is_connected()) {

            APP.player().reassign_all_weapon_targets();

            PLATFORM.speaker().play_sound("drone_beep", 1);

            return make_scene<ReadyScene>();
        }
    }

    return null_scene();
}



ScenePtr process_exit_condition(App::ExitCondition c)
{
    APP.exit_condition() = App::ExitCondition::none;
    switch (c) {
    case App::ExitCondition::player_fled:
        APP.effects().clear();
        APP.player_island().projectiles().clear();
        if (APP.opponent_island()) {
            APP.opponent_island()->projectiles().clear();
        }
        PLATFORM.speaker().play_sound("bell", 1);
        return make_scene<EscapeBeaconFadeScene>(true);

    case App::ExitCondition::opponent_fled:
        APP.effects().clear();
        APP.player_island().projectiles().clear();
        if (APP.opponent_island()) {
            APP.opponent_island()->projectiles().clear();
        }
        APP.effects().clear();
        PLATFORM.speaker().play_sound("bell", 1);
        return make_scene<EscapeBeaconFadeScene>(false);

    case App::ExitCondition::misc:
        return make_scene<FadeOutScene>();

    case App::ExitCondition::victory:
        return make_scene<PlayerIslandDestroyedScene>(APP.opponent_island(),
                                                      true);

    case App::ExitCondition::defeat:
        return make_scene<PlayerIslandDestroyedScene>(&APP.player_island(),
                                                      true);

    case App::ExitCondition::none:
        break;
    }

    return null_scene();
}



bool ReadyScene::displays_minimap()
{
    return true;
}



ScenePtr ReadyScene::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    const auto last_checksums = island_checksums_;

    island_checksums_ = island_checksums();

    if (state_bit_load(StateBit::minimap_on) and
        (minimap::needs_repaint() or island_checksums_ not_eq last_checksums)) {
        minimap::repaint({.show_destroyed_rooms_ = true});
        minimap::show();
    }

    const auto exit_cond = APP.exit_condition();
    if (exit_cond not_eq App::ExitCondition::none) {
        set_gamespeed(GameSpeed::normal);
        if (auto next = process_exit_condition(exit_cond)) {
            return next;
        }
    }

    if (auto next = process_script_menu_request()) {
        return next;
    }

    if (APP.game_mode() == App::GameMode::adventure or
        APP.game_mode() == App::GameMode::skyland_forever) {
        const auto achievement = achievements::update();
        if (achievement not_eq achievements::Achievement::none) {
            achievements::award(achievement);

            auto next = make_deferred_scene<ReadyScene>();

            return make_scene<AchievementNotificationScene>(achievement, next);
        }
    }

    auto& cursor_loc = globals().near_cursor_loc_;

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();


    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;


    auto sync_cursor = [&] {
        APP.player().network_sync_cursor(cursor_loc, cursor_anim_frame_, true);
    };


    if (not APP.player().key_pressed(Key::start)) {

        if (tapped_topleft_corner() or APP.player().key_down(Key::alt_2)) {
            return make_scene<ConstructionScene>();
        }

        bool cursor_moved = false;

        if (APP.player().key_held(Key::action_1, milliseconds(64))) {
            if (test_key(Key::left)) {
                auto grp = make_scene<GroupSelectionScene>();
                grp->left_qd_ = true;
                return grp;
            } else if (test_key(Key::right)) {
                auto grp = make_scene<GroupSelectionScene>();
                grp->right_qd_ = true;
                return grp;
            } else if (test_key(Key::up)) {
                auto grp = make_scene<GroupSelectionScene>();
                grp->up_qd_ = true;
                return grp;
            } else if (test_key(Key::down)) {
                auto grp = make_scene<GroupSelectionScene>();
                grp->down_qd_ = true;
                return grp;
            }
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < APP.player_island().terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;

                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);

            } else if ( // Do not allow the player to inspect the other island if we're in
                // the multiplayer waiting room.
                APP.opponent_island() and
                (mt_prep_seconds == 0 or globals().unhide_multiplayer_prep_)) {

                if (APP.world_graph()
                        .nodes_[APP.current_world_location()]
                        .type_ == WorldGraph::Node::Type::shop) {
                    invoke_hook("on-shop-enter");
                    return null_scene();
                }

                auto& cursor_loc = globals().far_cursor_loc_;

                cursor_loc.x = 0;
                cursor_loc.y = globals().near_cursor_loc_.y;

                APP.player().network_sync_cursor(cursor_loc, 0, false);

                PLATFORM.speaker().play_sound("cursor_tick", 0);
                return make_scene<InspectP2Scene>();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(room_description_);
                describe_room_timer_ = milliseconds(300);
                cursor_moved = true;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }

        if (cursor_moved) {
            // PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        if (await_start_key_ and APP.player().key_up(Key::start) and
            APP.game_mode() not_eq App::GameMode::multiplayer and
            APP.game_mode() not_eq App::GameMode::co_op) {
            auto next = make_scene<StartMenuScene>(0);
            next->cascade_anim_in_ = true;
            return next;
        }

        if (key_down<Key::start>() and not APP.player().key_down(Key::start)) {

            // For tutorial mode: allows the player to raise the start key when
            // the tutorial system has taken control of the player object.
            return make_scene<SkipTutorialScene>();
        }

    } else /* start pressed */ {

        if (APP.player().key_down(Key::start)) {
            await_start_key_ = true;
        }

        if (APP.player().key_held(Key::start, milliseconds(800))) {
            return make_scene<ModifierKeyHintScene>();
        }

        if (auto scene = update_modifier_keys()) {
            return scene;
        }
    }

    if (APP.player().key_down(Key::select)) {
        return make_scene<SelectMenuScene>();
    }

    if (is_far_camera()) {
        return make_scene<InspectP2Scene>();
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
            auto island_pos = APP.player_island().get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);
            island_pos.y -= Fixnum::from_integer(view_offset.y);

            if (Fixnum::from_integer(pos->x) >= island_pos.x and
                Fixnum::from_integer(pos->x) <=
                    island_pos.x +
                        Fixnum::from_integer(
                            APP.player_island().terrain().size() * 16)) {

                int x_tile = -((island_pos.x.as_integer() - pos->x) / 16);
                int y_tile = -((island_pos.y.as_integer() - pos->y) / 16);

                y_tile += 31; // FIXME!

                cursor_loc = {(u8)x_tile, (u8)y_tile};
                camera_update_timer_ = milliseconds(500);
            }
        }
    }

    if (auto pos = APP.player().tap_released()) {
        auto [x, y, island] = check_island_tapclick(*pos);

        if (is_player_island(island)) {
            if (auto scene = player_island_onclick(
                    camera_update_timer_, room_description_, {x, y})) {
                return scene;
            } else {
                cursor_loc = {x, std::min(u8(15), y)};
                camera_update_timer_ = milliseconds(500);
            }
        } else if (island == APP.opponent_island()) {
            globals().far_cursor_loc_ = {x, y};
            return make_scene<InspectP2Scene>();
        } else if (island == nullptr) {
            const auto view_offset =
                PLATFORM.screen().get_view().get_center().cast<s32>();
            auto island_pos = APP.player_island().get_position();
            island_pos.x -= Fixnum::from_integer(view_offset.x);

            if (Fixnum::from_integer(pos->x) >=
                island_pos.x +
                    Fixnum::from_integer(
                        APP.player_island().terrain().size() * 16 + 32)) {
                globals().far_cursor_loc_ = {0, cursor_loc.y};
                return make_scene<InspectP2Scene>();
            }
        }
    }

    if (APP.player().key_down(Key::action_1)) {
        if (auto scene = player_island_onclick(
                camera_update_timer_, room_description_, cursor_loc)) {
            return scene;
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        if (auto room = APP.player_island().get_room(cursor_loc)) {
            const auto props = (*room->metaclass())->properties();
            if (not(props & RoomProperties::salvage_disallowed)) {

                auto next = make_deferred_scene<SalvageRoomScene>();

                if (APP.game_mode() == App::GameMode::co_op) {
                    if (auto await = room->co_op_acquire_lock(next)) {
                        return await;
                    } else {
                        PLATFORM.speaker().play_sound("beep_error", 2);
                        // TODO: notification
                    }
                } else {
                    return next();
                }

            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
                auto msg = SYSTR(salvage_error_disallowed);
                auto next = make_deferred_scene<ReadyScene>();
                return make_scene<NotificationScene>(msg->c_str(), next);
            }
        } else if (auto drone = APP.player_island().get_drone(cursor_loc)) {
            if (is_player_island((*drone)->parent())) {
                return make_scene<SalvageDroneScene>(*drone);
            }
        } else if (not PLATFORM.network_peer().is_connected()) {
            await_b_key_ = true;
        }
    }

    if (await_b_key_ and APP.player().key_up(Key::action_2)) {
        await_b_key_ = false;
        if (APP.game_mode() == App::GameMode::tutorial) {
            return make_scene<MoveRoomScene>(true);
        }
    }
    if (await_b_key_ and
        APP.player().key_held(Key::action_2, milliseconds(400))) {
        return make_scene<MoveRoomScene>(true);
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(&APP.player_island(), cursor_loc, room_description_);
        }
    }

    return null_scene();
}



void describe_room(Island* island,
                   const RoomCoord& cursor_loc,
                   Optional<Text>& room_description)
{
    if (auto room = island->get_room(cursor_loc)) {
        if (not room_description) {
            room_description.emplace(
                OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
        } else if (not room->description_changed() and
                   room->reload_time_remaining() == 0) {
            return;
        }
        room->reset_description_changed();
        if (is_player_island(room->parent()) or
            (room->description_visible() and not room->visually_cloaked())) {

            int i = 0;
            if (length(room->characters())) {
                room_description->erase();

                int chr_icon = 0;
                bool overlap = false;

                bool dup = false;
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() == cursor_loc) {
                        if (dup) {
                            overlap = true;
                            break;
                        }
                        chr_icon = chr->get_icon();
                        dup = true;
                    }
                }

                if (auto ws = APP.scene().cast_world_scene()) {
                    if (ws->hide_chr_icon()) {
                        overlap = true;
                    }
                }


                if (chr_icon and not overlap) {
                    room_description.emplace(
                        OverlayCoord{4, u8(calc_screen_tiles().y - 1)});
                }

                for (auto& chr : room->characters()) {
                    if (chr->grid_position() == cursor_loc) {
                        if (i > 0) {
                            room_description->append(",");
                        }
                        Text::OptColors opts;
                        if (chr->owner() == &APP.player()) {
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
                                    auto str = [&] {
                                        using Race = Character::Race;
                                        switch (chr->get_race()) {
                                        default:
                                        case Race::default_race:
                                            return SYSTR(character_label_human);

                                        case Race::hostile_human:
                                            return SYSTR(
                                                character_label_bad_human);

                                        case Race::goblin: {
                                            auto ret =
                                                SYSTR(character_label_goblin);
                                            ret->pop_back();
                                            ret->pop_back();
                                            // Good goblins are already a
                                            // different color, but for
                                            // colorblind people, make their
                                            // name identifier a bit
                                            // different...
                                            ret->push_back('*');
                                            ret->push_back(')');
                                            ret->push_back(' ');
                                            return ret;
                                        }

                                        case Race::dog:
                                            return SYSTR(character_label_dog);
                                        }
                                    }();

                                    room_description->append(str->c_str(),
                                                             opts);

                                    if (chr->is_superpinned()) {
                                        int xoff = room_description->len() - 1;
                                        if (chr->get_icon()) {
                                            xoff += 4;
                                        }
                                        PLATFORM.set_tile(
                                            Layer::overlay,
                                            xoff,
                                            calc_screen_tiles().y - 1,
                                            390);
                                        room_description->append(" ");
                                    }
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
                        if (chr->get_max_health() == 255) {
                            room_description->append(25);
                        } else {
                            room_description->append(chr->get_max_health() /
                                                     10);
                        }
                        // Replicants with icons cause appended stats to be cropped offscreen
                        bool skip = chr->is_replicant() and chr_icon;
                        if (skip or
                            (not overlap and chr->owner() == &APP.player())) {
                            const int xo = chr_icon ? 4 : 0;
                            const int y = calc_screen_tiles().y - 1;
                            auto icon_x = [&] {
                                return xo + room_description->len() - 1;
                            };
                            auto b = chr->stats().info_.battles_fought_;
                            room_description->append("  ");
                            PLATFORM.set_tile(Layer::overlay, icon_x(), y, 484);
                            room_description->append(b);
                        }
                        ++i;
                    }
                }

                if (chr_icon and not overlap) {
                    int offset = (chr_icon - 1) * 16;
                    PLATFORM.load_overlay_chunk(
                        274, offset, 16, "character_art");
                    const auto st = calc_screen_tiles();

                    int tile = 274;
                    for (int y = 0; y < 4; ++y) {
                        for (int x = 0; x < 4; ++x) {
                            PLATFORM.set_tile(
                                Layer::overlay, x, st.y - 4 + y, tile++, 10);
                        }
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
                    desc += (*metac)->ui_name()->c_str();
                    room->append_name_suffix(desc);
                    desc += ") ";
                    room_description->assign(desc.c_str());
                    room_description->append(room->health());

                    room_description->append("/");
                    room_description->append(room->max_health());
                    room_description->append(" ");
                    room_description->append(room->power_usage());
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
                    const auto st = calc_screen_tiles();
                    if (room_description->len() + 1 == st.x) {
                        // If we're running out of room, try to squeeze the
                        // group icon into the last open tile slot.
                        room_description->append(" ");
                    } else {
                        room_description->append("  ");
                    }
                    PLATFORM.set_tile(Layer::overlay,
                                      room_description->len() - 1,
                                      calc_screen_tiles().y - 1,
                                      393 + ((int)room->group() - 1));
                }

                if (room->visually_cloaked()) {
                    const FontColors c{custom_color(0x66fff7),
                                       ColorConstant::rich_black};
                    room_description->append(" ?", c);
                    PLATFORM.set_tile(Layer::overlay,
                                      room_description->len() - 1,
                                      calc_screen_tiles().y - 1,
                                      156);
                }
            }

        } else {
            room_description.emplace(
                OverlayCoord{0, u8(calc_screen_tiles().y - 1)});

            room_description->assign("(??"); // Split to avoid trigraph
            room_description->append("?) ??");
            room_description->append("?/???");
        }
    } else {
        if (auto drone = island->get_drone(cursor_loc)) {
            room_description.emplace(
                OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
            Text::OptColors opts = {
                {custom_color(0x3e9bf7), ColorConstant::rich_black}};
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
            for (auto& bird : APP.birds()) {
                if (bird->island() == island and
                    bird->coordinate() == cursor_loc) {

                    if (not room_description) {
                        room_description.emplace(

                            OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
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
                    OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
            }

            auto flag_name = [&]() -> StringBuffer<48> {
                if (island->custom_flag_graphics()) {
                    switch (island->custom_flag_graphics()) {
                    case 1: // marauder
                        return SYS_CSTR(flag_alt3);
                    case 2: // old empire
                        return SYS_CSTR(flag_alt2);
                    case 3: // goblin horde
                        return SYS_CSTR(flag_alt4);
                    case 4: // second empire
                        return SYS_CSTR(flag_alt1);
                    case 5: // banana
                        return SYS_CSTR(flag_banana);
                    case 6: // merchant
                        return SYS_CSTR(flag_alt6);
                    case 7: // colonial
                        return SYS_CSTR(flag_default);
                    }
                    return "";
                } else if (island == APP.opponent_island()) {
                    return SYS_CSTR(flag_alt4);
                } else {
                    return "";
                }
            }();

            if (flag_name.length()) {
                room_description->assign(
                    format(SYS_CSTR(flag_fmt), flag_name.c_str()).c_str());
            } else {
                room_description->assign(SYS_CSTR(flag));
            }
        }
    }

    if (room_description) {
        const u8 y = calc_screen_tiles().y - 2;

        for (int i = 0; i < calc_screen_tiles().x; ++i) {
            if (PLATFORM.get_tile(Layer::overlay, i, y) == 425) {
                PLATFORM.set_tile(Layer::overlay, i, y, 0);
            }
        }

        for (int i = 0; i < room_description->len(); ++i) {
            auto xo = room_description->coord().x;
            if (not PLATFORM.get_tile(Layer::overlay, xo + i, y)) {
                PLATFORM.set_tile(Layer::overlay, xo + i, y, 425);
            }
        }
    }
}



void ReadyScene::display()
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

    auto origin = APP.player_island().visual_origin();

    auto& cursor_loc = globals().near_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    if (not(APP.next_scene() and
            APP.next_scene()->cast_boxed_dialog_scene_ws())) {
        // Don't draw the cursor if we're going into a dialog box. If we did,
        // the cursor would flicker in and out for one frame during the scene in
        // between two dialog boxes.
        PLATFORM.screen().draw(cursor);
    }

    if (auto room = APP.player_island().get_room(cursor_loc)) {
        room->display_on_hover(PLATFORM.screen(), cursor_loc);
    } else if (auto drone = APP.player_island().get_drone(cursor_loc)) {
        if (is_player_island((*drone)->parent())) {
            (*drone)->display_on_hover(PLATFORM.screen(), cursor_loc);
        }
    }

    if (APP.world_graph().nodes_[APP.current_world_location()].type_ ==
        WorldGraph::Node::Type::shop) {
        Sprite spr;
        spr.set_texture_index(57);
        auto o = APP.player_island().origin();
        o.x += Fixnum::from_integer(APP.player_island().terrain().size() * 16);
        o.y += 16.0_fixed * 12.0_fixed;
        spr.set_position(o);
        if (cursor_loc.x < APP.player_island().terrain().size()) {
            spr.set_alpha(Sprite::Alpha::translucent);
        }
        PLATFORM.screen().draw(spr);
    }

    if (state_bit_load(StateBit::minimap_on)) {
        minimap::draw_cursor(true);
    }

    WorldScene::display();
}



void ReadyScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    island_checksums_ = island_checksums();

    if (state_bit_load(StateBit::minimap_on)) {
        minimap::repaint({.show_destroyed_rooms_ = true});
        minimap::show();
    }
}



void ReadyScene::exit(Scene& next)
{
    clear_room_description(room_description_);

    if (not next.displays_minimap()) {
        minimap::hide();
    }

    if (not next.cast_world_scene()) {
        ActiveWorldScene::exit(next);
    }
}



} // namespace skyland
