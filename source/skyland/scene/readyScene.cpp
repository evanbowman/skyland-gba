#include "readyScene.hpp"
#include "assignWeaponGroupScene.hpp"
#include "boxedDialogScene.hpp"
#include "constructionScene.hpp"
#include "fadeOutScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "keyComboScene.hpp"
#include "levelCompleteOptionsScene.hpp"
#include "lispReplScene.hpp"
#include "modifierKeyHintScene.hpp"
#include "platform/platform.hpp"
#include "playerIslandDestroyedScene.hpp"
#include "salvageDroneScene.hpp"
#include "salvageRoomScene.hpp"
#include "selectWeaponGroupScene.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"
#include "worldScene.hpp"



namespace skyland {



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
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
        island_pos.x -= view_offset.x;
        island_pos.y -= view_offset.y;

        if (pos.x >= island_pos.x and
            pos.x <= island_pos.x + app.player_island().terrain().size() * 16) {

            int x_tile = -((island_pos.x - pos.x) / 16);
            int y_tile = -((island_pos.y - pos.y) / 16);

            y_tile += 31; // FIXME!

            return std::make_tuple(
                (u8)x_tile, (u8)y_tile, &app.player_island());
        }
    }

    if (app.opponent_island()) {
        auto island_pos = app.opponent_island()->get_position();
        island_pos.x -= view_offset.x;
        island_pos.y -= view_offset.y;

        if (pos.x >= island_pos.x and
            pos.x <=
                island_pos.x + app.opponent_island()->terrain().size() * 16) {

            int x_tile = -((island_pos.x - pos.x) / 16);
            int y_tile = -((island_pos.y - pos.y) / 16);

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
                                      const Vec2<u8>& pos)
{
    if (auto room = app.player_island().get_room(pos)) {
        if (auto scene = room->select(pfrm, app, pos)) {
            return scene;
        } else if (auto db = dynamic_cast<DroneBay*>(room)) {
            if (auto drone = db->drone()) {
                // If a user selects a drone bay with a drone already
                // attached, jump the cursor to the drone's location.
                camera_update_timer = milliseconds(500);
                clear_room_description(pfrm, room_description);
                if ((*drone)->destination() == &app.player_island()) {
                    std::get<SkylandGlobalData>(globals()).near_cursor_loc_ =
                        (*drone)->position();
                } else {
                    std::get<SkylandGlobalData>(globals()).far_cursor_loc_ =
                        (*drone)->position();
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
        app.exit_condition() = App::ExitCondition::none;
        if (exit_cond == App::ExitCondition::misc) {
            return scene_pool::alloc<FadeOutScene>();
        } else if (exit_cond == App::ExitCondition::victory) {
            return scene_pool::alloc<PlayerIslandDestroyedScene>(
                app.opponent_island(), true);
        } else if (exit_cond == App::ExitCondition::defeat) {
            return scene_pool::alloc<PlayerIslandDestroyedScene>(
                &app.player_island(), true);
        }
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;



    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;


    auto sync_cursor = [&] {
        app.player().network_sync_cursor(
            pfrm, cursor_loc, cursor_anim_frame_, true);
    };


    if (not app.player().key_pressed(pfrm, Key::start)) {

        if (tapped_topleft_corner(pfrm, app) or
            app.player().key_down(pfrm, Key::alt_2)) {
            return scene_pool::alloc<ConstructionScene>(pfrm);
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);

                sync_cursor();
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < app.player_island().terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);

                sync_cursor();

            } else if ( // Do not allow the player to inspect the other island if we're in
                // the multiplayer waiting room.
                app.opponent_island() and
                (mt_prep_seconds == 0 or std::get<SkylandGlobalData>(globals())
                                             .unhide_multiplayer_prep_)) {
                auto& cursor_loc =
                    std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

                cursor_loc.x = 0;
                cursor_loc.y =
                    std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y;

                app.player().network_sync_cursor(pfrm, cursor_loc, 0, false);

                return scene_pool::alloc<InspectP2Scene>();
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);

                sync_cursor();
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);

                sync_cursor();
            }
        }

        if (await_start_key_ and
            app.player().key_up(pfrm, Key::start) and
            app.game_mode() not_eq App::GameMode::multiplayer and
            app.game_mode() not_eq App::GameMode::co_op) {
            return scene_pool::alloc<StartMenuScene>(pfrm, 0);
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
            island_pos.x -= view_offset.x;
            island_pos.y -= view_offset.y;

            if (pos->x >= island_pos.x and
                pos->x <=
                    island_pos.x + app.player_island().terrain().size() * 16) {

                int x_tile = -((island_pos.x - pos->x) / 16);
                int y_tile = -((island_pos.y - pos->y) / 16);

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
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_ = {x, y};
            return scene_pool::alloc<InspectP2Scene>();
        } else if (island == nullptr) {
            const auto view_offset =
                pfrm.screen().get_view().get_center().cast<s32>();
            auto island_pos = app.player_island().get_position();
            island_pos.x -= view_offset.x;

            if (pos->x >=
                island_pos.x + app.player_island().terrain().size() * 16 + 32) {
                std::get<SkylandGlobalData>(globals()).far_cursor_loc_ = {
                    0, cursor_loc.y};
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

    if (not pfrm.network_peer().is_connected() and app.launch_repl()) {
        app.launch_repl() = false;
        return scene_pool::alloc<LispReplScene>(pfrm);
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (app.player_island().get_room(cursor_loc)) {
            return scene_pool::alloc<SalvageRoomScene>();
        } else if (auto drone = app.player_island().get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                return scene_pool::alloc<SalvageDroneScene>(*drone);
            }
        }
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
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description)
{
    if (auto room = island->get_room(cursor_loc)) {
        if (not room_description) {
            room_description.emplace(
                pfrm, OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
        }
        if (room->parent() == &app.player_island() or
            room->description_visible()) {

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
                                auto str = SYSTR(character_label_human);
                                room_description->append(str->c_str(), opts);
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
                    if (auto cb = dynamic_cast<CargoBay*>(room)) {
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
                    desc += ") ";
                    room_description->assign(desc.c_str());
                    room_description->append(room->health());

                    room_description->append("/");
                    room_description->append(room->max_health());
                    room_description->append(" ");
                    room_description->append((*metac)->consumes_power());
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
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    auto origin = app.player_island().visual_origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    if (not(app.next_scene() and
            dynamic_cast<BoxedDialogScene*>(app.next_scene().get()))) {
        // Don't draw the cursor if we're going into a dialog box. If we did,
        // the cursor would flicker in and out for one frame during the scene in
        // between two dialog boxes.
        pfrm.screen().draw(cursor);
    }

    if (auto room = app.player_island().get_room(cursor_loc)) {
        room->display_on_hover(pfrm.screen(), app, cursor_loc);
    }

    WorldScene::display(pfrm, app);
}



void ReadyScene::exit(Platform& pfrm, App&, Scene& next)
{
    clear_room_description(pfrm, room_description_);
}



} // namespace skyland
