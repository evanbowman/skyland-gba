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


#include "weaponSetTargetScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const RoomCoord& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



std::tuple<u8, u8, Island*>
check_island_tapclick(Platform& pfrm, App& app, const Vec2<u32>& pos);



WeaponSetTargetScene::WeaponSetTargetScene(const RoomCoord& weapon_loc,
                                           bool near,
                                           std::optional<RoomCoord> initial_pos)
    : weapon_loc_(weapon_loc), near_(near), initial_pos_(initial_pos)
{
}



static const int minimap_start_tile = 181;
static const int minimap_isle_spacing = 3;
static bool minimap_disabled = false;


u8 minimap_width(App& app)
{
    int pixel_width =
        (3 * (1 + app.player_island().terrain().size() + minimap_isle_spacing +
              (app.opponent_island() ? app.opponent_island()->terrain().size()
                                     : 0) +
              1));
    return pixel_width / 8 + (pixel_width % 8 > 0);
}



ScenePtr<Scene>
WeaponSetTargetScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    auto player_weapon_exit_scene = [&]() -> ScenePtr<Scene> {
        if (resume_far_) {
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    };

    auto drone_exit_scene = [&](Drone* drone) -> ScenePtr<Scene> {
        if (drone->destination() == &app.player_island()) {
            globals().near_cursor_loc_ = drone->position();
            return scene_pool::alloc<ReadyScene>();
        } else {
            globals().far_cursor_loc_ = drone->position();
            return scene_pool::alloc<InspectP2Scene>();
        }
    };

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (not app.opponent_island() or mt_prep_seconds not_eq 0) {
        return player_weapon_exit_scene();
    }

    if (app.player_island().checksum() not_eq last_player_checksum_ or
        app.opponent_island()->checksum() not_eq last_opponent_checksum_) {

        minimap_repaint_timer_ = milliseconds(100);

        last_player_checksum_ = app.player_island().checksum();
        last_opponent_checksum_ = app.opponent_island()->checksum();
    }

    auto& cursor_loc = globals().far_cursor_loc_;


    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    if (app.player().key_down(pfrm, Key::alt_2) and
        group_ not_eq Room::Group::none and
        not pfrm.network_peer().is_connected()) {
        firing_mode_ = (firing_mode_ + 1) % 3;
    }


    if (minimap_repaint_timer_ > 0) {
        minimap_repaint_timer_ -= delta;
        if (minimap_repaint_timer_ < 0) {
            minimap_repaint_timer_ = 0;
            minimap_repaint(pfrm, app);
            cursor_tics_ = 0;
        }
    }

    if (cursor_tics_ > 4) {
        minimap_hide(pfrm, app);
    }


    if (app.player().key_down(pfrm, Key::select)) {
        minimap_disabled = not minimap_disabled;
        pfrm.speaker().play_sound("click_wooden", 2);
        if (minimap_disabled) {
            minimap_hide(pfrm, app);
        } else {
            minimap_show(pfrm, app);
        }
    }


    if (test_key(Key::right)) {
        if (cursor_loc.x < app.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            ++cursor_tics_;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            ++cursor_tics_;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::up)) {
        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            ++cursor_tics_;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            ++cursor_tics_;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }

    auto onclick = [&](RoomCoord cursor_loc) -> ScenePtr<Scene> {
        if (app.opponent_island()->get_room(cursor_loc)) {

            auto do_set_target = [&pfrm, &app, cursor_loc](Room& room) {
                room.set_target(pfrm, app, cursor_loc);
                network::packet::WeaponSetTarget packet;
                packet.weapon_x_ = room.position().x;
                packet.weapon_y_ = room.position().y;
                packet.target_x_ = cursor_loc.x;
                packet.target_y_ = cursor_loc.y;
                packet.weapon_near_ = true;
                network::transmit(pfrm, packet);
            };


            auto room = app.player_island().get_room(weapon_loc_);

            if (group_ not_eq Room::Group::none) {

                auto with_group = [&](auto& callback) {
                    for (auto& r : app.player_island().rooms()) {
                        if (r->group() == group_) {
                            callback(*r);
                        }
                    }
                };

                // If the room has a group assigned, then assign a target
                // for all rooms of the same group.
                with_group(do_set_target);

                switch (firing_mode_) {
                case 1: { // barrage
                    int count = 0;
                    int interval_sum = 0;
                    int max_reload = 0;

                    auto collect = [&](Room& r) {
                        ++count;
                        interval_sum += r.reload_interval();
                        auto rem = r.reload_time_remaining();
                        if (rem > max_reload) {
                            max_reload = rem;
                        }
                    };

                    with_group(collect);

                    if (count == 0) {
                        // Note: just in case of division by zero.
                        count = 1;
                    }

                    const int average_reload = interval_sum / count;
                    const int balance = average_reload / count;
                    count = 0;

                    auto update_timers = [&](Room& r) {
                        r.override_reload_timer(max_reload + balance * count);
                        ++count;
                    };

                    with_group(update_timers);

                    break;
                }

                case 2: { // salvo
                    Microseconds max_reload = 0;

                    auto cb = [&max_reload](Room& r) {
                        auto rem = r.reload_time_remaining();
                        if (rem > max_reload) {
                            max_reload = rem;
                        }
                    };

                    with_group(cb);

                    auto update_timers = [max_reload](Room& r) {
                        r.override_reload_timer(max_reload);
                    };

                    with_group(update_timers);

                    break;
                }
                }

                return player_weapon_exit_scene();

            } else if (near_ and room) {

                do_set_target(*room);

                if (near_) {
                    return player_weapon_exit_scene();
                } else {
                    return scene_pool::alloc<InspectP2Scene>();
                }
            } else {

                auto sync = [&](Drone& drone) {
                    network::packet::DroneSetTarget packet;
                    packet.drone_x_ = drone.position().x;
                    packet.drone_y_ = drone.position().y;
                    packet.target_x_ = cursor_loc.x;
                    packet.target_y_ = cursor_loc.y;
                    packet.drone_near_ =
                        drone.destination() == &app.player_island();
                    packet.target_near_ = false;
                    network::transmit(pfrm, packet);
                };

                if (near_) {
                    if (auto drone =
                            app.player_island().get_drone(weapon_loc_)) {
                        (*drone)->set_target(pfrm, app, cursor_loc);
                        sync(**drone);

                        return drone_exit_scene(drone->get());
                    }
                } else {
                    if (auto drone =
                            app.opponent_island()->get_drone(weapon_loc_)) {
                        (*drone)->set_target(pfrm, app, cursor_loc);
                        sync(**drone);

                        return drone_exit_scene(drone->get());
                    }
                }
            }
        }
        return null_scene();
    };

    if (test_key(Key::start)) {
        snap(pfrm, app);
        camera_update_timer_ = milliseconds(500);
        minimap_repaint_timer_ = milliseconds(100);
    }
    if (test_key(Key::action_1)) {
        if (auto scene = onclick(cursor_loc)) {
            return scene;
        }
    }
    if (auto pos = app.player().tap_released(pfrm)) {
        auto [x, y, island] = check_island_tapclick(pfrm, app, *pos);
        if (island == app.opponent_island()) {
            if (auto scene = onclick({x, y})) {
                return scene;
            } else {
                return scene_pool::alloc<ReadyScene>();
            }
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    }


    // auto origin = app.opponent_island()->visual_origin();

    // origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    // origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    // auto abs_cursor_y = origin.y.as_integer() - pfrm.screen().get_view().int_center().y;

    // if (abs_cursor_x > 8 * (27 - minimap_width(app) - 3)) {
    //     minimap_show(pfrm, app, 1);
    // } else if (abs_cursor_x < 8 * (27 - minimap_width(app) + 1)) {


    if (app.player().key_down(pfrm, Key::action_2)) {
        if (near_) {
            if (auto drone = app.player_island().get_drone(weapon_loc_)) {
                return drone_exit_scene(drone->get());
            }
        } else {
            if (auto drone = app.opponent_island()->get_drone(weapon_loc_)) {
                return drone_exit_scene(drone->get());
            }
        }
        return player_weapon_exit_scene();
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            if (app.opponent_island()) {
                describe_room(pfrm,
                              app,
                              app.opponent_island(),
                              cursor_loc,
                              room_description_);
            }
        }
    }

    return null_scene();
}


void WeaponSetTargetScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (not app.opponent_island()) {
        return;
    }

    auto origin = app.opponent_island()->visual_origin();

    auto& cursor_loc = globals().far_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_texture_index((17 * 2));
    sprite.set_size(Sprite::Size::w16_h16);

    pfrm.screen().draw(sprite);

    if (firing_mode_) {
        sprite.set_size(Sprite::Size::w16_h32);
        sprite.set_texture_index(111 + firing_mode_);
        origin.x += 12.0_fixed;
        origin.y += 10.0_fixed;
        sprite.set_position(origin);
        pfrm.screen().draw(sprite);
    }
}



void WeaponSetTargetScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    clear_room_description(pfrm, room_description_);

    if (app.game_mode() == App::GameMode::co_op) {

        if (auto room = app.player_island().get_room(weapon_loc_)) {
            room->co_op_release_lock(pfrm);
        }
    }

    pfrm.fill_overlay(0);
}



void WeaponSetTargetScene::minimap_show(Platform& pfrm, App& app)
{
    static const u8 anchor = 29 - minimap_width(app);

    if (minimap_x_anchor_ == anchor and minimap_visible_) {
        return;
    }

    minimap_hide(pfrm, app);

    if (minimap_disabled) {
        return;
    }

    const u8 width = minimap_width(app);

    u16 tile = minimap_start_tile;
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < width; ++x) {
            pfrm.set_tile(Layer::overlay, anchor + x, 14 + y, tile++);
        }
    }

    minimap_visible_ = true;
    minimap_x_anchor_ = anchor;
}



void WeaponSetTargetScene::minimap_hide(Platform& pfrm, App& app)
{
    if (not minimap_visible_) {
        return;
    }

    const u8 width = minimap_width(app);

    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < width; ++x) {
            pfrm.set_tile(Layer::overlay, minimap_x_anchor_ + x, 14 + y, 0);
        }
    }

    minimap_visible_ = false;
}



void WeaponSetTargetScene::minimap_init(Platform& pfrm, App& app)
{
    minimap_repaint(pfrm, app);
}



static Platform::EncodedTile encode_small_tile(u8 tile_data[16][16])
{
    Platform::EncodedTile t;
    u8* out = t.bytes_;

    u8 temp = 0;
    for (int i = 0; i < 8; ++i) {
        for (int j = 0; j < 8; ++j) {
            if (j % 2) {
                temp |= tile_data[j][i] << 4;
                *(out++) = temp;
            } else {
                temp = tile_data[j][i] & 0xff;
            }
        }
    }

    return t;
}



void WeaponSetTargetScene::minimap_repaint(Platform& pfrm, App& app)
{
    if (not app.opponent_island()) {
        return;
    }

    [[maybe_unused]] auto before = pfrm.delta_clock().sample();


    const u8 width = minimap_width(app);

    static const int minimap_px_width = 104;
    static const int minimap_px_height = 40;
    using MinimapPixels = u8[minimap_px_width][minimap_px_height];

    // auto pixel_buffer = allocate_dynamic<MinimapPixels>("m-px-buffer");
    MinimapPixels pixel_buffer;

    auto cursor_loc = globals().far_cursor_loc_;

    auto save_pixels = [&]() {
        if (fb_cache_.pixels_.size() == minimap_px_width * minimap_px_height) {
            auto it = fb_cache_.pixels_.begin();
            for (int x = 0; x < minimap_px_width; ++x) {
                for (int y = 0; y < minimap_px_height; ++y) {
                    *(it++) = pixel_buffer[x][y];
                }
            }
        } else {
            for (int x = 0; x < minimap_px_width; ++x) {
                for (int y = 0; y < minimap_px_height; ++y) {
                    fb_cache_.pixels_.push_back(pixel_buffer[x][y]);
                }
            }
        }
        fb_cache_.player_island_checksum_ = app.player_island().checksum();
        fb_cache_.opponent_island_checksum_ = app.opponent_island()->checksum();
    };

    auto restore_pixels = [&]() {
        if (fb_cache_.pixels_.size() < minimap_px_width * minimap_px_height) {
            Platform::fatal("logic err");
        }
        auto it = fb_cache_.pixels_.begin();
        for (int x = 0; x < minimap_px_width; ++x) {
            for (int y = 0; y < minimap_px_height; ++y) {
                pixel_buffer[x][y] = *(it++);
            }
        }
    };

    static const u8 color_black_index = 3;
    static const u8 color_tan_index = 8;
    static const u8 color_white_index = 4;
    static const u8 color_el_blue_index = 7;
    static const u8 color_gray_index = 10;
    static const u8 color_darkgray_index = 5;
    static const u8 color_burnt_orange_index = 14;
    static const u8 color_green_index = 11;

    int opp_offset =
        1 + app.player_island().terrain().size() + minimap_isle_spacing;

    Buffer<Room*, 32> weapons;

    if (fb_cache_.player_island_checksum_ == app.player_island().checksum() and
        fb_cache_.opponent_island_checksum_ ==
            app.opponent_island()->checksum()) {

        for (u8 y = 4; y < 15; ++y) {
            for (u8 x = 0; x < 13; ++x) {
                if (auto room = app.player_island().get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                            Room::Category::weapon and
                        (weapon_loc_ == Vec2<u8>{x, y} or
                         (room->group() not_eq Room::Group::none and
                          room->group() == group_))) {
                        bool found = false;
                        for (auto& wpn : weapons) {
                            if (wpn == room) {
                                found = true;
                                break;
                            }
                        }
                        if (not found) {
                            weapons.push_back(room);
                        }
                    }
                }
            }
        }

        restore_pixels();

    } else {
        memset(pixel_buffer, color_black_index, sizeof pixel_buffer);

        for (u32 x = 0; x < app.player_island().terrain().size(); ++x) {
            for (int xx = 0; xx < 3; ++xx) {
                for (int yy = 0; yy < 3; ++yy) {
                    pixel_buffer[(x + 1) * 3 + xx][((15 - 3) * 3 + yy) - 2] =
                        (yy == 0) ? color_green_index : color_darkgray_index;
                }
            }
        }

        for (u32 x = 0; x < app.opponent_island()->terrain().size(); ++x) {
            for (int xx = 0; xx < 3; ++xx) {
                for (int yy = 0; yy < 3; ++yy) {
                    pixel_buffer[(x + opp_offset) * 3 + xx -
                                 2][((15 - 3) * 3 + yy) - 2] =
                        (yy == 0) ? color_green_index : color_darkgray_index;
                }
            }
        }

        for (u8 y = 4; y < 15; ++y) {
            for (u8 x = 0; x < 13; ++x) {
                if (auto room = app.player_island().get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                            Room::Category::weapon and
                        (weapon_loc_ == Vec2<u8>{x, y} or
                         (room->group() not_eq Room::Group::none and
                          room->group() == group_))) {
                        bool found = false;
                        for (auto& wpn : weapons) {
                            if (wpn == room) {
                                found = true;
                                break;
                            }
                        }
                        if (not found) {
                            weapons.push_back(room);
                        }
                    }
                    for (int xx = 0; xx < 3; ++xx) {
                        for (int yy = 0; yy < 3; ++yy) {
                            u8 clr;
                            switch ((*room->metaclass())->category()) {
                            case Room::Category::wall:
                                if ((*room->metaclass())->properties() &
                                    RoomProperties::accepts_ion_damage) {
                                    clr = color_el_blue_index;
                                } else {
                                    clr = color_gray_index;
                                }
                                break;

                            case Room::Category::weapon:
                                clr = color_burnt_orange_index;
                                break;

                            default:
                                clr = color_tan_index;
                                break;
                            }

                            if ((weapon_loc_ == Vec2<u8>{x, y} or
                                 (room->group() not_eq Room::Group::none and
                                  room->group() == group_)) and
                                not(xx == 1 and yy == 1)) {
                                clr = color_white_index;
                            }

                            pixel_buffer[(x + 1) * 3 + xx]
                                        [((y - 3) * 3 + yy) - 2] = clr;
                        }
                    }
                }
            }
        }

        for (u8 y = 4; y < 15; ++y) {
            for (u8 x = 0; x < 13; ++x) {
                if (auto room = app.opponent_island()->get_room({x, y})) {
                    for (int xx = 0; xx < 3; ++xx) {
                        for (int yy = 0; yy < 3; ++yy) {
                            u8 clr;
                            switch ((*room->metaclass())->category()) {
                            case Room::Category::wall:
                                if ((*room->metaclass())->properties() &
                                    RoomProperties::accepts_ion_damage) {
                                    clr = color_el_blue_index;
                                } else {
                                    clr = color_gray_index;
                                }
                                break;

                            case Room::Category::weapon:
                                clr = color_burnt_orange_index;
                                break;

                            default:
                                clr = color_tan_index;
                                break;
                            }

                            pixel_buffer[(x + opp_offset) * 3 + xx - 2]
                                        [((y - 3) * 3 + yy) - 2] = clr;
                        }
                    }
                }
            }
        }

        save_pixels();
    }


    pixel_buffer[(cursor_loc.x + opp_offset) * 3 - 2]
                [((cursor_loc.y - 3) * 3) - 2] = color_white_index;
    pixel_buffer[(cursor_loc.x + opp_offset) * 3 + 1 - 2]
                [((cursor_loc.y - 3) * 3) - 2 + 1] = color_white_index;
    pixel_buffer[(cursor_loc.x + opp_offset) * 3 + 2 - 2]
                [((cursor_loc.y - 3) * 3) - 2 + 2] = color_white_index;
    pixel_buffer[(cursor_loc.x + opp_offset) * 3 + 2 - 2]
                [((cursor_loc.y - 3) * 3) - 2] = color_white_index;
    pixel_buffer[(cursor_loc.x + opp_offset) * 3 - 2]
                [((cursor_loc.y - 3) * 3) - 2 + 2] = color_white_index;

    const u8 cursor_center_px_x = (cursor_loc.x + opp_offset) * 3 + 1 - 2;
    const u8 cursor_center_px_y = ((cursor_loc.y - 3) * 3) - 2 + 1;

    for (auto wpn : weapons) {
        auto emit_pos = wpn->position();
        emit_pos.x += wpn->size().x;

        auto plot = [&](int x, int y) {
            if (pixel_buffer[x][y] == color_black_index or
                pixel_buffer[x][y] == color_white_index) {
                pixel_buffer[x][y] = color_white_index;
                return true;
            } else {
                if (pixel_buffer[x][y] == color_gray_index) {
                    pixel_buffer[x][y] = 13;
                } else {
                    pixel_buffer[x][y] = 1;
                }

                return false;
            }
        };

        auto plot_line = [&](int x0, int y0, int x1, int y1) {
            int dx = abs(x1 - x0);
            int sx = x0 < x1 ? 1 : -1;
            int dy = -abs(y1 - y0);
            int sy = y0 < y1 ? 1 : -1;
            int error = dx + dy;

            while (true) {
                plot(x0, y0);
                if (x0 == x1 && y0 == y1)
                    break;
                int e2 = 2 * error;
                if (e2 >= dy) {
                    if (x0 == x1)
                        break;
                    error = error + dy;
                    x0 = x0 + sx;
                }
                if (e2 <= dx) {
                    if (y0 == y1)
                        break;
                    error = error + dx;
                    y0 = y0 + sy;
                }
            }
        };

        int wpn_emit_px_x = (emit_pos.x + 1) * 3;
        int wpn_emit_px_y = ((emit_pos.y - 3) * 3 + 1) - 2;

        if ((*wpn->metaclass())->weapon_orientation() ==
            Room::WeaponOrientation::horizontal) {
            plot_line(wpn_emit_px_x,
                      wpn_emit_px_y,
                      cursor_center_px_x - 1,
                      cursor_center_px_y);
        } else if ((*wpn->metaclass())->weapon_orientation() ==
                   Room::WeaponOrientation::vertical) {
            wpn_emit_px_y -= 2;
            wpn_emit_px_x -= 2;
            for (int y = wpn_emit_px_y; y > 0; --y) {
                plot(wpn_emit_px_x, y);
            }
            for (int y = 1; y < cursor_center_px_y; ++y) {
                plot(cursor_center_px_x, y);
            }
        }
    }

    u16 tile = minimap_start_tile;
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < width; ++x) {
            Platform::TilePixels td;
            for (int xx = 0; xx < 8; ++xx) {
                for (int yy = 0; yy < 8; ++yy) {
                    td.data_[xx][yy] = pixel_buffer[x * 8 + xx][y * 8 + yy];
                }
            }
            pfrm.overwrite_overlay_tile(tile, encode_small_tile(td.data_));
            tile++;
        }
    }

    minimap_show(pfrm, app);

    [[maybe_unused]] auto after = pfrm.delta_clock().sample();
    // Platform::fatal(format("%", after - before));

    if (not pfrm.network_peer().is_connected()) {
        // FIXME: repaint function has large overhead. Optimize and remove clock
        // reset.
        pfrm.delta_clock().reset();
    }
}



void WeaponSetTargetScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (auto w = prev.cast_world_scene()) {
        // Yeah I know, this doesn't look pretty. If we came from a scene where
        // our camera was anchored on the far island, remember to return to the
        // far island. Originally, the WeaponSetTargetScene was only created
        // when selecting a weapon on the player's island. But then I added key
        // combos for assigning a weapon target while the camera was anchored on
        // the opponent's island, so in these cases, we don't want to resume on
        // a state where we're anchored over the player's island, as was the
        // case previously.
        if (w->is_far_camera()) {
            resume_far_ = true;
        }
    }

    ActiveWorldScene::enter(pfrm, app, prev);

    if (not app.opponent_island()) {
        return;
    }

    auto& cursor_loc = globals().far_cursor_loc_;

    if (initial_pos_) {
        cursor_loc = *initial_pos_;
    } else {
        snap(pfrm, app);
    }

    if (not app.player_island().get_drone(weapon_loc_)) {
        pfrm.speaker().play_sound("weapon_target", 3);
    }

    app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);

    if (near_) {
        if (auto room = app.player_island().get_room(weapon_loc_)) {
            group_ = room->group();
        }
    }

    far_camera();

    minimap_init(pfrm, app);

    last_player_checksum_ = app.player_island().checksum();

    if (app.opponent_island()) {
        last_opponent_checksum_ = app.opponent_island()->checksum();
    }
}



void WeaponSetTargetScene::snap(Platform& pfrm, App& app)
{
    auto& cursor_loc = globals().far_cursor_loc_;

    bool weapon_is_missile = false;
    if (auto weapon = app.player_island().get_room(weapon_loc_)) {
        weapon_is_missile = str_eq(weapon->name(), "missile-silo") or
                            str_eq(weapon->name(), "rocket-bomb");
    }

    Buffer<std::pair<Room*, RoomCoord>, 16> choices;

    if (weapon_is_missile) {
        for (u32 x = 0; x < app.opponent_island()->terrain().size(); ++x) {
            for (int y = construction_zone_min_y; y < 15; ++y) {
                auto room = app.opponent_island()->get_room({(u8)x, (u8)y});
                if (room) {
                    choices.push_back({room, {(u8)x, (u8)y}});
                    break;
                }
            }
        }
    } else {
        for (int y = construction_zone_min_y; y < 15; ++y) {
            for (u32 x = 0; x < app.opponent_island()->terrain().size(); ++x) {
                auto room = app.opponent_island()->get_room({(u8)x, (u8)y});
                if (room) {
                    choices.push_back({room, {(u8)x, (u8)y}});
                    break;
                }
            }
        }
    }

    if (choices.empty()) {
        return;
    }

    std::sort(choices.begin(), choices.end(), [](auto& lhs, auto& rhs) {
        return (*lhs.first->metaclass())->atp_value() >
               (*rhs.first->metaclass())->atp_value();
    });

    cursor_loc.x = choices[0].second.x;
    cursor_loc.y = choices[0].second.y;
}



} // namespace skyland
