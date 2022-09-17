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


    if (test_key(Key::right)) {
        if (cursor_loc.x < app.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::up)) {
        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            pfrm.speaker().play_sound("cursor_tick", 0);
            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

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

                auto with_group =
                    [&](auto& callback) {
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

                    auto collect =
                        [&](Room& r) {
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

                    auto update_timers =
                        [&](Room& r) {
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

                    auto update_timers =
                        [max_reload](Room& r) {
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

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_texture_index(17);
    sprite.set_size(Sprite::Size::w16_h32);

    pfrm.screen().draw(sprite);

    if (firing_mode_) {
        sprite.set_texture_index(111 + firing_mode_);
        origin.x += 12;
        origin.y += 10;
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
                for (u32 x = 0; x < app.opponent_island()->terrain().size();
                     ++x) {
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
}



} // namespace skyland
