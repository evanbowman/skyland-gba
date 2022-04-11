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
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



std::tuple<u8, u8, Island*>
check_island_tapclick(Platform& pfrm, App& app, const Vec2<u32>& pos);



WeaponSetTargetScene::WeaponSetTargetScene(const Vec2<u8>& weapon_loc,
                                           bool near,
                                           std::optional<Vec2<u8>> initial_pos)
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
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_ =
                drone->position();
            return scene_pool::alloc<ReadyScene>();
        } else {
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_ =
                drone->position();
            return scene_pool::alloc<InspectP2Scene>();
        }
    };

    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (targets_.empty() or not app.opponent_island() or
        mt_prep_seconds not_eq 0) {
        return player_weapon_exit_scene();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;


    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    if (test_key(Key::right)) {
        if (cursor_loc.x < app.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::up)) {
        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }
    if (test_key(Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);

            app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);
        }
    }

    auto onclick = [&](Vec2<u8> cursor_loc) -> ScenePtr<Scene> {
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

                // If the room has a group assigned, then assign a target
                // for all rooms of the same group.
                for (auto& r : app.player_island().rooms()) {
                    if (r->group() == group_) {
                        do_set_target(*r);
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

    if (targets_.empty()) {
        return;
    }

    if (not app.opponent_island()) {
        return;
    }

    auto origin = app.opponent_island()->visual_origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_texture_index(17);
    sprite.set_size(Sprite::Size::w16_h32);

    pfrm.screen().draw(sprite);
}



void WeaponSetTargetScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    clear_room_description(pfrm, room_description_);

    if (app.game_mode() == App::GameMode::co_op) {
        network::packet::CoopRoomLockRelease pkt;
        pkt.x_ = weapon_loc_.x;
        pkt.y_ = weapon_loc_.y;
        network::transmit(pfrm, pkt);

        if (auto room = app.player_island().get_room(weapon_loc_)) {
            room->co_op_release_lock();
        }
    }
}



void WeaponSetTargetScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (auto w = dynamic_cast<WorldScene*>(&prev)) {
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

    collect_targets(pfrm, app);

    if (not targets_.empty()) {
        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
        cursor_loc.x = targets_[selector_].x;
        cursor_loc.y = targets_[selector_].y;

        app.player().network_sync_cursor(pfrm, cursor_loc, 2, false);

        if (initial_pos_) {
            cursor_loc = *initial_pos_;
        }
    }

    if (near_) {
        if (auto room = app.player_island().get_room(weapon_loc_)) {
            group_ = room->group();
        }
    }

    far_camera();
}



void WeaponSetTargetScene::collect_targets(Platform& pfrm, App& app)
{
    targets_.clear();

    if (app.opponent_island()) {
        Island& island = *app.opponent_island();

        for (auto& room : island.rooms()) {
            targets_.push_back(room->position());
        }
    }

    std::sort(targets_.begin(),
              targets_.end(),
              [](const Vec2<u8>& lhs, const Vec2<u8>& rhs) {
                  return lhs.x < rhs.x || (lhs.x == rhs.x and lhs.y < rhs.y);
              });
}



} // namespace skyland
