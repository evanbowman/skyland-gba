#include "weaponSetTargetScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



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
        return scene_pool::alloc<ReadyScene>();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    if (freeform_) {

        if (app.player().key_down(pfrm, Key::alt_2)) {
            freeform_ = false;
        }

        if (app.player().key_down(pfrm, Key::right)) {
            if (cursor_loc.x < app.opponent_island()->terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (app.player().key_down(pfrm, Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (app.player().key_down(pfrm, Key::up)) {
            if (cursor_loc.y > 6) {
                --cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (app.player().key_down(pfrm, Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (app.player().key_down(pfrm, Key::action_1)) {
            if (app.opponent_island()->get_room(cursor_loc)) {

                if (auto room = app.player_island().get_room(weapon_loc_)) {

                    room->set_target(pfrm, app, cursor_loc, true);
                    network::packet::WeaponSetTarget packet;
                    packet.weapon_x_ = weapon_loc_.x;
                    packet.weapon_y_ = weapon_loc_.y;
                    packet.target_x_ = cursor_loc.x;
                    packet.target_y_ = cursor_loc.y;
                    network::transmit(pfrm, packet);

                    if (near_) {
                        return scene_pool::alloc<ReadyScene>();
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
                            (*drone)->set_target(cursor_loc);
                            sync(**drone);

                            return drone_exit_scene(drone->get());
                        }
                    } else {
                        if (auto drone =
                                app.opponent_island()->get_drone(weapon_loc_)) {
                            (*drone)->set_target(cursor_loc);
                            sync(**drone);

                            return drone_exit_scene(drone->get());
                        }
                    }
                }
            }
        }
    } else {
        cursor_loc.x = targets_[selector_].x;
        cursor_loc.y = targets_[selector_].y;

        if (app.player().key_down(pfrm, Key::alt_2)) {
            freeform_ = true;
        }

        if (app.player().key_down(pfrm, Key::right) or
            app.player().key_down(pfrm, Key::down)) {

            if (selector_ < (int)targets_.size() - 1) {
                selector_++;
            } else {
                selector_ = 0;
            }

            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }

        if (app.player().key_down(pfrm, Key::left) or
            app.player().key_down(pfrm, Key::up)) {

            if (selector_ > 0) {
                --selector_;
            } else {
                selector_ = targets_.size() - 1;
            }

            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            const auto target = targets_[selector_];
            if (auto room = app.player_island().get_room(weapon_loc_)) {
                room->set_target(pfrm, app, target, true);

                network::packet::WeaponSetTarget packet;
                packet.weapon_x_ = weapon_loc_.x;
                packet.weapon_y_ = weapon_loc_.y;
                packet.target_x_ = target.x;
                packet.target_y_ = target.y;
                network::transmit(pfrm, packet);
            }
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
        return scene_pool::alloc<ReadyScene>();
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            if (app.opponent_island()) {
                describe_room(pfrm,
                              app,
                              &*app.opponent_island(),
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
}



void WeaponSetTargetScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    collect_targets(pfrm, app);

    if (not targets_.empty()) {
        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
        cursor_loc.x = targets_[selector_].x;
        cursor_loc.y = targets_[selector_].y;

        if (initial_pos_) {
            cursor_loc = *initial_pos_;
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
