////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "weaponSetTargetScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/incinerator.hpp"
#include "skyland/rooms/rocketSilo.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void describe_room(Island* island,
                   const RoomCoord& cursor_loc,
                   Optional<Text>& room_description);



void clear_room_description(Optional<Text>& room_description);



std::tuple<u8, u8, Island*> check_island_tapclick(const Vec2<u32>& pos);



WeaponSetTargetScene::WeaponSetTargetScene(GroupSelection& sel)
    : weapon_loc_({}), near_(true)
{
    selection_ = allocate_dynamic<GroupSelection>("selgroup");
    **selection_ = sel;
}



WeaponSetTargetScene::WeaponSetTargetScene(const RoomCoord& weapon_loc,
                                           bool near,
                                           Optional<RoomCoord> initial_pos)
    : weapon_loc_(weapon_loc), near_(near), initial_pos_(initial_pos)
{
}



static const int minimap_start_tile = 181;
static const int minimap_isle_spacing = 3;
static bool minimap_disabled = false;


u8 minimap_width()
{
    int pixel_width =
        (3 * (1 + APP.player_island().terrain().size() + minimap_isle_spacing +
              (APP.opponent_island() ? APP.opponent_island()->terrain().size()
                                     : 0) +
              1));
    return pixel_width / 8 + (pixel_width % 8 > 0);
}



ScenePtr WeaponSetTargetScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    auto player_weapon_exit_scene = [&]() -> ScenePtr {
        if (resume_far_) {
            return make_scene<InspectP2Scene>();
        } else {
            return make_scene<ReadyScene>();
        }
    };

    auto drone_exit_scene = [&](Drone* drone) -> ScenePtr {
        if (is_player_island(drone->destination())) {
            globals().near_cursor_loc_ = drone->position();
            return make_scene<ReadyScene>();
        } else {
            globals().far_cursor_loc_ = drone->position();
            return make_scene<InspectP2Scene>();
        }
    };

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (not APP.opponent_island() or mt_prep_seconds not_eq 0) {
        return player_weapon_exit_scene();
    }

    if (APP.player_island().checksum() not_eq last_player_checksum_ or
        APP.opponent_island()->checksum() not_eq last_opponent_checksum_) {

        minimap_repaint_timer_ = milliseconds(100);

        last_player_checksum_ = APP.player_island().checksum();
        last_opponent_checksum_ = APP.opponent_island()->checksum();
    }

    auto& cursor_loc = globals().far_cursor_loc_;


    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();


    if (APP.player().key_down(Key::alt_2) and
        group_ not_eq Room::Group::none and
        not PLATFORM.network_peer().is_connected()) {
        firing_mode_ = (firing_mode_ + 1) % 3;
    }


    if (minimap_repaint_timer_ > 0) {
        minimap_repaint_timer_ -= delta;
        if (minimap_repaint_timer_ < 0) {
            minimap_repaint_timer_ = 0;
            minimap_repaint();
            cursor_tics_ = 0;
        }
    }

    if (cursor_tics_ > 4) {
        minimap_hide();
    }


    if (not queue_mode_ and APP.player().key_down(Key::select)) {
        if (PLATFORM.network_peer().is_connected()) {
            PLATFORM.speaker().play_sound("beep_error", 3);
            return null_scene();
        }
        PLATFORM.speaker().play_sound("weapon_target", 3);
        queue_mode_ = true;
        redraw_target_queue_text();
    }


    if (test_key(Key::right)) {
        if (cursor_loc.x < APP.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            ++cursor_tics_;
            clear_room_description(room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            PLATFORM.speaker().play_sound("cursor_tick", 0);
            APP.player().network_sync_cursor(cursor_loc, 2, false);
        }
    }
    if (test_key(Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            ++cursor_tics_;
            clear_room_description(room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            PLATFORM.speaker().play_sound("cursor_tick", 0);
            APP.player().network_sync_cursor(cursor_loc, 2, false);
        }
    }
    if (test_key(Key::up)) {
        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            ++cursor_tics_;
            clear_room_description(room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            PLATFORM.speaker().play_sound("cursor_tick", 0);
            APP.player().network_sync_cursor(cursor_loc, 2, false);
        }
    }
    if (test_key(Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            ++cursor_tics_;
            clear_room_description(room_description_);
            describe_room_timer_ = milliseconds(300);

            minimap_repaint_timer_ = milliseconds(110);

            PLATFORM.speaker().play_sound("cursor_tick", 0);
            APP.player().network_sync_cursor(cursor_loc, 2, false);
        }
    }

    auto onclick = [&](RoomCoord cursor_loc) -> ScenePtr {
        if (APP.opponent_island()->get_room(cursor_loc)) {

            if (queue_mode_) {
                if (not target_queue_.full()) {
                    target_queue_.push_back(PackedTarget::pack(cursor_loc));
                    redraw_target_queue_text();
                    return null_scene();
                }
            } else {
                target_queue_.push_back(PackedTarget::pack(cursor_loc));
            }

            auto do_set_target = [this, cursor_loc](Room& room) {
                room.set_target(target_queue_, true);
                network::packet::WeaponSetTarget packet;
                packet.weapon_x_ = room.position().x;
                packet.weapon_y_ = room.position().y;
                packet.target_x_ = cursor_loc.x;
                packet.target_y_ = cursor_loc.y;
                packet.weapon_near_ = true;
                network::transmit(packet);
            };


            auto room = APP.player_island().get_room(weapon_loc_);

            if (selection_) {
                for (auto& c : (*selection_)->rooms_) {
                    if (auto r = APP.player_island().get_room(c)) {
                        do_set_target(*r);
                    }
                }
                return player_weapon_exit_scene();
            } else if (group_ not_eq Room::Group::none) {

                auto with_group = [&](auto& callback) {
                    for (auto& r : APP.player_island().rooms()) {
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
                    Time max_reload = 0;

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
                    return make_scene<InspectP2Scene>();
                }
            } else {

                auto sync = [&](Drone& drone) {
                    network::packet::DroneSetTarget packet;
                    packet.drone_x_ = drone.position().x;
                    packet.drone_y_ = drone.position().y;
                    packet.target_x_ = cursor_loc.x;
                    packet.target_y_ = cursor_loc.y;
                    packet.drone_near_ = is_player_island(drone.destination());
                    packet.target_near_ = false;
                    network::transmit(packet);
                };

                if (near_) {
                    if (auto drone =
                            APP.player_island().get_drone(weapon_loc_)) {
                        (*drone)->set_target(target_queue_, true, false);
                        sync(**drone);

                        return drone_exit_scene(drone->get());
                    }
                } else {
                    if (auto drone =
                            APP.opponent_island()->get_drone(weapon_loc_)) {
                        (*drone)->set_target(target_queue_, true, false);
                        sync(**drone);

                        return drone_exit_scene(drone->get());
                    }
                }
            }
        }
        return null_scene();
    };

    if (test_key(Key::start)) {
        snap();
        camera_update_timer_ = milliseconds(500);
        minimap_repaint_timer_ = milliseconds(100);
    }
    if (test_key(Key::action_1) or target_queue_.full()) {
        if (auto scene = onclick(cursor_loc)) {
            return scene;
        }
    }


    if (APP.player().key_down(Key::action_2)) {
        if (near_) {
            if (auto drone = APP.player_island().get_drone(weapon_loc_)) {
                return drone_exit_scene(drone->get());
            }
        } else {
            if (auto drone = APP.opponent_island()->get_drone(weapon_loc_)) {
                return drone_exit_scene(drone->get());
            }
        }
        return player_weapon_exit_scene();
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            if (APP.opponent_island()) {
                describe_room(
                    APP.opponent_island(), cursor_loc, room_description_);
            }
        }
    }

    return null_scene();
}



void WeaponSetTargetScene::redraw_target_queue_text()
{
    if (not target_queue_text_) {
        target_queue_text_.emplace(OverlayCoord{0, 0});
    }

    auto fmt_str = SYSTR(weapon_target_queue);
    target_queue_text_->assign(
        format(fmt_str->c_str(), target_queue_.size(), target_queue_.capacity())
            .c_str());
}



void WeaponSetTargetScene::display()
{
    WorldScene::display();

    if (not APP.opponent_island()) {
        return;
    }

    auto origin = APP.opponent_island()->visual_origin();

    auto& cursor_loc = globals().far_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_texture_index((17 * 2));
    sprite.set_size(Sprite::Size::w16_h16);

    PLATFORM.screen().draw(sprite);

    if (firing_mode_) {
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(58, [&] {
            switch (firing_mode_) {
            default:
            case 1:
                return 0;
            case 2:
                return 1;
            }
        }());
        origin.x += 12.0_fixed;
        origin.y += 10.0_fixed;
        sprite.set_position(origin);
        PLATFORM.screen().draw(sprite);
    }

    static const int reticule_spr_idx = 95;

    Sprite::Alpha alpha = Sprite::Alpha::opaque;

    for (int i = target_queue_.size() - 1; i > -1; --i) {
        auto target = target_queue_[i].coord();

        auto pos = APP.opponent_island()->visual_origin();
        pos.x += Fixnum::from_integer(target.x * 16);
        pos.y += Fixnum::from_integer(target.y * 16);

        Sprite spr;
        spr.set_position(pos);
        spr.set_tidx_16x16(reticule_spr_idx, 1);
        spr.set_size(Sprite::Size::w16_h16);
        spr.set_alpha(alpha);

        PLATFORM.screen().draw(spr);

        alpha = Sprite::Alpha::translucent;
    }
}



void WeaponSetTargetScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    clear_room_description(room_description_);

    if (APP.game_mode() == App::GameMode::co_op) {

        if (auto room = APP.player_island().get_room(weapon_loc_)) {
            room->co_op_release_lock();
        }
    }

    if (state_bit_load(StateBit::minimap_on)) {
        if (not next.displays_minimap()) {
            minimap::hide();
        }
    } else {
        minimap::hide();
        PLATFORM.fill_overlay(0);
    }
}



void WeaponSetTargetScene::minimap_show()
{
    const u8 anchor = 29 - minimap_width();

    if (minimap_x_anchor_ == anchor and minimap_visible_) {
        return;
    }

    minimap_hide();

    if (minimap_disabled) {
        return;
    }

    minimap::show();

    minimap_visible_ = true;
    minimap_x_anchor_ = anchor;
}



void WeaponSetTargetScene::minimap_hide()
{
    if (not minimap_visible_) {
        return;
    }

    minimap::hide();

    minimap_visible_ = false;
}



bool WeaponSetTargetScene::displays_minimap()
{
    return true;
}



void WeaponSetTargetScene::minimap_init()
{
    minimap_repaint();
}



void WeaponSetTargetScene::minimap_repaint()
{
    if (minimap_disabled) {
        return;
    }

    minimap::Settings s;
    s.pixel_cache_ = &fb_cache_;

    if (selection_) {
        s.weapon_group_selection_ = &**selection_;
    }

    s.weapon_loc_ = weapon_loc_;
    s.group_ = group_;
    s.target_near_ = near_;

    minimap::repaint(s);

    minimap_show();
}



void WeaponSetTargetScene::enter(Scene& prev)
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

    ActiveWorldScene::enter(prev);

    if (not APP.opponent_island()) {
        return;
    }

    auto& cursor_loc = globals().far_cursor_loc_;

    if (initial_pos_) {
        cursor_loc = *initial_pos_;
    } else {
        snap();
    }

    if (not APP.player_island().get_drone(weapon_loc_)) {
        PLATFORM.speaker().play_sound("weapon_target", 3);
    }

    APP.player().network_sync_cursor(cursor_loc, 2, false);

    if (near_) {
        if (auto room = APP.player_island().get_room(weapon_loc_)) {
            group_ = room->group();
        }
    }

    far_camera();

    minimap_init();

    last_player_checksum_ = APP.player_island().checksum();

    if (APP.opponent_island()) {
        last_opponent_checksum_ = APP.opponent_island()->checksum();
    }
}



void WeaponSetTargetScene::snap()
{
    auto& cursor_loc = globals().far_cursor_loc_;

    bool weapon_is_missile = false;
    if (auto weapon = APP.player_island().get_room(weapon_loc_)) {
        weapon_is_missile = str_eq(weapon->name(), "missile-silo") or
                            str_eq(weapon->name(), "rocket-bomb");
    }

    Buffer<std::pair<Room*, RoomCoord>, 16> choices;

    if (weapon_is_missile) {
        for (u32 x = 0; x < APP.opponent_island()->terrain().size(); ++x) {
            for (int y = construction_zone_min_y; y < 15; ++y) {
                auto room = APP.opponent_island()->get_room({(u8)x, (u8)y});
                if (room) {
                    choices.push_back({room, {(u8)x, (u8)y}});
                    break;
                }
            }
        }
    } else {
        for (int y = construction_zone_min_y; y < 15; ++y) {
            for (u32 x = 0; x < APP.opponent_island()->terrain().size(); ++x) {
                auto room = APP.opponent_island()->get_room({(u8)x, (u8)y});
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



bool WeaponSetTargetScene::hide_chr_icon() const
{
    return true;
}



} // namespace skyland
