////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "placeDroneScene.hpp"
#include "readyScene.hpp"
#include "skyland/entity/drones/attackDrone.hpp"
#include "skyland/network.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



PlaceDroneScene::PlaceDroneScene(RoomCoord origin,
                                 const DroneMeta* drone_class,
                                 bool near)
    : matrix_(allocate_dynamic<bool[16][16]>("drone-placement-matrix")),
      origin_(origin), near_(near), drone_class_(drone_class)
{
    if (not matrix_) {
        PLATFORM.fatal("MDS: buffers exhausted");
    }

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            (*matrix_)[x][y] = true;
        }
    }

    camera_update_timer_ = milliseconds(500);
}



void get_drone_slots(bool slots[16][16], Island* dest_island, Island* parent)
{
    int island_min_y = 14;

    for (auto& room : dest_island->rooms()) {
        auto pos = room->position();

        if (pos.y < island_min_y) {
            island_min_y = pos.y;
        }

        for (int x = 0; x < room->size().x; ++x) {
            for (int y = 0; y < room->size().y; ++y) {
                slots[pos.x + x][pos.y + y] = false;
            }
        }
        if (not((*room->metaclass())->properties() &
                RoomProperties::roof_hidden)) {
            for (int x = 0; x < room->size().x; ++x) {
                slots[pos.x + x][pos.y - 1] = false;
            }
        }
    }

    for (auto& drone_sp : dest_island->drones()) {
        slots[drone_sp->position().x][drone_sp->position().y] = false;
    }

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if (x >= (int)dest_island->terrain().size()) {
                slots[x][y] = false;
            }
            if (not(dest_island == parent)) {
                // Limit drone placement around enemy's castle. Drones would be
                // overpowered if you could place them within empty gaps inside
                // an enemy's perimeter.
                if (x < (int)dest_island->terrain().size() - 1 and x > 0 and
                    ((island_min_y - 3 <= construction_zone_min_y and
                      y > construction_zone_min_y) or
                     (island_min_y - 3 > construction_zone_min_y and
                      y > island_min_y - 4 and y > construction_zone_min_y))) {
                    slots[x][y] = false;
                }
            }
            if (y > 14) {
                slots[x][y] = false;
            } else if (y < construction_zone_min_y) {
                slots[x][y] = false;
            }
        }
    }
}



void PlaceDroneScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    if (not near_) {
        far_camera();
    }

    message_.emplace(SYSTR(drone_position_prompt)->c_str(),
                     OverlayCoord{0, 19});

    for (int i = 0; i < message_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 18, 425);
    }

    Island* island = &APP.player_island();
    if (not near_ and APP.opponent_island()) {
        island = APP.opponent_island();
    }


    get_drone_slots(*matrix_, island, &APP.player_island());

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y] == true) {
                PLATFORM.set_tile(
                    island->layer(), x, y, Tile::airborne_selection);
            }
        }
    }
}



void PlaceDroneScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    message_.reset();
    PLATFORM.fill_overlay(0);

    Island* island = &APP.player_island();
    if (not near_ and APP.opponent_island()) {
        island = APP.opponent_island();
    }

    island->repaint();
}



void PlaceDroneScene::display()
{
    ActiveWorldScene::display();

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

    Island* island = &APP.player_island();
    if (not near_ and APP.opponent_island()) {
        island = APP.opponent_island();
    }

    Vec2<Fixnum> origin = island->visual_origin();

    RoomCoord cursor_loc;
    if (near_) {
        cursor_loc = globals().near_cursor_loc_;
    } else {
        cursor_loc = globals().far_cursor_loc_;
    }

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);
}



ScenePtr PlaceDroneScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (APP.player().key_down(Key::action_2)) {
        return make_scene<ReadyScene>();
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    Island* island = &APP.player_island();
    if (not near_ and APP.opponent_island()) {
        island = APP.opponent_island();
    }

    RoomCoord* cursor_loc;
    if (near_) {
        cursor_loc = &globals().near_cursor_loc_;
    } else {
        cursor_loc = &globals().far_cursor_loc_;
    }

    if (APP.player().key_down(Key::action_1)) {
        if ((*matrix_)[cursor_loc->x][cursor_loc->y]) {
            if (auto room = APP.player_island().get_room(origin_)) {
                if (auto drone =
                        (*drone_class_)
                            ->create(room->parent(),
                                     island,
                                     RoomCoord{origin_.x, u8(origin_.y - 1)})) {
                    (*drone)->set_movement_target(*cursor_loc);

                    if (not room->attach_drone(*drone)) {
                        return make_scene<ReadyScene>();
                    }

                    island->drones().push(*drone);

                    APP.set_coins(APP.coins() - (*drone_class_)->cost());

                    network::packet::DroneSpawn spawn;
                    spawn.origin_x_ = origin_.x;
                    spawn.origin_y_ = origin_.y - 1;

                    spawn.deploy_x_ = cursor_loc->x;
                    spawn.deploy_y_ = cursor_loc->y;

                    spawn.destination_near_ = is_player_island(island);

                    spawn.drone_class_ =
                        DroneMeta::index((*drone_class_)->name());

                    network::transmit(spawn);

                    globals().near_cursor_loc_ = origin_;
                    globals().near_cursor_loc_.y--;
                    return make_scene<ReadyScene>();
                }
            }
        }
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();


    if (test_key(Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (not near_) {
            globals().near_cursor_loc_.y = cursor_loc->y;
            globals().near_cursor_loc_.x =
                APP.player_island().terrain().size() - 1;
            return make_scene<PlaceDroneScene>(origin_, drone_class_, true);
        }
    }

    if (test_key(Key::right)) {
        if (cursor_loc->x < island->terrain().size() - 1) {
            ++cursor_loc->x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (near_ and APP.opponent_island()) {
            globals().far_cursor_loc_.y = cursor_loc->y;
            globals().far_cursor_loc_.x = 0;
            return make_scene<PlaceDroneScene>(origin_, drone_class_, false);
        }
    }

    if (test_key(Key::up)) {
        if (cursor_loc->y > construction_zone_min_y) {
            --cursor_loc->y;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::down)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }
    }


    return null_scene();
}


} // namespace skyland
