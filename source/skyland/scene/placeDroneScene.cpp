#include "placeDroneScene.hpp"
#include "readyScene.hpp"
#include "skyland/entity/drones/attackDrone.hpp"
#include "skyland/network.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



PlaceDroneScene::PlaceDroneScene(Platform& pfrm,
                                 Vec2<u8> origin,
                                 DroneMeta* drone_class,
                                 bool near)
    : matrix_(allocate_dynamic<bool[16][16]>(pfrm, "drone-placement-matrix")),
      origin_(origin), near_(near), drone_class_(drone_class)
{
    if (not matrix_) {
        pfrm.fatal("MDS: buffers exhausted");
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
    for (auto& room : dest_island->rooms()) {
        auto pos = room->position();
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
                    y > construction_zone_min_y) {
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



void PlaceDroneScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    if (not near_) {
        far_camera();
    }

    message_.emplace(
        pfrm, SYSTR(drone_position_prompt)->c_str(), OverlayCoord{0, 19});

    for (int i = 0; i < message_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }

    Island* island = &app.player_island();
    if (not near_ and app.opponent_island()) {
        island = app.opponent_island();
    }


    get_drone_slots(*matrix_, island, &app.player_island());

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y] == true) {
                pfrm.set_tile(island->layer(), x, y, Tile::airborne_selection);
            }
        }
    }
}



void PlaceDroneScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    message_.reset();
    pfrm.fill_overlay(0);

    Island* island = &app.player_island();
    if (not near_ and app.opponent_island()) {
        island = app.opponent_island();
    }

    island->repaint(pfrm, app);
}



void PlaceDroneScene::display(Platform& pfrm, App& app)
{
    ActiveWorldScene::display(pfrm, app);

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    Island* island = &app.player_island();
    if (not near_ and app.opponent_island()) {
        island = app.opponent_island();
    }

    Vec2<Float> origin = island->visual_origin();

    Vec2<u8> cursor_loc;
    if (near_) {
        cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    } else {
        cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
    }

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);
}



ScenePtr<Scene>
PlaceDroneScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    Island* island = &app.player_island();
    if (not near_ and app.opponent_island()) {
        island = app.opponent_island();
    }

    Vec2<u8>* cursor_loc;
    if (near_) {
        cursor_loc = &std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    } else {
        cursor_loc = &std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if ((*matrix_)[cursor_loc->x][cursor_loc->y]) {
            if (auto room = app.player_island().get_room(origin_)) {
                if (auto drone =
                        (*drone_class_)
                            ->create(room->parent(),
                                     island,
                                     Vec2<u8>{origin_.x, u8(origin_.y - 1)})) {
                    (*drone)->set_movement_target(*cursor_loc);

                    if (not room->attach_drone(pfrm, app, *drone)) {
                        return scene_pool::alloc<ReadyScene>();
                    }

                    island->drones().push(*drone);

                    app.set_coins(pfrm, app.coins() - (*drone_class_)->cost());

                    network::packet::DroneSpawn spawn;
                    spawn.origin_x_ = origin_.x;
                    spawn.origin_y_ = origin_.y - 1;

                    spawn.deploy_x_ = cursor_loc->x;
                    spawn.deploy_y_ = cursor_loc->y;

                    spawn.destination_near_ = island == &app.player_island();

                    spawn.drone_class_ =
                        DroneMeta::index((*drone_class_)->name());

                    network::transmit(pfrm, spawn);


                    return scene_pool::alloc<ReadyScene>();
                }
            }
        }
    }

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    if (test_key(Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
        } else if (not near_) {
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y =
                cursor_loc->y;
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_.x =
                app.player_island().terrain().size() - 1;
            return scene_pool::alloc<PlaceDroneScene>(
                pfrm, origin_, drone_class_, true);
        }
    }

    if (test_key(Key::right)) {
        if (cursor_loc->x < island->terrain().size() - 1) {
            ++cursor_loc->x;
        } else if (near_ and app.opponent_island()) {
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_.y =
                cursor_loc->y;
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_.x = 0;
            return scene_pool::alloc<PlaceDroneScene>(
                pfrm, origin_, drone_class_, false);
        }
    }

    if (test_key(Key::up)) {
        if (cursor_loc->y > construction_zone_min_y) {
            --cursor_loc->y;
        }
    }

    if (test_key(Key::down)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
        }
    }


    return null_scene();
}


} // namespace skyland
