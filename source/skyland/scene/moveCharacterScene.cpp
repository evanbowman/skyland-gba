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


#include "moveCharacterScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/path.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



MoveCharacterScene::MoveCharacterScene(CharacterId chr_id, bool near)
    : matrix_(allocate_dynamic<bool[16][16]>("chr-movement-slots")),
      chr_id_(chr_id), near_(near)
{
    if (not matrix_) {
        Platform::fatal("MCS: buffers exhausted");
    }
}



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void MoveCharacterScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);


    Island* island = nullptr;

    if (near_) {
        island = &app.player_island();
    } else if (app.opponent_island()) {
        island = app.opponent_island();
    }

    island->render_interior(pfrm, app);

    if (app.game_mode() == App::GameMode::co_op) {

        network::packet::CoOpChrLockRelease pkt;
        pkt.chr_id_.set(chr_id_);
        network::transmit(pfrm, pkt);

        if (auto chr = BasicCharacter::find_by_id(app, chr_id_).first) {
            chr->co_op_release_lock();
        }
    }
}



void MoveCharacterScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    if (not near_) {
        far_camera();
    }

    Island* island = nullptr;

    if (near_) {
        island = &app.player_island();
    } else if (app.opponent_island()) {
        island = app.opponent_island();
    }

    island->plot_walkable_zones(app, *matrix_);

    // Now, we want to do a bfs walk, to find all connected parts of the
    // walkable areas.

    RoomCoord cursor_loc;

    if (near_) {
        cursor_loc = globals().near_cursor_loc_;
    } else {
        cursor_loc = globals().far_cursor_loc_;
    }

    if (not(*matrix_)[cursor_loc.x][cursor_loc.y]) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                // Because you should only be in this state if you selected a
                // character, which should only ever be standing in a valid
                // tile, you shouldn't be able to get into a state where you've
                // selected a non-walkable tile as the starting point.
                return;
            }
        }
    }

    u8 matrix[16][16];
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y]) {
                matrix[x][y] = 1;
            } else {
                matrix[x][y] = 0;
            }
        }
    }

    // Ok, so now we do a flood fill, to find all cells reachable from the
    // position of the cursor. Erase all othre disconnected components.
    flood_fill(pfrm, matrix, 2, cursor_loc.x, cursor_loc.y);


    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if (matrix[x][y] == 2) {
                (*matrix_)[x][y] = true;
            } else {
                (*matrix_)[x][y] = false;
            }
        }
    }


    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y]) {
                pfrm.set_tile(island->layer(), x, y, StaticTile::path_marker);
            }
        }
    }
}



ScenePtr<Scene>
MoveCharacterScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::select)) {
        return null_scene();
    }

    Island* island = nullptr;

    if (near_) {
        island = &app.player_island();
    } else if (app.opponent_island()) {
        island = app.opponent_island();
    }

    RoomCoord* cursor_loc = nullptr;

    if (near_) {
        cursor_loc = &globals().near_cursor_loc_;
    } else {
        cursor_loc = &globals().far_cursor_loc_;
    }

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);

    if (test_key(Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::right)) {
        if (cursor_loc->x < island->terrain().size()) {
            ++cursor_loc->x;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::up)) {
        if (cursor_loc->y > construction_zone_min_y) {
            --cursor_loc->y;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::down)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }


    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (near_) {
            return scene_pool::alloc<ReadyScene>();
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    }

    if (app.player().key_down(pfrm, Key::action_1) and
        (*matrix_)[cursor_loc->x][cursor_loc->y]) {

        auto sel_chr = [&]() -> BasicCharacter* {
            return BasicCharacter::find_by_id(app, chr_id_).first;
        }();


        if (sel_chr) {

            for (auto& room : island->rooms()) {
                for (auto& other : room->characters()) {
                    if (other.get() not_eq sel_chr and
                        other->owner() == sel_chr->owner()) {

                        if (auto dest = other->destination()) {
                            // We don't want to allow a character to move into a
                            // slot that another character is already moving
                            // into.
                            if (*dest == *cursor_loc) {
                                return null_scene();
                            }
                        } else if (other->grid_position() == *cursor_loc) {
                            // We don't want to allow a character to move into a
                            // slot that another non-moving character already
                            // occupies.
                            return null_scene();
                        }
                    }
                }
            }

            auto current = sel_chr->grid_position();

            auto path = find_path(pfrm, app, island, current, *cursor_loc);

            if (path and *path) {
                sel_chr->set_movement_path(pfrm, app, std::move(*path));

                network::packet::ChrSetTargetV2 packet;
                packet.target_x_ = cursor_loc->x;
                packet.target_y_ = cursor_loc->y;
                packet.chr_id_.set(sel_chr->id());
                packet.near_island_ = island not_eq &app.player_island();
                network::transmit(pfrm, packet);

            } else {
                // path not found, raise error?
            }
        }

        if (near_) {
            return scene_pool::alloc<ReadyScene>();
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    }

    return null_scene();
}



void MoveCharacterScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);


    Vec2<Fixnum> origin;
    if (near_) {
        origin = app.player_island().visual_origin();
    } else {
        if (app.opponent_island()) {
            origin = app.opponent_island()->visual_origin();
        }
    }

    const auto cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);

    WorldScene::display(pfrm, app);

    // Sprite sprite;
    // sprite.set_texture_index(19);
    // sprite.set_size(Sprite::Size::w16_h32);

    // for (int x = 0; x < 16; ++x) {
    //     for (int y = 0; y < 16; ++y) {
    //         if ((*matrix_)[x][y]) {
    //             auto origin = app.player_island().origin();
    //             origin.x += x * 16;
    //             origin.y += (y - 1) * 16;
    //             sprite.set_position(origin);
    //             pfrm.screen().draw(sprite);
    //         }
    //     }
    // }
}



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y)
{
    using Coord = RoomCoord;

    ScratchBufferBulkAllocator mem;

    auto stack = mem.alloc<Buffer<Coord, 16 * 16>>();

    if (UNLIKELY(not stack)) {
        pfrm.fatal("fatal error in floodfill");
    }

    if (x >= 16 or y >= 16) {
        return 0;
    }

    const u8 target = matrix[x][y];

    if (target == replace) {
        return 0;
    }

    u32 count = 0;

    const auto action = [&](const Coord& c, u8 x_off, u8 y_off) {
        const u8 x = c.x + x_off;
        const u8 y = c.y + y_off;
        if (x < 16 and y < 16) {
            if (matrix[x][y] == target) {
                matrix[x][y] = replace;
                stack->push_back({x, y});
                count += 1;
            }
        }
    };

    action({x, y}, 0, 0);

    while (not stack->empty()) {
        Coord c = stack->back();
        stack->pop_back();
        action(c, -1, 0);
        action(c, 0, 1);
        action(c, 0, -1);
        action(c, 1, 0);
    }

    return count;
}



} // namespace skyland
