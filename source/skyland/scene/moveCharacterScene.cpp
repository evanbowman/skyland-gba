////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "moveCharacterScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/minimap.hpp"
#include "skyland/network.hpp"
#include "skyland/path.hpp"
#include "skyland/rooms/portal.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "textEntryScene.hpp"



namespace skyland
{



ModifyCharacterScene::ModifyCharacterScene(CharacterId chr_id, bool near)
    : matrix_(allocate_dynamic<bool[16][16]>("chr-movement-slots")),
      chr_id_(chr_id), near_(near)
{
    if (not matrix_) {
        Platform::fatal("MCS: buffers exhausted");
    }
}



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y);



void flood_fill_through_portals(Island& isle,
                                u8 matrix[16][16],
                                u8 replace,
                                u8 x,
                                u8 y)
{
    flood_fill(matrix, replace, x, y);

    // Good lord this is horrible...
    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y] == replace) {
                if (auto room = isle.get_room({x, y})) {
                    if (room->cast<Portal>()) {
                        for (u8 xx = 0; xx < 16; ++xx) {
                            for (u8 yy = 0; yy < 16; ++yy) {
                                if (matrix[xx][yy] not_eq replace) {
                                    if (auto o = isle.get_room({xx, yy})) {
                                        if (o->cast<Portal>()) {
                                            flood_fill(matrix, replace, xx, yy);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}



void ModifyCharacterScene::exit(Scene& next)
{
    WorldScene::exit(next);


    Island* island = nullptr;

    if (near_) {
        island = &APP.player_island();
    } else if (APP.opponent_island()) {
        island = APP.opponent_island();
    }

    island->render_interior_fast();

    if (APP.game_mode() == App::GameMode::co_op) {

        network::packet::CoOpChrLockRelease pkt;
        pkt.chr_id_.set(chr_id_);
        network::transmit(pkt);

        if (auto chr = Character::find_by_id(chr_id_).first) {
            chr->co_op_release_lock();
        }
    }

    if (not next.displays_minimap()) {
        minimap::hide();
    }
}



void ModifyCharacterScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    if (not near_) {
        far_camera();
    }

    Island* island = nullptr;

    if (near_) {
        island = &APP.player_island();
    } else if (APP.opponent_island()) {
        island = APP.opponent_island();
    }

    island_checksums_ = island_checksums();

    if (state_bit_load(StateBit::minimap_on)) {
        minimap::repaint({.show_destroyed_rooms_ = true});
        minimap::show();
    }

    auto found = Character::find_by_id(chr_id_);

    island->plot_walkable_zones(*matrix_, found.first);

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
    flood_fill_through_portals(*island, matrix, 2, cursor_loc.x, cursor_loc.y);


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
                PLATFORM.set_tile(
                    island->layer(), x, y, StaticTile::path_marker);
            }
        }
    }
}



ScenePtr ModifyCharacterScene::update(Time delta)
{
    if (APP.player().key_down(Key::select)) {
        return null_scene();
    }

    const auto last_checksums = island_checksums_;

    island_checksums_ = island_checksums();

    if (state_bit_load(StateBit::minimap_on) and
        (minimap::needs_repaint() or island_checksums_ not_eq last_checksums)) {
        minimap::repaint({.show_destroyed_rooms_ = true});
        minimap::show();
    }

    Island* island = nullptr;

    if (near_) {
        island = &APP.player_island();
    } else if (APP.opponent_island()) {
        island = APP.opponent_island();
    }

    RoomCoord* cursor_loc = nullptr;

    if (near_) {
        cursor_loc = &globals().near_cursor_loc_;
    } else {
        cursor_loc = &globals().far_cursor_loc_;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();

    if (test_key(Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::right)) {
        if (cursor_loc->x < island->terrain().size()) {
            ++cursor_loc->x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
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

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }


    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (APP.player().key_down(Key::action_2)) {
        if (near_) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<InspectP2Scene>();
        }
    }

    if (APP.player().key_down(Key::action_1) and
        (*matrix_)[cursor_loc->x][cursor_loc->y]) {

        auto sel_chr = [&]() -> Character* {
            return Character::find_by_id(chr_id_).first;
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

            auto path = find_path(island, sel_chr, current, *cursor_loc);

            if (path and *path) {
                sel_chr->set_movement_path(std::move(*path));
                sel_chr->set_wants_to_chat(false);
                sel_chr->pin();

                network::packet::ChrSetTargetV2 packet;
                packet.target_x_ = cursor_loc->x;
                packet.target_y_ = cursor_loc->y;
                packet.chr_id_.set(sel_chr->id());
                packet.near_island_ = island not_eq &APP.player_island();
                network::transmit(packet);

            } else {
                // path not found, raise error?
            }
        }

        if (near_) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<InspectP2Scene>();
        }
    }

    return null_scene();
}



void ModifyCharacterScene::display()
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((15 * 2) + cursor_anim_frame_);


    Vec2<Fixnum> origin;
    if (near_) {
        origin = APP.player_island().visual_origin();
    } else {
        if (APP.opponent_island()) {
            origin = APP.opponent_island()->visual_origin();
        }
    }

    const auto cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);

    WorldScene::display();

    // Sprite sprite;
    // sprite.set_texture_index(19);
    // sprite.set_size(Sprite::Size::w16_h32);

    // for (int x = 0; x < 16; ++x) {
    //     for (int y = 0; y < 16; ++y) {
    //         if ((*matrix_)[x][y]) {
    //             auto origin = APP.player_island().origin();
    //             origin.x += x * 16;
    //             origin.y += (y - 1) * 16;
    //             sprite.set_position(origin);
    //             PLATFORM.screen().draw(sprite);
    //         }
    //     }
    // }
}



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y)
{
    using Coord = RoomCoord;

    ScratchBufferBulkAllocator mem;

    auto stack = mem.alloc<Buffer<Coord, 16 * 16>>();

    if (UNLIKELY(not stack)) {
        PLATFORM.fatal("fatal error in floodfill");
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



bool ModifyCharacterScene::displays_minimap()
{
    return true;
}



} // namespace skyland
