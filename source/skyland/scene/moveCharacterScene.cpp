#include "moveCharacterScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/path.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



MoveCharacterScene::MoveCharacterScene(Platform& pfrm, bool near)
    : matrix_(allocate_dynamic<bool[16][16]>(pfrm)), near_(near)
{
    if (not matrix_) {
        pfrm.fatal("MCS: buffers exhausted");
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
        island = &*app.opponent_island();
    }

    island->render_interior(pfrm);
    // island->repaint()
    // app.player_island().render_interior(pfrm);
    // app.player_island().repaint(pfrm);
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
        island = &*app.opponent_island();
    }

    island->plot_walkable_zones(*matrix_);

    // Now, we want to do a bfs walk, to find all connected parts of the
    // walkable areas.

    Vec2<u8> cursor_loc;

    if (near_) {
        cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    } else {
        cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
    }

    initial_cursor_ = cursor_loc;

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
                pfrm.set_tile(island->layer(), x, y, 34);
            }
        }
    }
}



ScenePtr<Scene>
MoveCharacterScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.keyboard().down_transition<Key::select>()) {
        return null_scene();
    }

    Island* island = nullptr;

    if (near_) {
        island = &app.player_island();
    } else if (app.opponent_island()) {
        island = &*app.opponent_island();
    }

    Vec2<u8>* cursor_loc = nullptr;

    if (near_) {
        cursor_loc = &std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    } else {
        cursor_loc = &std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
    }

    if (key_down<Key::left>(pfrm)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
        }
    }

    if (key_down<Key::right>(pfrm)) {
        if (cursor_loc->x < island->terrain().size()) {
            ++cursor_loc->x;
        }
    }

    if (key_down<Key::up>(pfrm)) {
        if (cursor_loc->y > 6) {
            --cursor_loc->y;
        }
    }

    if (key_down<Key::down>(pfrm)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
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

    if (key_down<Key::action_2>(pfrm)) {
        if (near_) {
            return scene_pool::alloc<ReadyScene>();
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    }

    if (key_down<Key::action_1>(pfrm) and
        (*matrix_)[cursor_loc->x][cursor_loc->y]) {

        auto sel_chr = [&]() -> BasicCharacter* {
            if (auto room = island->get_room(initial_cursor_)) {
                for (auto it = room->characters().begin();
                     it not_eq room->characters().end();
                     ++it) {
                    if ((*it)->grid_position() == initial_cursor_ and
                        (*it)->owner() == &app.player()) {

                        return it->get();
                    }
                }
            }
            return nullptr;
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

            auto path = find_path(pfrm, island, initial_cursor_, *cursor_loc);

            if (path and *path) {
                sel_chr->set_movement_path(std::move(*path));
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
    WorldScene::display(pfrm, app);

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    Vec2<Float> origin;
    if (near_) {
        origin = app.player_island().origin();
    } else {
        if (app.opponent_island()) {
            origin = app.opponent_island()->origin();
        }
    }

    const auto cursor_loc =
        near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
              : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);


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
    using Coord = Vec2<u8>;

    ScratchBufferBulkAllocator mem(pfrm);

    auto stack = mem.alloc<Buffer<Coord, 16 * 16>>();

    if (UNLIKELY(not stack)) {
        pfrm.fatal("fatal error in floodfill");
    }

    if (x >= 16 or y >= 16) {
        return 0;
    }

    const u8 target = matrix[x][y];

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
