#include "moveCharacterScene.hpp"
#include "globals.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/path.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



MoveCharacterScene::MoveCharacterScene(Platform& pfrm)
    : matrix_(allocate_dynamic<bool[16][16]>(pfrm))
{
    if (not matrix_) {
        pfrm.fatal("MCS: buffers exhausted");
    }
}



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void MoveCharacterScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    app.player_island().render_interior(pfrm);
    app.player_island().repaint(pfrm);
}



void MoveCharacterScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    // TODO: parameterize island...
    app.player_island().plot_walkable_zones(*matrix_);

    // Now, we want to do a bfs search, to find all connected parts of the
    // walkable areas.


    // TODO: again, parameterize island. Currently using near cursor.
    auto cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

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
                pfrm.set_tile(app.player_island().layer(), x, y, 34);
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

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;


    if (pfrm.keyboard().down_transition<Key::left>()) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
        }
    }

    if (pfrm.keyboard().down_transition<Key::right>()) {
        if (cursor_loc.x < app.player_island().terrain().size()) {
            ++cursor_loc.x;
        }
    }

    if (pfrm.keyboard().down_transition<Key::up>()) {
        if (cursor_loc.y > 6) {
            --cursor_loc.y;
        }
    }

    if (pfrm.keyboard().down_transition<Key::down>()) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
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

    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (pfrm.keyboard().down_transition<Key::action_1>()
         and (*matrix_)[cursor_loc.x][cursor_loc.y]) {
        // FIXME: this instantly jumps the character to a room. We want to
        // actually calculate a path, and have the character walk.
        if (auto room = app.player_island().get_room(initial_cursor_)) {
            for (auto it = room->characters().begin();
                 it not_eq room->characters().end();
                 ++it) {
                if ((*it)->grid_position() == initial_cursor_) {

                    auto path = find_path(pfrm,
                                          &app.player_island(),
                                          initial_cursor_,
                                          cursor_loc);

                    if (path and *path) {
                        (*it)->set_movement_path(std::move(*path));
                    } else {
                        // path not found, raise error?
                    }
                    return scene_pool::alloc<ReadyScene>();
                }
            }
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

    auto origin = app.player_island().origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

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
