#include "moveCharacterScene.hpp"
#include "skyland/skyland.hpp"
#include "readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "globals.hpp"



namespace skyland {



MoveCharacterScene::MoveCharacterScene(Platform& pfrm) :
    matrix_(allocate_dynamic<bool[16][16]>(pfrm))
{
    if (not matrix_) {
        pfrm.fatal("MCS: buffers exhausted");
    }
}



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void MoveCharacterScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    // TODO: parameterize island...
    app.player_island().plot_walkable_zones(*matrix_);

    // Now, we want to do a bfs search, to find all connected parts of the
    // walkable areas.

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

    // TODO: again, parameterize island. Currently using near cursor.
    auto cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;


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
}



ScenePtr<Scene> MoveCharacterScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.keyboard().down_transition<Key::alt_1>()) {
        // We can only get into the MoveCharacterScene when viewing the interior
        // of our building. When switching to view the exterior, cancel out of
        // the current scene.
        pfrm.load_tile0_texture("tilesheet");
        app.player_island().render_exterior(pfrm);
        return scene_pool::alloc<ReadyScene>();
    }

    if (auto new_scene = WorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



void MoveCharacterScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    Sprite sprite;
    sprite.set_texture_index(19);
    sprite.set_size(Sprite::Size::w16_h32);

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y]) {
                auto origin = app.player_island().origin();
                origin.x += x * 16;
                origin.y += (y - 1) * 16;
                sprite.set_position(origin);
                pfrm.screen().draw(sprite);
            }
        }
    }
}



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y)
{
    using Coord = Vec2<u8>;

    ScratchBufferBulkAllocator mem(pfrm);

    auto stack = mem.alloc<Buffer<Coord, 16*16>>();

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



}
