#include "moveDroneScene.hpp"
#include "skyland/skyland.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/droneBay.hpp"



namespace skyland {



MoveDroneScene::MoveDroneScene(Platform& pfrm, Vec2<u8> origin)
    : matrix_(allocate_dynamic<bool[16][16]>(pfrm)),
      origin_(origin)
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



void MoveDroneScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    auto island = &app.player_island();

    for (auto& room : island->rooms()) {
        auto pos = room->position();
        for (int x = 0; x < room->size().x; ++x) {
            for (int y = 0; y < room->size().y; ++y) {
                (*matrix_)[pos.x + x][pos.y + y] = false;
            }
        }
        if (room->has_roof()) {
            for (int x = 0; x < room->size().x; ++x) {
                (*matrix_)[pos.x + x][pos.y - 1] = false;
            }
        }
    }

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if (x > (int)island->terrain().size()) {
                (*matrix_)[x][y] = false;
            }
            if (y > 14) {
                (*matrix_)[x][y] = false;
            }
            if ((*matrix_)[x][y] == true) {
                pfrm.set_tile(island->layer(), x, y, 103);
            }
        }
    }
}



void MoveDroneScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    auto island = &app.player_island();

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            pfrm.set_tile(island->layer(), x, y, 0);
        }
    }

    island->repaint(pfrm);
}



void MoveDroneScene::display(Platform& pfrm, App& app)
{
    ActiveWorldScene::display(pfrm, app);

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    Vec2<Float> origin = app.player_island().origin();

    Vec2<u8>* cursor_loc =
        &std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    origin.x += cursor_loc->x * 16;
    origin.y += cursor_loc->y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);
}



ScenePtr<Scene> MoveDroneScene::update(Platform& pfrm,
                                       App& app,
                                       Microseconds delta)
{
    if (auto new_scene = WorldScene::update(pfrm, app, delta)) {
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

    auto island = &app.player_island();

    Vec2<u8>* cursor_loc =
        &std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    if (app.player().key_down(pfrm, Key::action_1)) {
        if ((*matrix_)[cursor_loc->x][cursor_loc->y]) {
            if (auto room = island->get_room(origin_)) {
                if (auto db = dynamic_cast<DroneBay*>(room)) {
                    if (auto drone = alloc_shared_entity<Drone>(room->parent(), *cursor_loc)) {
                        db->attach_drone(*drone);
                        room->parent()->drones().push(*drone);

                        return scene_pool::alloc<ReadyScene>();
                    }

                }
            }


        }
    }

    if (app.player().key_down(pfrm, Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
        }
    }

    if (app.player().key_down(pfrm, Key::right)) {
        if (cursor_loc->x < island->terrain().size() + 1) {
            ++cursor_loc->x;
        }
    }

    if (app.player().key_down(pfrm, Key::up)) {
        if (cursor_loc->y > 6) {
            --cursor_loc->y;
        }
    }

    if (app.player().key_down(pfrm, Key::down)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
        }
    }


    return null_scene();
}


}
