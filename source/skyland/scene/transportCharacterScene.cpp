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


#include "transportCharacterScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



TransportCharacterScene::TransportCharacterScene(Platform& pfrm,
                                                 RoomCoord origin)
    : NotificationScene(SYSTR(transporter_transport_char)->c_str(),
                        [] { return scene_pool::alloc<ReadyScene>(); }),
      origin_(origin)
{
}



void TransportCharacterScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    NotificationScene::enter(pfrm, app, prev);

    far_camera();

    if (not app.opponent_island()) {
        return;
    }

    matrix_ = allocate_dynamic<bool[16][16]>("chr-transport-matrix");

    app.opponent_island()->plot_walkable_zones(app, **matrix_);

    bool set_cursor = false;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if ((**matrix_)[x][y]) {
                if (not set_cursor) {
                    globals().far_cursor_loc_ = {
                        x, y};
                    set_cursor = true;
                }
                pfrm.set_tile(app.opponent_island()->layer(),
                              x,
                              y,
                              StaticTile::path_marker);
            }
        }
    }
}



void TransportCharacterScene::exit(Platform& pfrm, App& app, Scene& next)
{
    NotificationScene::exit(pfrm, app, next);

    if (app.opponent_island()) {
        app.opponent_island()->repaint(pfrm, app);
    }
}



void TransportCharacterScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    Vec2<Fixnum> origin;
    if (app.opponent_island()) {
        origin = app.opponent_island()->visual_origin();
    }

    const auto cursor_loc =
        globals().far_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);

    WorldScene::display(pfrm, app);
}



ScenePtr<Scene>
TransportCharacterScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    RoomCoord* cursor_loc = nullptr;
    cursor_loc = &globals().far_cursor_loc_;

    if (app.player().key_down(pfrm, Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
        }
    }

    if (app.player().key_down(pfrm, Key::right)) {
        if (cursor_loc->x < app.opponent_island()->terrain().size()) {
            ++cursor_loc->x;
        }
    }

    if (app.player().key_down(pfrm, Key::up)) {
        if (cursor_loc->y > construction_zone_min_y) {
            --cursor_loc->y;
        }
    }

    if (app.player().key_down(pfrm, Key::down)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    if (app.player().key_down(pfrm, Key::action_1) and
        (**matrix_)[cursor_loc->x][cursor_loc->y]) {

        for (auto& room : app.opponent_island()->rooms()) {
            for (auto& other : room->characters()) {
                if (other->owner() == &app.player()) {

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

        if (auto room = app.player_island().get_room(origin_)) {
            if (auto transporter = room->cast<Transporter>()) {
                transporter->transport_occupant(pfrm, app, *cursor_loc);
                return scene_pool::alloc<InspectP2Scene>();
            } else {
                return scene_pool::alloc<ReadyScene>();
            }
        }
    }

    return null_scene();
}



} // namespace skyland
