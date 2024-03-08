////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



TransportCharacterScene::TransportCharacterScene(RoomCoord origin)
    : NotificationScene(SYSTR(transporter_transport_char)->c_str(),
                        [] { return make_scene<ReadyScene>(); }),
      origin_(origin)
{
}



void TransportCharacterScene::enter(Scene& prev)
{
    NotificationScene::enter(prev);

    far_camera();

    if (not APP.opponent_island()) {
        return;
    }

    matrix_ = allocate_dynamic<bool[16][16]>("chr-transport-matrix");

    APP.opponent_island()->plot_walkable_zones(**matrix_, nullptr);

    bool set_cursor = false;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if ((**matrix_)[x][y]) {
                if (not set_cursor) {
                    globals().far_cursor_loc_ = {x, y};
                    set_cursor = true;
                }
                PLATFORM.set_tile(APP.opponent_island()->layer(),
                                  x,
                                  y,
                                  StaticTile::path_marker);
            }
        }
    }
}



void TransportCharacterScene::exit(Scene& next)
{
    NotificationScene::exit(next);

    if (APP.opponent_island()) {
        APP.opponent_island()->repaint();
    }
}



void TransportCharacterScene::display()
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

    Vec2<Fixnum> origin;
    if (APP.opponent_island()) {
        origin = APP.opponent_island()->visual_origin();
    }

    const auto cursor_loc = globals().far_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);

    WorldScene::display();
}



ScenePtr TransportCharacterScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.player().key_down(Key::action_2)) {
        return make_scene<ReadyScene>();
    }

    if (not APP.opponent_island()) {
        return make_scene<ReadyScene>();
    }

    RoomCoord* cursor_loc = nullptr;
    cursor_loc = &globals().far_cursor_loc_;

    if (APP.player().key_down(Key::left)) {
        if (cursor_loc->x > 0) {
            --cursor_loc->x;
        }
    }

    if (APP.player().key_down(Key::right)) {
        if (cursor_loc->x < APP.opponent_island()->terrain().size()) {
            ++cursor_loc->x;
        }
    }

    if (APP.player().key_down(Key::up)) {
        if (cursor_loc->y > construction_zone_min_y) {
            --cursor_loc->y;
        }
    }

    if (APP.player().key_down(Key::down)) {
        if (cursor_loc->y < 14) {
            ++cursor_loc->y;
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    if (APP.player().key_down(Key::action_1) and
        (**matrix_)[cursor_loc->x][cursor_loc->y]) {

        for (auto& room : APP.opponent_island()->rooms()) {
            for (auto& other : room->characters()) {
                if (other->owner() == &APP.player()) {

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

        if (auto room = APP.player_island().get_room(origin_)) {
            if (auto transporter = room->cast<Transporter>()) {
                transporter->transport_occupant(*cursor_loc);
                return make_scene<InspectP2Scene>();
            } else {
                return make_scene<ReadyScene>();
            }
        }
    }

    return null_scene();
}



} // namespace skyland
