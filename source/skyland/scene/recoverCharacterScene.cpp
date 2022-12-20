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


#include "recoverCharacterScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



RecoverCharacterScene::RecoverCharacterScene(const RoomCoord& transporter_loc)
    : transporter_loc_(transporter_loc)
{
}



ScenePtr<Scene>
RecoverCharacterScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto& cursor_loc = globals().far_cursor_loc_;

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);


    if (test_key(Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::right)) {
        if (cursor_loc.x < app.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::up)) {
        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            pfrm.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (auto room = app.opponent_island()->get_room(cursor_loc)) {
            if (length(room->characters())) {
                if (auto origin =
                        app.player_island().get_room(transporter_loc_)) {

                    if (auto transporter = origin->cast<Transporter>()) {
                        transporter->recover_character(pfrm, app, cursor_loc);
                        return scene_pool::alloc<ReadyScene>();
                    }
                }
            }
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }


    return null_scene();
}


void RecoverCharacterScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (not app.opponent_island()) {
        return;
    }

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_tidx_16x16(17, 0);

    auto origin = app.opponent_island()->visual_origin();

    auto& cursor_loc = globals().far_cursor_loc_;

    origin.x += Fixnum::from_integer(cursor_loc.x * 16);
    origin.y += Fixnum::from_integer(cursor_loc.y * 16);

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);
}



void RecoverCharacterScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    far_camera();

    auto st = calc_screen_tiles(pfrm);
    text_.emplace(pfrm,
                  SYSTR(transporter_recover_char)->c_str(),
                  OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }
}



void RecoverCharacterScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);


    auto st = calc_screen_tiles(pfrm);
    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 1, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 0);
    }

    text_.reset();
}



} // namespace skyland
