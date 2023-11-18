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


#include "replicatorSelectionScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const auto replicator_fee = 700;



void ReplicatorSelectionScene::enter(App& app, Scene& prev)
{
    WorldScene::enter(app, prev);

    if (not near_) {
        far_camera();
    }

    auto st = calc_screen_tiles();
    StringBuffer<30> text(SYSTR(create_replicant)->c_str());
    text += stringify(replicator_fee);
    text += "@";

    text_.emplace(text.c_str(), OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    for (int i = 23; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    persist_ui();
}



void ReplicatorSelectionScene::exit(App& app, Scene& next)
{
    WorldScene::exit(app, next);

    text_.reset();
    yes_text_.reset();
    no_text_.reset();

    const auto st = calc_screen_tiles();
    for (int x = 0; x < st.x; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, st.y - 1, 0);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 2, 0);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 3, 0);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 4, 0);
    }

    if (app.game_mode() == App::GameMode::co_op) {

        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        Island* island = near_ ? &app.player_island() : app.opponent_island();

        if (auto room = island->get_room(cursor_loc)) {
            room->co_op_release_lock();
        }
    }
}



ScenePtr<Scene> ReplicatorSelectionScene::update(App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(app, delta)) {
        return next;
    }

    if (app.coins() < replicator_fee) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (exit_countdown_) {
        exit_countdown_ -= delta;
        if (exit_countdown_ <= 0) {
            return scene_pool::alloc<ReadyScene>();
        }
    } else {
        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        Island* island = near_ ? &app.player_island() : app.opponent_island();

        if (app.player().key_down(Key::action_1)) {
            exit_countdown_ = milliseconds(500);
            if (auto room = island->get_room(cursor_loc)) {
                if (room->create_replicant(app)) {
                    app.set_coins(app.coins() - replicator_fee);
                }
                return scene_pool::alloc<ReadyScene>();
            }
        }
    }

    if (app.player().key_down(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
