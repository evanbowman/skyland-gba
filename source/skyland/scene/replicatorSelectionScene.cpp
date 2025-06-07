////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



void ReplicatorSelectionScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

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



void ReplicatorSelectionScene::exit(Scene& next)
{
    WorldScene::exit(next);

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

    if (APP.game_mode() == App::GameMode::co_op) {

        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        Island* island = near_ ? &APP.player_island() : APP.opponent_island();

        if (auto room = island->get_room(cursor_loc)) {
            room->co_op_release_lock();
        }
    }
}



ScenePtr ReplicatorSelectionScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.coins() < replicator_fee) {
        return make_scene<ReadyScene>();
    }

    if (exit_countdown_) {
        exit_countdown_ -= delta;
        if (exit_countdown_ <= 0) {
            return make_scene<ReadyScene>();
        }
    } else {
        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        Island* island = near_ ? &APP.player_island() : APP.opponent_island();

        if (APP.player().key_down(Key::action_1)) {
            exit_countdown_ = milliseconds(500);
            if (auto room = island->get_room(cursor_loc)) {
                if (room->create_replicant()) {
                    APP.set_coins(APP.coins() - replicator_fee);
                }
                return make_scene<ReadyScene>();
            }
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        return make_scene<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
