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


#include "replicatorSelectionScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const auto replicator_fee = 900;



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



ScenePtr<Scene> ReplicatorSelectionScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.coins() < replicator_fee) {
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

        Island* island = near_ ? &APP.player_island() : APP.opponent_island();

        if (APP.player().key_down(Key::action_1)) {
            exit_countdown_ = milliseconds(500);
            if (auto room = island->get_room(cursor_loc)) {
                if (room->create_replicant()) {
                    APP.set_coins(APP.coins() - replicator_fee);
                }
                return scene_pool::alloc<ReadyScene>();
            }
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
