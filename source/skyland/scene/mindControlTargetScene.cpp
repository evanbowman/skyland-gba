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


#include "mindControlTargetScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/mindControl.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



MindControlTargetScene::MindControlTargetScene(const RoomCoord& controller_loc)
    : controller_loc_(controller_loc)
{
}



ScenePtr<Scene> MindControlTargetScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (not APP.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto& cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    APP.player().key_held_distribute();


    if (test_key(Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (cursor_loc.x == 0 and not near_) {
            near_ = true;
            near_camera();
            globals().near_cursor_loc_.x = APP.player_island().terrain().size();
            globals().near_cursor_loc_.y = globals().far_cursor_loc_.y;
        }
    }

    if (test_key(Key::right)) {
        if (cursor_loc.x < APP.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            if (near_ and
                cursor_loc.x == APP.opponent_island()->terrain().size()) {
                far_camera();
                near_ = false;
                globals().far_cursor_loc_.x = 0;
                globals().far_cursor_loc_.y = globals().near_cursor_loc_.y;
            }
        }
    }

    if (test_key(Key::up)) {
        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (test_key(Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }
    }

    if (APP.player().key_down(Key::action_1)) {
        Island* island = APP.opponent_island();
        if (near_) {
            island = &APP.player_island();
        }
        if (not near_ and not island->interior_visible()) {
            PLATFORM.speaker().play_sound("beep_error", 2);
            return null_scene();
        }
        if (auto room = island->get_room(cursor_loc)) {
            if (length(room->characters())) {
                if (auto origin =
                        APP.player_island().get_room(controller_loc_)) {

                    BasicCharacter* ch = nullptr;
                    CharacterId id = 0;
                    for (auto& chr : room->characters()) {
                        if (chr->owner() not_eq &APP.player() and
                            chr->grid_position() == cursor_loc and
                            not chr->mind_controlled()) {
                            id = chr->id();
                            ch = chr.get();
                        }
                    }

                    if (id and ch) {
                        if (auto controller = origin->cast<MindControl>()) {
                            ch->start_mind_control(
                                app, &APP.player(), controller);
                            globals().near_cursor_loc_ = room->position();
                            return scene_pool::alloc<ReadyScene>();
                        }
                    }
                }
            }
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}


void MindControlTargetScene::display()
{
    if (APP.opponent_island()) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_tidx_16x16(17, 0);

        Island* island = APP.opponent_island();
        if (near_) {
            island = &APP.player_island();
        }

        auto origin = island->visual_origin();

        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        cursor.set_position(origin);

        PLATFORM.screen().draw(cursor);
    }

    WorldScene::display();
}



void MindControlTargetScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    far_camera();

    auto st = calc_screen_tiles();
    text_.emplace(SYSTR(mind_control_prompt)->c_str(),
                  OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
    }
}



void MindControlTargetScene::exit(Scene& next)
{
    WorldScene::exit(next);


    auto st = calc_screen_tiles();
    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 1, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 0);
    }

    text_.reset();
}



} // namespace skyland
