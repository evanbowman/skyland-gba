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


#include "selInputScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene/salvageRoomScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void SelInputScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    if (not near_) {
        far_camera();
    }

    text_.emplace(pfrm,
                  // NOTE: sel-input should have already checked the types of
                  // its parameters.
                  parameters_->cons().car()->cons().car()->string().value(),
                  OverlayCoord{0, 19});


    auto cdr = parameters_->cons().cdr();
    if (cdr->type() == lisp::Value::Type::cons) {
        auto xrange = cdr->cons().car();
        auto yrange = cdr->cons().cdr();

        if (xrange->type() == lisp::Value::Type::integer and
            yrange->type() == lisp::Value::Type::integer) {
            required_space_ = {(u8)xrange->integer().value_,
                               (u8)yrange->integer().value_};
        }
    }


    auto st = calc_screen_tiles(pfrm);

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    cached_near_cursor_ = globals().near_cursor_loc_;
    cached_far_cursor_ = globals().far_cursor_loc_;
}



void SelInputScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    text_.reset();

    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
SelInputScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }


    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }


    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(150));
    };

    app.player().key_held_distribute(pfrm);


    if (near_) {

        near_camera();

        auto& cursor_loc = globals().near_cursor_loc_;

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < app.player_island().terrain().size()) {
                ++cursor_loc.x;
            } else {
                // std::get<SkylandGlobalData>(globals()).far_cursor_loc_.x = 0;
                // std::get<SkylandGlobalData>(globals()).far_cursor_loc_.y =
                //     cursor_loc.y;
                // near_ = false;
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
            }
        }

    } else {

        if (not app.opponent_island()) {
            near_ = true;
            return null_scene();
        }

        far_camera();

        auto& cursor_loc = globals().far_cursor_loc_;

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
            } else {
                globals().near_cursor_loc_.x =
                    app.player_island().terrain().size();
                globals().near_cursor_loc_.y = cursor_loc.y;
                near_ = true;
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < app.opponent_island()->terrain().size()) {
                ++cursor_loc.x;
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
            }
        }
    }


    if (near_ and app.player().key_down(pfrm, Key::action_2)) {
        auto next = scene_pool::alloc<SalvageRoomScene>();
        next->set_next_scene(scene_pool::make_deferred_scene<SelInputScene>(
            parameters_.get(), near_));
        return next;
    } else if (app.player().key_down(pfrm, Key::action_1)) {

        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;


        if (required_space_) {
            for (u8 x = cursor_loc.x; x < cursor_loc.x + required_space_->x;
                 ++x) {
                for (u8 y = cursor_loc.y; y < cursor_loc.y + required_space_->y;
                     ++y) {
                    auto room = app.player_island().get_room({x, y});
                    if (room or x == app.player_island().terrain().size() or
                        y == 15) {
                        pfrm.speaker().play_sound("beep_error", 1);
                        return null_scene();
                    }
                }
            }
        }

        lisp::push_op(lisp::make_userdata(near_ ? &app.player_island()
                                                : app.opponent_island()));

        lisp::push_op(lisp::make_integer(cursor_loc.x));
        lisp::push_op(lisp::make_integer(cursor_loc.y));

        lisp::funcall(parameters_->cons().car()->cons().cdr(), 3);
        lisp::pop_op(); // TODO: check for lisp::Error.

        globals().near_cursor_loc_ = cached_near_cursor_;
        globals().far_cursor_loc_ = cached_far_cursor_;

        if (started_near_) {
            return scene_pool::alloc<ReadyScene>();
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    }

    return null_scene();
}



void SelInputScene::display(Platform& pfrm, App& app)
{
    ActiveWorldScene::display(pfrm, app);

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    if (near_) {
        auto origin = app.player_island().visual_origin();

        auto& cursor_loc = globals().near_cursor_loc_;

        origin.x += cursor_loc.x * 16;
        origin.y += cursor_loc.y * 16;

        cursor.set_position(origin);
    } else if (app.opponent_island()) {
        auto origin = app.opponent_island()->visual_origin();

        auto& cursor_loc = globals().far_cursor_loc_;

        origin.x += cursor_loc.x * 16;
        origin.y += cursor_loc.y * 16;

        cursor.set_position(origin);
    }

    pfrm.screen().draw(cursor);
}



} // namespace skyland
