////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "selInputScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "scriptHookScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/scene/salvageRoomScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void SelInputScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    if (not near_) {
        far_camera();
    }

    text_.emplace( // NOTE: sel-input should have already checked the types of
        // its parameters.
        parameters_->cons().car()->cons().car()->string().value(),
        OverlayCoord{0, 19});


    auto cdr = parameters_->cons().cdr();
    if (cdr->type() == lisp::Value::Type::symbol) {

        auto mt = load_metaclass(cdr->symbol().name());
        if (mt) {
            auto xrange = (*mt)->size().x;
            auto yrange = (*mt)->size().y;

            required_space_ = {(u8)xrange, (u8)yrange};
            w_ot_ = (*mt)->weapon_orientation();
        }
    }


    auto st = calc_screen_tiles();

    for (int i = 0; i < text_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    cached_near_cursor_ = globals().near_cursor_loc_;
    cached_far_cursor_ = globals().far_cursor_loc_;
}



void SelInputScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    text_.reset();

    PLATFORM.fill_overlay(0);
}



lisp::Value* wrap_island(Island* isle);



ScenePtr SelInputScene::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }


    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    flicker_timer_ += delta;
    if (flicker_timer_ > milliseconds(300)) {
        flicker_timer_ -= milliseconds(300);
        flicker_on_ = not flicker_on_;
    }


    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(150));
    };

    APP.player().key_held_distribute();


    if (near_) {

        near_camera();

        auto& cursor_loc = globals().near_cursor_loc_;

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                flicker_timer_ = -milliseconds(150);
                flicker_on_ = false;
                --cursor_loc.x;
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < APP.player_island().terrain().size()) {
                flicker_timer_ = -milliseconds(150);
                flicker_on_ = false;
                ++cursor_loc.x;
            }
        }

        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                flicker_timer_ = -milliseconds(150);
                flicker_on_ = false;
                --cursor_loc.y;
            }
        }

        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                flicker_timer_ = -milliseconds(150);
                flicker_on_ = false;
                ++cursor_loc.y;
            }
        }

    } else {

        if (APP.game_speed() not_eq GameSpeed::stopped) {
            set_gamespeed(GameSpeed::stopped);
        }

        if (not APP.opponent_island()) {
            return make_scene<ReadyScene>();
        }

        far_camera();

        auto& cursor_loc = globals().far_cursor_loc_;

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < APP.opponent_island()->terrain().size()) {
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


    if (near_ and APP.player().key_down(Key::action_2)) {
        if (APP.player_island().get_room(globals().near_cursor_loc_)) {
            auto next = make_scene<SalvageRoomScene>();
            next->set_next_scene(
                make_deferred_scene<SelInputScene>(parameters_.get(), near_));
            return next;
        } else {
            const auto loc = APP.current_world_location();
            auto& node = APP.world_graph().nodes_[loc];
            if (node.type_ == WorldGraph::Node::Type::shop) {
                invoke_hook("on-shop-enter");
                return null_scene();
            } else {
                PLATFORM.speaker().play_sound("beep_error", 1);
            }
        }
    } else if (APP.player().key_down(Key::action_1)) {

        auto& cursor_loc =
            near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

        if (required_space_) {
            for (u8 x = cursor_loc.x; x < cursor_loc.x + required_space_->x;
                 ++x) {
                for (u8 y = 0; y < required_space_->y; ++y) {
                    u8 ry = cursor_loc.y - y;
                    auto room = APP.player_island().get_room({x, ry});
                    if (room or x == APP.player_island().terrain().size() or
                        ry < construction_zone_min_y) {
                        PLATFORM.speaker().play_sound("beep_error", 1);
                        return null_scene();
                    }
                }
            }
        }

        auto isle = near_ ? &APP.player_island() : APP.opponent_island();

        lisp::push_op(wrap_island(isle));
        lisp::push_op(lisp::make_integer(cursor_loc.x));

        int y = cursor_loc.y;
        if (required_space_) {
            y -= (required_space_->y - 1);
        }
        lisp::push_op(lisp::make_integer(y));

        lisp::safecall(parameters_->cons().car()->cons().cdr(), 3);
        lisp::pop_op();

        if (started_near_) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<InspectP2Scene>();
        }
    }

    return null_scene();
}



void draw_required_space(Island& island,
                         const Vec2<Fixnum> origin,
                         const Vec2<u8>& sz,
                         Room::WeaponOrientation o);



void SelInputScene::display()
{
    ActiveWorldScene::display();

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((15 * 2) + cursor_anim_frame_);

    if (near_) {
        auto origin = APP.player_island().visual_origin();

        auto& cursor_loc = globals().near_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        cursor.set_position(origin);
    } else if (APP.opponent_island()) {
        auto origin = APP.opponent_island()->visual_origin();

        auto& cursor_loc = globals().far_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        cursor.set_position(origin);
    }

    PLATFORM.screen().draw(cursor);

    if (required_space_) {
        auto sz = *required_space_;

        auto& cursor_loc = globals().near_cursor_loc_;

        auto rloc = cursor_loc;
        rloc.y -= sz.y - 1;

        for (u8 x = rloc.x; x < rloc.x + sz.x; ++x) {
            for (u8 y = rloc.y; y < rloc.y + sz.y; ++y) {
                if (player_island().get_room({x, y}) or
                    x >= (int)player_island().terrain().size()) {

                    if (flicker_on_) {
                        Sprite spr;
                        spr.set_tidx_16x16(13, 1);
                        spr.set_size(Sprite::Size::w16_h16);
                        auto origin = player_island().visual_origin();
                        origin.x += Fixnum(x * 16);
                        origin.y += Fixnum(y * 16);
                        spr.set_position({origin.x, origin.y});
                        spr.set_mix({custom_color(0xf7ce9e), 250});
                        PLATFORM.screen().draw(spr);
                    }
                }
            }
        }

        auto origin = player_island().visual_origin();
        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);
        origin.y -= Fixnum::from_integer((required_space_->y - 1) * 16);

        draw_required_space(player_island(), origin, sz, w_ot_);
    }
}



} // namespace skyland
