////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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

#include "groupSelectionScene.hpp"
#include "constructionScene.hpp"
#include "skyland/scene_pool.hpp"
#include "readyScene.hpp"
#include "weaponSetTargetScene.hpp"




namespace skyland
{



void GroupSelectionScene::exit(Scene& next)
{
    text_.reset();
    PLATFORM.fill_overlay(0);
}



void GroupSelectionScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    group_selection_ = allocate_dynamic<GroupSelection>("selgroup");

    auto cursor = globals().near_cursor_loc_;

    (*group_selection_)->anchor_ = cursor;
    (*group_selection_)->sel_tl_ = cursor;
    (*group_selection_)->sel_bl_ = cursor;
    (*group_selection_)->sel_tr_ = cursor;
    (*group_selection_)->sel_br_ = cursor;

    auto st = calc_screen_tiles();

    text_.emplace(SYSTR(select_rooms_prompt)->c_str(),
                  OverlayCoord{0, u8(st.y - 1)});

    for (int i = 0; i < text_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

}



ScenePtr GroupSelectionScene::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };


    auto& cursor_loc = globals().near_cursor_loc_;

    if (not APP.player().key_pressed(Key::action_1)) {
        if ((**group_selection_).rooms_.empty()) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<WeaponSetTargetScene>(**group_selection_);
        }
    }

    if (test_key(Key::left) or left_qd_) {
        left_qd_ = false;

        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);

            auto anchor = (*group_selection_)->anchor_;
            if (cursor_loc.x < anchor.x) {
                (*group_selection_)->sel_tl_.x -= 1;
                (*group_selection_)->sel_bl_.x -= 1;
                (*group_selection_)->sel_tr_.x = anchor.x;
                (*group_selection_)->sel_br_.x = anchor.x;
            } else if (cursor_loc.x == anchor.x) {
                (*group_selection_)->sel_tl_.x = anchor.x;
                (*group_selection_)->sel_bl_.x = anchor.x;
                (*group_selection_)->sel_tr_.x = anchor.x;
                (*group_selection_)->sel_br_.x = anchor.x;
            } else {
                (*group_selection_)->sel_tl_.x = anchor.x;
                (*group_selection_)->sel_bl_.x = anchor.x;
                (*group_selection_)->sel_tr_.x -= 1;
                (*group_selection_)->sel_br_.x -= 1;
            }
        }
    } else if (test_key(Key::right) or right_qd_) {
        right_qd_ = false;

        if (cursor_loc.x < APP.player_island().terrain().size()) {
            ++cursor_loc.x;
            PLATFORM.speaker().play_sound("cursor_tick", 0);

            auto anchor = (*group_selection_)->anchor_;
            if (cursor_loc.x < anchor.x) {
                (*group_selection_)->sel_tl_.x += 1;
                (*group_selection_)->sel_bl_.x += 1;
                (*group_selection_)->sel_tr_.x = anchor.x;
                (*group_selection_)->sel_br_.x = anchor.x;
            } else if (cursor_loc.x == anchor.x) {
                (*group_selection_)->sel_tl_.x = anchor.x;
                (*group_selection_)->sel_bl_.x = anchor.x;
                (*group_selection_)->sel_tr_.x = anchor.x;
                (*group_selection_)->sel_br_.x = anchor.x;
            } else {
                (*group_selection_)->sel_tl_.x = anchor.x;
                (*group_selection_)->sel_bl_.x = anchor.x;
                (*group_selection_)->sel_tr_.x += 1;
                (*group_selection_)->sel_br_.x += 1;
            }
        }
    }

    if (test_key(Key::up) or up_qd_) {
        up_qd_ = false;

        if (cursor_loc.y > construction_zone_min_y) {
            --cursor_loc.y;
            PLATFORM.speaker().play_sound("cursor_tick", 0);

            auto anchor = (*group_selection_)->anchor_;
            if (cursor_loc.y < anchor.y) {
                (*group_selection_)->sel_tl_.y -= 1;
                (*group_selection_)->sel_bl_.y = anchor.y;
                (*group_selection_)->sel_tr_.y -= 1;
                (*group_selection_)->sel_br_.y = anchor.y;
            } else if (cursor_loc.y == anchor.y) {
                (*group_selection_)->sel_tl_.y = anchor.y;
                (*group_selection_)->sel_bl_.y = anchor.y;
                (*group_selection_)->sel_tr_.y = anchor.y;
                (*group_selection_)->sel_br_.y = anchor.y;
            } else {
                (*group_selection_)->sel_tl_.y = anchor.y;
                (*group_selection_)->sel_bl_.y -= 1;
                (*group_selection_)->sel_tr_.y = anchor.y;
                (*group_selection_)->sel_br_.y -= 1;
            }
        }
    } else if (test_key(Key::down) or down_qd_) {
        down_qd_ = false;

        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            PLATFORM.speaker().play_sound("cursor_tick", 0);

            auto anchor = (*group_selection_)->anchor_;
            if (cursor_loc.y < anchor.y) {
                (*group_selection_)->sel_tl_.y += 1;
                (*group_selection_)->sel_bl_.y = anchor.y;
                (*group_selection_)->sel_tr_.y += 1;
                (*group_selection_)->sel_br_.y = anchor.y;
            } else if (cursor_loc.y == anchor.y) {
                (*group_selection_)->sel_tl_.y = anchor.y;
                (*group_selection_)->sel_bl_.y = anchor.y;
                (*group_selection_)->sel_tr_.y = anchor.y;
                (*group_selection_)->sel_br_.y = anchor.y;
            } else {
                (*group_selection_)->sel_tl_.y = anchor.y;
                (*group_selection_)->sel_bl_.y += 1;
                (*group_selection_)->sel_tr_.y = anchor.y;
                (*group_selection_)->sel_br_.y += 1;
            }
        }
    }

    (*group_selection_)->rooms_.clear();
    auto tl = (*group_selection_)->sel_tl_;
    auto br = (*group_selection_)->sel_br_;

    for (u8 x = tl.x; x < br.x + 1; ++x) {
        for (u8 y = tl.y; y < br.y + 1; ++y) {
            if (auto room = APP.player_island().get_room({x, y})) {
                const auto cg = (*room->metaclass())->category();
                if (cg == Room::Category::weapon) {
                    (*group_selection_)->rooms_.push_back({x, y});
                }
            }
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    return null_scene();
}



void GroupSelectionScene::display()
{
    Sprite cursor;

    auto draw_cursor = [&](auto cursor_loc, int x_off, int y_off) {
        auto origin = APP.player_island().visual_origin();

        origin.x += Fixnum::from_integer(cursor_loc.x * 16 + x_off);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16 + y_off);

        cursor.set_position(origin);


        PLATFORM.screen().draw(cursor);
    };

    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_texture_index((52 * 2) + cursor_anim_frame_ * 4);
    draw_cursor((*group_selection_)->sel_tl_, -4, -4);
    cursor.set_texture_index((52 * 2) + 1 + cursor_anim_frame_ * 4);
    draw_cursor((*group_selection_)->sel_bl_, -4, 4);
    cursor.set_texture_index((52 * 2) + 2 + cursor_anim_frame_ * 4);
    draw_cursor((*group_selection_)->sel_tr_, 4, -4);
    cursor.set_texture_index((52 * 2) + 3 + cursor_anim_frame_ * 4);
    draw_cursor((*group_selection_)->sel_br_, 4, 4);

    Sprite sel;
    sel.set_size(Sprite::Size::w16_h16);
    sel.set_tidx_16x16(13, 0);

    for (auto& c : (*group_selection_)->rooms_) {
        auto origin = APP.player_island().visual_origin();

        origin.x += Fixnum::from_integer(c.x * 16);
        origin.y += Fixnum::from_integer(c.y * 16);

        sel.set_position(origin);

        PLATFORM.screen().draw(sel);
    }

    ActiveWorldScene::display();
}



}
