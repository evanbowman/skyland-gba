////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "groupSelectionScene.hpp"
#include "constructionScene.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene_pool.hpp"
#include "weaponSetTargetScene.hpp"



namespace skyland
{



GroupSelectionScene::GroupSelectionScene(MetaclassIndex mti)
{
    group_mti_ = mti;
}



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

    if (group_mti_) {
        (*group_selection_)->room_mti_ = group_mti_;
        state_ = State::list_actions;
        text_.reset();
        PLATFORM.fill_overlay(0);

        for (auto& room : APP.player_island().rooms()) {
            if (room->metaclass_index() == *group_mti_) {
                auto pos = room->position();
                (*group_selection_)->rooms_.push_back({pos.x, pos.y});
            }
        }

        cursor_anim_frame_ = false;

        show_action_list();
    }
}



static const auto anim_out_time = milliseconds(200);


ScenePtr GroupSelectionScene::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };


    auto& cursor_loc = globals().near_cursor_loc_;

    switch (state_) {
    case State::draw_selection: {
        if (not APP.player().key_pressed(Key::action_1)) {
            if ((**group_selection_).rooms_.empty()) {
                state_ = State::animate_out;
                break;
            } else {
                state_ = State::list_actions;
                text_.reset();
                PLATFORM.fill_overlay(0);

                cursor_anim_frame_ = false;

                show_action_list();
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
                    if (room->cast_weapon()) {
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
        break;
    }

    case State::animate_out: {
        anim_out_timer_ += milliseconds(16);
        if (anim_out_timer_ > anim_out_time) {
            return make_scene<ReadyScene>();
        }

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < APP.player_island().terrain().size()) {
                ++cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }
        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        }
        break;
    }

    case State::list_actions: {
        if (test_key(Key::action_2)) {
            state_ = State::animate_out;
            break;
        }

        if (test_key(Key::down)) {
            ++action_list_index_;
            if (action_list_index_ == 2) {
                action_list_index_ = 0;
            }
            show_action_list();
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (test_key(Key::up)) {
            if (action_list_index_ == 0) {
                action_list_index_ = 1;
            }
            --action_list_index_;
            show_action_list();
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        if (test_key(Key::action_1)) {
            switch (action_list_index_) {
            case 0:
                if (auto err = reject_if_friendly()) {
                    return err;
                }
                if (APP.player_island().power_supply() <
                    APP.player_island().power_drain()) {
                    auto future_scene = []() {
                        return make_scene<ReadyScene>();
                    };
                    PLATFORM.speaker().play_sound("beep_error", 2);
                    auto str = SYSTR(error_power_out);
                    return make_scene<NotificationScene>(str->c_str(),
                                                         future_scene);
                }
                return make_scene<WeaponSetTargetScene>(**group_selection_);

            case 1: {
                state_ = State::pick_group;

                auto st = calc_screen_tiles();

                for (int x = 0; x < st.x; ++x) {
                    PLATFORM.set_tile(Layer::overlay, x, st.y - 1, 112);
                    PLATFORM.set_tile(Layer::overlay, x, st.y - 2, 425);
                    PLATFORM.set_tile(Layer::overlay, x, st.y - 3, 0);
                }

                auto msg = SYSTR(group_pick_group);
                Text::print(msg->c_str(), OverlayCoord{0, u8(st.y - 1)});

                auto len = utf8::len(msg->c_str());
                PLATFORM.set_tile(Layer::overlay, len, st.y - 1, 395);
                PLATFORM.set_tile(Layer::overlay, len + 1, st.y - 1, 393);
                PLATFORM.set_tile(Layer::overlay, len + 2, st.y - 1, 394);

                break;
            }
            }
        }
        break;
    }

    case State::pick_group: {
        if (test_key(Key::action_2)) {
            state_ = State::animate_out;
            break;
        }

        Optional<Room::Group> group;

        if (test_key(Key::up)) {
            group = Room::Group::one;
        } else if (test_key(Key::left)) {
            group = Room::Group::three;
        } else if (test_key(Key::right)) {
            group = Room::Group::two;
        }

        if (group) {
            for (auto& c : (*group_selection_)->rooms_) {
                if (auto r = APP.player_island().get_room(c)) {
                    if ((*r->metaclass())->category() ==
                        Room::Category::weapon) {
                        auto prev = r->group();

                        time_stream::event::WeaponSetGroup e;
                        e.room_x_ = r->position().x;
                        e.room_y_ = r->position().y;
                        e.prev_group_ = (u8)prev;
                        APP.time_stream().push(APP.level_timer(), e);

                        r->set_group(*group);
                        APP.player_island().schedule_repaint();

                        network::packet::SetWeaponGroup p;
                        p.x_ = r->position().x;
                        p.y_ = r->position().y;
                        p.group_ = (u8)*group;
                        network::transmit(p);
                    }
                }
            }
            PLATFORM.speaker().play_sound("button_wooden", 3);
            state_ = State::animate_out;
            break;
        }

        break;
    }
    }

    return null_scene();
}



void GroupSelectionScene::show_action_list()
{
    auto st = calc_screen_tiles();

    for (int x = 0; x < st.x; ++x) {
        for (int y = 0; y < 2; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, st.y - (y + 1), 112);
        }
        PLATFORM.set_tile(Layer::overlay, x, st.y - 3, 425);
    }

    PLATFORM.set_tile(Layer::overlay, 0, (st.y - 2) + action_list_index_, 475);

    static const auto highlight_colors =
        FontColors{custom_color(0x000010), ColorConstant::aerospace_orange};

    Text::print(SYSTR(group_weapon_target)->c_str(),
                OverlayCoord{1, u8(st.y - 2)},
                action_list_index_ == 0 ? highlight_colors : Text::OptColors{});


    Text::print(SYSTR(group_weapon_group)->c_str(),
                OverlayCoord{1, u8(st.y - 1)},
                action_list_index_ == 1 ? highlight_colors : Text::OptColors{});
}



void GroupSelectionScene::display()
{
    Sprite cursor;

    if (state_ == State::animate_out) {

        auto anchor = globals().near_cursor_loc_;

        Fixnum interval(Float(anim_out_timer_) / anim_out_time);

        auto draw_cursor =
            [&](auto cursor_loc, int x_off, int y_off, Vec2<Fixnum>& last) {
                auto origin = APP.player_island().visual_origin();
                auto anchor_origin = origin;

                origin.x += Fixnum::from_integer(cursor_loc.x * 16 + x_off);
                origin.y += Fixnum::from_integer(cursor_loc.y * 16 + y_off);

                anchor_origin.x += Fixnum::from_integer(anchor.x * 16 + x_off);
                anchor_origin.y += Fixnum::from_integer(anchor.y * 16 + y_off);

                if (last.x == 0.0_fixed or last.y == 0.0_fixed) {
                    last.x = origin.x;
                    last.y = origin.y;
                }

                auto result = origin;
                result.x = interpolate_fp(anchor_origin.x, last.x, interval);
                result.y = interpolate_fp(anchor_origin.y, last.y, interval);

                last.x = result.x;
                last.y = result.y;

                cursor.set_position(result);

                PLATFORM.screen().draw(cursor);
            };

        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_texture_index((52 * 2) + cursor_anim_frame_ * 4);
        draw_cursor((*group_selection_)->sel_tl_, -4, -4, last_draw_pos_[0]);

        cursor.set_texture_index((52 * 2) + 1 + cursor_anim_frame_ * 4);
        draw_cursor((*group_selection_)->sel_bl_, -4, 4, last_draw_pos_[1]);

        cursor.set_texture_index((52 * 2) + 2 + cursor_anim_frame_ * 4);
        draw_cursor((*group_selection_)->sel_tr_, 4, -4, last_draw_pos_[2]);

        cursor.set_texture_index((52 * 2) + 3 + cursor_anim_frame_ * 4);
        draw_cursor((*group_selection_)->sel_br_, 4, 4, last_draw_pos_[3]);

    } else {

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
    }

    ActiveWorldScene::display();
}



} // namespace skyland
