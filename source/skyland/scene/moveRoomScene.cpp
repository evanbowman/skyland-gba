////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "moveRoomScene.hpp"



namespace skyland
{



ScenePtr MoveRoomScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.game_speed() not_eq GameSpeed::stopped) {
        set_gamespeed(GameSpeed::stopped);
    }

    switch (state_) {
    case State::setup_prompt: {
        auto st = calc_screen_tiles();
        StringBuffer<30> text;
        text += format(SYSTR(move_room_prompt)->c_str(), 800).c_str();

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

        PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
        PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

        state_ = State::prompt;
        persist_ui();

        PLATFORM.set_tile(Layer::overlay, 0, st.y - 3, 249);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 3, 250);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 2, 251);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 2, 252);

        PLATFORM.set_tile(Layer::overlay, 2, st.y - 2, 418);
        PLATFORM.set_tile(Layer::overlay, 2, st.y - 3, 433);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 4, 425);
        PLATFORM.set_tile(Layer::overlay, 1, st.y - 4, 425);
        break;
    }

    case State::prompt: {
        if (player().key_down(Key::action_2)) {
            if (is_far_camera()) {
                return make_scene<InspectP2Scene>();
            }
            return make_scene<ReadyScene>();
        }

        const bool skip = not APP.opponent_island();

        if (skip or player().key_down(Key::action_1)) {
            if (not skip and APP.coins() < 800) {
                auto future_scene = make_deferred_scene<ReadyScene>();
                auto str = SYSTR(construction_insufficient_funds);
                PLATFORM.speaker().play_sound("beep_error", 2);
                return make_scene<NotificationScene>(str->c_str(),
                                                     future_scene);
            }
            unpersist_ui();
            if (not skip) {
                PLATFORM.speaker().play_sound("coin", 2);
                APP.set_coins(APP.coins() - 800);
            }
            state_ = State::move_stuff;
            yes_text_.reset();
            no_text_.reset();
            text_.reset();
            PLATFORM.fill_overlay(0);

            auto st = calc_screen_tiles();

            text_.emplace(SYSTR(move_room_1)->c_str(),
                          OverlayCoord{0, u8(st.y - 1)});

            for (int i = 0; i < text_->len(); ++i) {
                PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
            }
        }
        break;
    }

    case State::move_stuff:
        if ((player().key_pressed(Key::start) or
             player().key_pressed(Key::action_1)) and
            (player().key_down(Key::left) or player().key_down(Key::right) or
             player().key_down(Key::up) or player().key_down(Key::down))) {
            state_ = State::select_group;
            text_.reset();
            PLATFORM.fill_overlay(0);
            group_selection_ = allocate_dynamic<GroupSelection>("selgroup");
            (*group_selection_)->anchor_ = cursor();
            (*group_selection_)->sel_tl_ = cursor();
            (*group_selection_)->sel_bl_ = cursor();
            (*group_selection_)->sel_tr_ = cursor();
            (*group_selection_)->sel_br_ = cursor();
            break;
        }

        if (player().key_down(Key::action_2)) {
            return make_scene<ReadyScene>();
        }
        if (player().key_down(Key::action_1)) {
            auto cursor_loc = cursor();
            if (auto r = island_->get_room(cursor_loc)) {
                if (str_eq(r->name(), "mycelium")) {
                    // Can't be moved!
                    PLATFORM.speaker().play_sound("beep_error", 3);
                    return null_scene();
                }
                state_ = State::move_block;
                move_src_ = r->position();
                move_diff_ =
                    (cursor_loc.cast<int>() - move_src_.cast<int>()).cast<u8>();
                text_.reset();
                PLATFORM.fill_overlay(0);

                mv_size_.x = r->size().x;
                mv_size_.y = r->size().y;
                mv_ot_ = (*r->metaclass())->weapon_orientation();

                auto st = calc_screen_tiles();

                text_.emplace(SYSTR(move_room_2)->c_str(),
                              OverlayCoord{0, u8(st.y - 1)});
                for (int i = 0; i < text_->len(); ++i) {
                    PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
                }
            }
        }
        break;

    case State::move_block:
        if (player().key_down(Key::action_1) or
            player().key_down(Key::action_2)) {

            auto cursor_loc = cursor();
            cursor_loc =
                (cursor_loc.cast<int>() - move_diff_.cast<int>()).cast<u8>();

            if (player().key_down(Key::action_1)) {
                if (auto room = island_->get_room(move_src_)) {
                    for (u32 x = 0; x < room->size().x; ++x) {
                        for (int y = 0; y < room->size().y; ++y) {
                            RoomCoord c;
                            c.x = cursor_loc.x + x;
                            c.y = cursor_loc.y + y;

                            auto err = [&]() {
                                PLATFORM.speaker().play_sound("beep_error", 3);
                                return null_scene();
                            };

                            if (auto d = island_->get_drone(c)) {
                                return err();
                            }
                            if (c.x >= island_->terrain().size() or c.y >= 15 or
                                c.y < construction_zone_min_y) {
                                return err();
                            }
                            if (auto o = island_->get_room(c)) {
                                if (o not_eq room) {
                                    return err();
                                }
                            }
                        }
                    }
                    PLATFORM.speaker().play_sound("build0", 4);
                    island_->move_room(move_src_, cursor_loc);
                }
            }

            text_.reset();
            PLATFORM.fill_overlay(0);

            auto st = calc_screen_tiles();

            text_.emplace(SYSTR(move_room_1)->c_str(),
                          OverlayCoord{0, u8(st.y - 1)});

            for (int i = 0; i < text_->len(); ++i) {
                PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
            }

            state_ = State::move_stuff;
        }
        break;

    case State::select_group:
        if (not player().key_pressed(Key::action_1) and
            not player().key_pressed(Key::start)) {
            state_ = State::move_group;
            for (int x = (*group_selection_)->sel_tl_.x;
                 x < (*group_selection_)->sel_tr_.x + 1;
                 ++x) {
                for (int y = (*group_selection_)->sel_tl_.y;
                     y < (*group_selection_)->sel_bl_.y + 1;
                     ++y) {
                    if (auto room = island_->get_room({(u8)x, (u8)y})) {
                        bool found = false;
                        for (auto& r : (*group_selection_)->rooms_) {
                            if (r == room->position()) {
                                found = true;
                                break;
                            }
                        }
                        if (str_eq(room->name(), "mycelium")) {
                            // Can't be moved!
                            PLATFORM.speaker().play_sound("beep_error", 3);
                            auto st = calc_screen_tiles();
                            text_.emplace(SYSTR(move_room_1)->c_str(),
                                          OverlayCoord{0, u8(st.y - 1)});
                            group_selection_.reset();
                            state_ = State::move_stuff;
                            return null_scene();
                        }
                        if (not found) {
                            (*group_selection_)
                                ->rooms_.push_back(room->position());
                        }
                    }
                }
            }

            (*group_selection_)->anchor_ = cursor();
        }
        break;

    case State::move_group:
        if (player().key_down(Key::action_1) or
            player().key_down(Key::action_2)) {
            text_.reset();
            PLATFORM.fill_overlay(0);

            if (player().key_down(Key::action_1)) {
                auto offset_x =
                    (*group_selection_)->anchor_.x - this->cursor().x;
                auto offset_y =
                    (*group_selection_)->anchor_.y - this->cursor().y;

                for (auto& r : (*group_selection_)->rooms_) {
                    if (auto room = island_->get_room(r)) {
                        for (int x = 0; x < room->size().x; ++x) {
                            for (int y = 0; y < room->size().y; ++y) {
                                auto dest = room->position();
                                dest.x = (dest.x - offset_x) + x;
                                dest.y = (dest.y - offset_y) + y;

                                auto err = [&]() {
                                    PLATFORM.speaker().play_sound("beep_error",
                                                                  3);
                                    return null_scene();
                                };

                                if (auto d = island_->get_drone(dest)) {
                                    return err();
                                }
                                if (dest.x >= island_->terrain().size() or
                                    dest.y >= 15 or
                                    dest.y < construction_zone_min_y) {
                                    return err();
                                }


                                if (auto room = island_->get_room(dest)) {
                                    bool found = false;
                                    for (auto& r :
                                         (*group_selection_)->rooms_) {
                                        if (room->position() == r) {
                                            found = true;
                                        }
                                    }

                                    if (not found) {
                                        return err();
                                    }
                                }
                            }
                        }
                    }
                }

                PLATFORM.speaker().play_sound("build0", 4);

                time_stream::event::MoveRegionBegin e;
                APP.time_stream().push(APP.level_timer(), e);

                for (auto& r : (*group_selection_)->rooms_) {
                    if (auto room = island_->get_room(r)) {
                        auto dest = room->position();
                        dest.x = dest.x - offset_x;
                        dest.y = dest.y - offset_y;
                        island_->move_room(room->position(), dest);
                        room->set_hidden(true);
                    }
                }

                time_stream::event::MoveRegionEnd e2;
                APP.time_stream().push(APP.level_timer(), e2);

                for (auto& room : island_->rooms()) {
                    room->set_hidden(false);
                }
            }

            auto st = calc_screen_tiles();

            text_.emplace(SYSTR(move_room_1)->c_str(),
                          OverlayCoord{0, u8(st.y - 1)});

            for (int i = 0; i < text_->len(); ++i) {
                PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
            }

            group_selection_.reset();
            state_ = State::move_stuff;
            break;
        }
        break;
    }

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    auto& cursor_loc = cursor();

    auto sync_cursor = [&] {
        APP.player().network_sync_cursor(cursor_loc, cursor_anim_frame_, true);
    };

    if ((int)state_ > (int)State::prompt) {
        if (test_key(Key::left)) {

            // if (state_ == State::move_group) {
            //     auto offset_y =
            //         (*group_selection_)->anchor_.y - this->cursor().y;
            //     if (offset_y not_eq 0) {
            //         PLATFORM.speaker().play_sound("beep_error", 2);
            //         return null_scene();
            //     }
            // }

            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);

                if (state_ == State::select_group) {
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
            }
        } else if (test_key(Key::right)) {

            // if (state_ == State::move_group) {
            //     auto offset_y =
            //         (*group_selection_)->anchor_.y - this->cursor().y;
            //     if (offset_y not_eq 0) {
            //         PLATFORM.speaker().play_sound("beep_error", 2);
            //         return null_scene();
            //     }
            // }

            if (cursor_loc.x < island_->terrain().size()) {
                ++cursor_loc.x;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);

                if (state_ == State::select_group) {
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
        }

        if (test_key(Key::up)) {

            // if (state_ == State::move_group) {
            //     auto offset_x =
            //         (*group_selection_)->anchor_.x - this->cursor().x;
            //     if (offset_x not_eq 0) {
            //         PLATFORM.speaker().play_sound("beep_error", 2);
            //         return null_scene();
            //     }
            // }

            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);

                if (state_ == State::select_group) {
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
            }
        } else if (test_key(Key::down)) {

            // if (state_ == State::move_group) {
            //     auto offset_x =
            //         (*group_selection_)->anchor_.x - this->cursor().x;
            //     if (offset_x not_eq 0) {
            //         PLATFORM.speaker().play_sound("beep_error", 2);
            //         return null_scene();
            //     }
            // }

            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                sync_cursor();
                PLATFORM.speaker().play_sound("cursor_tick", 0);

                if (state_ == State::select_group) {
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
        }

        cursor_anim_timer_ += delta;
        if (cursor_anim_timer_ > milliseconds(200)) {
            cursor_anim_timer_ -= milliseconds(200);
            cursor_anim_frame_ = not cursor_anim_frame_;
            sync_cursor();
        }
    }
    return null_scene();
}



void MoveRoomScene::display()
{
    if ((int)state_ > (int)State::prompt) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h32);
        if (state_ == State::move_block) {
            cursor.set_size(Sprite::Size::w16_h16);
            cursor.set_tidx_16x16(99, 0);
        } else {
            cursor.set_size(Sprite::Size::w16_h16);
            cursor.set_texture_index((15 * 2) + cursor_anim_frame_);
        }

        auto& cursor_loc = this->cursor();

        auto draw_cursor = [&](auto cursor_loc, int x_off, int y_off) {
            auto origin = island_->visual_origin();


            origin.x += Fixnum::from_integer(cursor_loc.x * 16 + x_off);
            origin.y += Fixnum::from_integer(cursor_loc.y * 16 + y_off);

            cursor.set_position(origin);


            PLATFORM.screen().draw(cursor);
        };

        if (state_ == State::select_group or state_ == State::move_group) {
            if (group_selection_) {
                cursor.set_size(Sprite::Size::w16_h16);
                cursor.set_texture_index((52 * 2) + cursor_anim_frame_ * 4);
                draw_cursor((*group_selection_)->sel_tl_, -4, -4);
                cursor.set_texture_index((52 * 2) + 1 + cursor_anim_frame_ * 4);
                draw_cursor((*group_selection_)->sel_bl_, -4, 4);
                cursor.set_texture_index((52 * 2) + 2 + cursor_anim_frame_ * 4);
                draw_cursor((*group_selection_)->sel_tr_, 4, -4);
                cursor.set_texture_index((52 * 2) + 3 + cursor_anim_frame_ * 4);
                draw_cursor((*group_selection_)->sel_br_, 4, 4);
            }
            if (state_ == State::move_group) {
                cursor.set_tidx_16x16(99, 0);
                draw_cursor(cursor_loc, 0, 0);

                auto offset_x =
                    (*group_selection_)->anchor_.x - this->cursor().x;
                auto offset_y =
                    (*group_selection_)->anchor_.y - this->cursor().y;

                for (auto& r : (*group_selection_)->rooms_) {
                    if (auto room = island_->get_room(r)) {
                        auto sz = room->size();
                        auto pos = room->position();
                        int x = pos.x - offset_x;
                        int y = pos.y - offset_y;
                        auto origin = island_->visual_origin();
                        origin.x += Fixnum::from_integer(x * 16);
                        origin.y += Fixnum::from_integer(y * 16);
                        auto o = (*room->metaclass())->weapon_orientation();
                        draw_required_space(*island_, origin, sz, o);
                    }
                }
            }
        } else {
            draw_cursor(cursor_loc, 0, 0);
        }
    }

    if (state_ == State::move_block) {

        auto origin = island_->visual_origin();
        auto loc = (move_src_.cast<int>() + move_diff_.cast<int>()).cast<u8>();
        origin.x += Fixnum::from_integer(loc.x * 16);
        origin.y += Fixnum::from_integer(loc.y * 16);

        Sprite sprite;
        sprite.set_position(origin);
        sprite.set_texture_index((15 * 2) + cursor_anim_frame_);
        sprite.set_size(Sprite::Size::w16_h16);
        PLATFORM.screen().draw(sprite);

        sprite.set_size(Sprite::Size::w16_h32);

        origin = island_->visual_origin();
        auto cursor_loc = cursor();
        cursor_loc =
            (cursor_loc.cast<int>() - move_diff_.cast<int>()).cast<u8>();
        origin.x += Fixnum::from_integer(cursor_loc.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);
        auto sz = mv_size_;
        draw_required_space(*island_, origin, sz, mv_ot_);
    }

    if ((int)state_ > (int)State::prompt) {
        auto& cursor_loc = cursor();
        if (auto room = island_->get_room(cursor_loc)) {
            room->display_on_hover(PLATFORM.screen(), cursor_loc);
        }
    }

    ActiveWorldScene::display();
}



} // namespace skyland
