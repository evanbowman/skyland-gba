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


#pragma once


#include "constructionScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "worldScene.hpp"



namespace skyland
{



void draw_required_space(Platform& pfrm,
                         const Vec2<Fixnum> origin,
                         const Vec2<u8>& sz);



class MoveRoomScene : public ActiveWorldScene
{
public:
    MoveRoomScene(App& app, bool near)
    {
        bind_island(app, near);
    }


    void bind_island(App& app, bool near)
    {
        if (near) {
            island_ = &player_island(app);
            near_camera();
        } else {
            island_ = opponent_island(app);
            far_camera();
        }
    }


    auto& cursor()
    {
        if (is_far_camera()) {
            return globals().far_cursor_loc_;
        } else {
            return globals().near_cursor_loc_;
        }
    }


    void exit(Platform& pfrm, App& app, Scene& prev) override
    {
        ActiveWorldScene::exit(pfrm, app, prev);

        text_.reset();
        no_text_.reset();
        yes_text_.reset();

        pfrm.fill_overlay(0);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
            return next;
        }

        if (app.game_speed() not_eq GameSpeed::stopped) {
            set_gamespeed(pfrm, app, GameSpeed::stopped);
        }

        switch (state_) {
        case State::setup_prompt: {
            auto st = calc_screen_tiles(pfrm);
            StringBuffer<30> text;
            text += format(SYSTR(move_room_prompt)->c_str(), 800).c_str();

            text_.emplace(pfrm, text.c_str(), OverlayCoord{0, u8(st.y - 1)});

            const int count = st.x - text_->len();
            for (int i = 0; i < count; ++i) {
                pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
            }

            for (int i = 0; i < st.x; ++i) {
                pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
            }

            yes_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
            no_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

            yes_text_->assign(SYSTR(salvage_option_A)->c_str());
            no_text_->assign(SYSTR(salvage_option_B)->c_str());

            for (int i = 23; i < st.x; ++i) {
                pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
            }

            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 3, 128);

            state_ = State::prompt;
            persist_ui();
            break;
        }

        case State::prompt:
            if (player(app).key_down(pfrm, Key::action_2)) {
                if (is_far_camera()) {
                    return scene_pool::alloc<InspectP2Scene>();
                }
                return scene_pool::alloc<ReadyScene>();
            }
            if (player(app).key_down(pfrm, Key::action_1)) {
                if (app.coins() < 800) {
                    auto future_scene =
                        scene_pool::make_deferred_scene<ReadyScene>();
                    auto str = SYSTR(construction_insufficient_funds);
                    pfrm.speaker().play_sound("beep_error", 2);
                    return scene_pool::alloc<NotificationScene>(str->c_str(),
                                                                future_scene);
                }
                unpersist_ui();
                pfrm.speaker().play_sound("coin", 2);
                app.set_coins(pfrm, app.coins() - 800);
                state_ = State::move_stuff;
                yes_text_.reset();
                no_text_.reset();
                text_.reset();
                pfrm.fill_overlay(0);

                auto st = calc_screen_tiles(pfrm);

                text_.emplace(pfrm,
                              SYSTR(move_room_1)->c_str(),
                              OverlayCoord{0, u8(st.y - 1)});

                for (int i = 0; i < text_->len(); ++i) {
                    pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
                }
            }
            break;

        case State::move_stuff:
            if ((player(app).key_pressed(pfrm, Key::start) or
                 player(app).key_pressed(pfrm, Key::action_1)) and
                (player(app).key_down(pfrm, Key::left) or
                 player(app).key_down(pfrm, Key::right) or
                 player(app).key_down(pfrm, Key::up) or
                 player(app).key_down(pfrm, Key::down))) {
                state_ = State::select_group;
                text_.reset();
                pfrm.fill_overlay(0);
                group_selection_ = allocate_dynamic<GroupSelection>("selgroup");
                (*group_selection_)->anchor_ = cursor();
                (*group_selection_)->sel_tl_ = cursor();
                (*group_selection_)->sel_bl_ = cursor();
                (*group_selection_)->sel_tr_ = cursor();
                (*group_selection_)->sel_br_ = cursor();
                break;
            }

            if (player(app).key_down(pfrm, Key::action_2)) {
                return scene_pool::alloc<ReadyScene>();
            }
            if (player(app).key_down(pfrm, Key::action_1)) {
                auto cursor_loc = cursor();
                if (auto r = island_->get_room(cursor_loc)) {
                    if (str_eq(r->name(), "mycelium")) {
                        // Can't be moved!
                        pfrm.speaker().play_sound("beep_error", 3);
                        return null_scene();
                    }
                    state_ = State::move_block;
                    move_src_ = r->position();
                    move_diff_ =
                        (cursor_loc.cast<int>() - move_src_.cast<int>())
                            .cast<u8>();
                    text_.reset();
                    pfrm.fill_overlay(0);

                    mv_size_.x = r->size().x;
                    mv_size_.y = r->size().y;

                    auto st = calc_screen_tiles(pfrm);

                    text_.emplace(pfrm,
                                  SYSTR(move_room_2)->c_str(),
                                  OverlayCoord{0, u8(st.y - 1)});
                    for (int i = 0; i < text_->len(); ++i) {
                        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
                    }
                }
            }
            break;

        case State::move_block:
            if (player(app).key_down(pfrm, Key::action_1) or
                player(app).key_down(pfrm, Key::action_2)) {

                auto cursor_loc = cursor();
                cursor_loc = (cursor_loc.cast<int>() - move_diff_.cast<int>())
                                 .cast<u8>();

                if (player(app).key_down(pfrm, Key::action_1)) {
                    if (auto room = island_->get_room(move_src_)) {
                        for (u32 x = 0; x < room->size().x; ++x) {
                            for (int y = 0; y < room->size().y; ++y) {
                                RoomCoord c;
                                c.x = cursor_loc.x + x;
                                c.y = cursor_loc.y + y;

                                auto err = [&]() {
                                    pfrm.speaker().play_sound("beep_error", 3);
                                    return null_scene();
                                };

                                if (auto d = island_->get_drone(c)) {
                                    return err();
                                }
                                if (c.x >= island_->terrain().size() or
                                    c.y >= 15 or
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
                        pfrm.speaker().play_sound("build0", 4);
                        island_->move_room(pfrm, app, move_src_, cursor_loc);
                    }
                }

                text_.reset();
                pfrm.fill_overlay(0);

                auto st = calc_screen_tiles(pfrm);

                text_.emplace(pfrm,
                              SYSTR(move_room_1)->c_str(),
                              OverlayCoord{0, u8(st.y - 1)});

                for (int i = 0; i < text_->len(); ++i) {
                    pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
                }

                state_ = State::move_stuff;
            }
            break;

        case State::select_group:
            if (not player(app).key_pressed(pfrm, Key::action_1) and
                not player(app).key_pressed(pfrm, Key::start)) {
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
            if (player(app).key_down(pfrm, Key::action_1) or
                player(app).key_down(pfrm, Key::action_2)) {
                text_.reset();
                pfrm.fill_overlay(0);

                if (player(app).key_down(pfrm, Key::action_1)) {
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
                                        pfrm.speaker().play_sound("beep_error",
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

                    pfrm.speaker().play_sound("build0", 4);

                    time_stream::event::MoveRegionBegin e;
                    app.time_stream().push(app.level_timer(), e);

                    for (auto& r : (*group_selection_)->rooms_) {
                        if (auto room = island_->get_room(r)) {
                            auto dest = room->position();
                            dest.x = dest.x - offset_x;
                            dest.y = dest.y - offset_y;
                            island_->move_room(
                                pfrm, app, room->position(), dest);
                            room->set_hidden(true);
                        }
                    }

                    time_stream::event::MoveRegionEnd e2;
                    app.time_stream().push(app.level_timer(), e2);

                    for (auto& room : island_->rooms()) {
                        room->set_hidden(false);
                    }
                }

                auto st = calc_screen_tiles(pfrm);

                text_.emplace(pfrm,
                              SYSTR(move_room_1)->c_str(),
                              OverlayCoord{0, u8(st.y - 1)});

                for (int i = 0; i < text_->len(); ++i) {
                    pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
                }

                group_selection_.reset();
                state_ = State::move_stuff;
                break;
            }
            break;
        }

        auto test_key = [&](Key k) {
            return app.player().test_key(
                pfrm, k, milliseconds(500), milliseconds(100));
        };

        auto& cursor_loc = cursor();

        auto sync_cursor = [&] {
            app.player().network_sync_cursor(
                pfrm, cursor_loc, cursor_anim_frame_, true);
        };

        if ((int)state_ > (int)State::prompt) {
            if (test_key(Key::left)) {

                // if (state_ == State::move_group) {
                //     auto offset_y =
                //         (*group_selection_)->anchor_.y - this->cursor().y;
                //     if (offset_y not_eq 0) {
                //         pfrm.speaker().play_sound("beep_error", 2);
                //         return null_scene();
                //     }
                // }

                if (cursor_loc.x > 0) {
                    --cursor_loc.x;
                    sync_cursor();
                    pfrm.speaker().play_sound("cursor_tick", 0);

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
                //         pfrm.speaker().play_sound("beep_error", 2);
                //         return null_scene();
                //     }
                // }

                if (cursor_loc.x < island_->terrain().size()) {
                    ++cursor_loc.x;
                    sync_cursor();
                    pfrm.speaker().play_sound("cursor_tick", 0);

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
                //         pfrm.speaker().play_sound("beep_error", 2);
                //         return null_scene();
                //     }
                // }

                if (cursor_loc.y > construction_zone_min_y) {
                    --cursor_loc.y;
                    sync_cursor();
                    pfrm.speaker().play_sound("cursor_tick", 0);

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
                //         pfrm.speaker().play_sound("beep_error", 2);
                //         return null_scene();
                //     }
                // }

                if (cursor_loc.y < 14) {
                    ++cursor_loc.y;
                    sync_cursor();
                    pfrm.speaker().play_sound("cursor_tick", 0);

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


    void display(Platform& pfrm, App& app) override
    {
        if ((int)state_ > (int)State::prompt) {
            Sprite cursor;
            cursor.set_size(Sprite::Size::w16_h32);
            if (state_ == State::move_block) {
                cursor.set_texture_index(110);
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


                pfrm.screen().draw(cursor);
            };

            if (state_ == State::select_group or state_ == State::move_group) {
                if (group_selection_) {
                    cursor.set_size(Sprite::Size::w16_h16);
                    cursor.set_texture_index((52 * 2) + cursor_anim_frame_ * 4);
                    draw_cursor((*group_selection_)->sel_tl_, -4, -4);
                    cursor.set_texture_index((52 * 2) + 1 +
                                             cursor_anim_frame_ * 4);
                    draw_cursor((*group_selection_)->sel_bl_, -4, 4);
                    cursor.set_texture_index((52 * 2) + 2 +
                                             cursor_anim_frame_ * 4);
                    draw_cursor((*group_selection_)->sel_tr_, 4, -4);
                    cursor.set_texture_index((52 * 2) + 3 +
                                             cursor_anim_frame_ * 4);
                    draw_cursor((*group_selection_)->sel_br_, 4, 4);
                }
                if (state_ == State::move_group) {
                    cursor.set_texture_index(110 * 2);
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
                            draw_required_space(pfrm, origin, sz);
                        }
                    }
                }
            } else {
                draw_cursor(cursor_loc, 0, 0);
            }
        }

        if (state_ == State::move_block) {

            auto origin = island_->visual_origin();
            auto loc =
                (move_src_.cast<int>() + move_diff_.cast<int>()).cast<u8>();
            origin.x += Fixnum::from_integer(loc.x * 16);
            origin.y += Fixnum::from_integer(loc.y * 16);

            Sprite sprite;
            sprite.set_position(origin);
            sprite.set_texture_index((15 * 2) + cursor_anim_frame_);
            sprite.set_size(Sprite::Size::w16_h16);
            pfrm.screen().draw(sprite);

            sprite.set_size(Sprite::Size::w16_h32);

            origin = island_->visual_origin();
            auto cursor_loc = cursor();
            cursor_loc =
                (cursor_loc.cast<int>() - move_diff_.cast<int>()).cast<u8>();
            origin.x += Fixnum::from_integer(cursor_loc.x * 16);
            origin.y += Fixnum::from_integer(cursor_loc.y * 16);
            auto sz = mv_size_;
            draw_required_space(pfrm, origin, sz);
        }

        if ((int)state_ > (int)State::prompt) {
            auto& cursor_loc = cursor();
            if (auto room = island_->get_room(cursor_loc)) {
                room->display_on_hover(pfrm.screen(), app, cursor_loc);
            }
        }

        ActiveWorldScene::display(pfrm, app);
    }


private:
    enum class State {
        setup_prompt,
        prompt,
        move_stuff,
        move_block,
        select_group,
        move_group,
    } state_ = State::setup_prompt;

    Vec2<u8> move_diff_;
    u8 cursor_anim_frame_ = 0;
    Microseconds cursor_anim_timer_ = 0;

    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;

    RoomCoord move_src_;
    Vec2<u8> mv_size_;

    struct GroupSelection
    {
        RoomCoord sel_tl_;
        RoomCoord sel_bl_;
        RoomCoord sel_tr_;
        RoomCoord sel_br_;

        RoomCoord anchor_;

        Buffer<RoomCoord, 90> rooms_;
    };

    std::optional<DynamicMemory<GroupSelection>> group_selection_;

    Island* island_;
};



} // namespace skyland