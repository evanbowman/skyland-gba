////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "constructionScene.hpp"
#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



class AdjustPowerScene : public ActiveWorldScene
{
public:
    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);

        APP.player_island().show_powerdown_opts(true);

        persist_ui();
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);

        APP.player_island().show_powerdown_opts(false);
        text_.reset();
        PLATFORM.fill_overlay(0);
    }


    Optional<Text> text_;


    ScenePtr update(Time delta) override
    {
        if (auto new_scene = ActiveWorldScene::update(delta)) {
            return new_scene;
        }

        auto& cursor_loc = globals().near_cursor_loc_;

        auto test_key = [&](Key k) {
            return APP.player().test_key(
                k, milliseconds(500), milliseconds(100));
        };

        APP.player().key_held_distribute();

        auto desc_block = [&] {
            for (int x = 0; x < calc_screen_tiles().x; ++x) {
                PLATFORM.set_tile(
                    Layer::overlay, x, calc_screen_tiles().y - 2, 0);
            }
            if (auto room = APP.player_island().get_room(cursor_loc)) {
                text_.emplace(OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
                text_->assign("(");
                auto metac = room->metaclass();
                text_->append((*metac)->ui_name()->c_str());
                text_->append("): ");
                if (room->is_powered_down()) {
                    text_->append("0/");
                    text_->append((*metac)->consumes_power());
                } else {
                    text_->append(room->power_usage());
                    text_->append("/");
                    text_->append(room->power_usage());
                }
                text_->append("`");

                for (int i = 0; i < text_->len(); ++i) {
                    PLATFORM.set_tile(
                        Layer::overlay, i, calc_screen_tiles().y - 2, 425);
                }

            } else {
                text_.reset();
            }
        };


        if (not text_) {
            desc_block();
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < APP.player_island().terrain().size()) {
                ++cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                desc_block();
            }
        }
        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                desc_block();
            }
        }
        if (test_key(Key::up)) {
            if (cursor_loc.y > construction_zone_min_y) {
                --cursor_loc.y;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                desc_block();
            }
        }
        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                desc_block();
            }
        }

        if (APP.player().key_down(Key::action_1)) {
            if (auto room = APP.player_island().get_room(cursor_loc)) {
                if (room->allows_powerdown()) {
                    if (room->is_powered_down()) {
                        room->set_powerdown(false);
                        PLATFORM.speaker().play_sound("poweron.raw", 4);
                        APP.player_island().schedule_repaint();
                        desc_block();
                    } else if (room->power_usage() > 0) {
                        room->set_powerdown(true);
                        PLATFORM.speaker().play_sound("powerdown.raw", 4);
                        APP.player_island().schedule_repaint();
                        desc_block();
                    } else {
                        PLATFORM.speaker().play_sound("beep_error", 3);
                    }
                } else {
                    PLATFORM.speaker().play_sound("beep_error", 3);
                    auto next = make_deferred_scene<AdjustPowerScene>();
                    auto err = SYSTR(error_cannot_divert_power);
                    return make_scene<NotificationScene>(err->c_str(), next);
                }
            }
        }

        if (APP.player().key_down(Key::action_2)) {
            return make_scene<ReadyScene>();
        }

        return null_scene();
    }


    void display() override
    {
        WorldScene::display();

        auto origin = APP.player_island().visual_origin();

        auto& cursor_loc = globals().near_cursor_loc_;

        origin.x += Fixnum::from_integer(cursor_loc.x * 16 + 3);
        origin.y += Fixnum::from_integer(cursor_loc.y * 16);

        Sprite sprite;
        sprite.set_position(origin);
        sprite.set_priority(0);
        sprite.set_texture_index(62);
        sprite.set_size(Sprite::Size::w16_h32);

        sprite.set_flip({true, false});

        PLATFORM.screen().draw(sprite);
    }
};



} // namespace skyland
