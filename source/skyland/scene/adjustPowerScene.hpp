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


#pragma once


#include "constructionScene.hpp"
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

        persist_ui();
    }


    void exit(Scene& next) override
    {
        ActiveWorldScene::exit(next);
        text_.reset();
        PLATFORM.fill_overlay(0);
    }


    Optional<Text> text_;


    ScenePtr<Scene> update(Time delta) override
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
                auto cg = (*room->metaclass())->category();
                if (cg not_eq Room::Category::wall) {
                    if (room->is_powered_down()) {
                        room->set_powerdown(false);
                        PLATFORM.speaker().play_sound("poweron", 4);
                        APP.player_island().schedule_repaint();
                        desc_block();
                    } else if (room->power_usage() > 0) {
                        room->set_powerdown(true);
                        PLATFORM.speaker().play_sound("powerdown", 4);
                        APP.player_island().schedule_repaint();
                        desc_block();
                    } else {
                        PLATFORM.speaker().play_sound("beep_error", 3);
                    }
                } else {
                    PLATFORM.speaker().play_sound("beep_error", 3);
                }
            }
        }

        if (APP.player().key_down(Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
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
