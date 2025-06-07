////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



#include "menuOptionsScene.hpp"
#include "macroverseScene.hpp"
#include "nextTurnScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



class ExitIslandScene : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.fill_overlay(112);
    }


    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
    }


    void display() override
    {
        int circ_center_x = PLATFORM.screen().size().x / 2;
        int circ_center_y = PLATFORM.screen().size().y / 2;
        PLATFORM_EXTENSION(
            iris_wipe_effect, circ_radius_, circ_center_x, circ_center_y);
    }


    ScenePtr update(Time delta) override
    {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(400);
        if (timer_ > fade_duration) {
            PLATFORM.screen().schedule_fade(1);
            circ_radius_ = 0;
            PLATFORM.fill_overlay(0);
            return make_scene<MacroverseScene>(true);
        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);
            circ_radius_ = 144 - int(144 * amount);
            if (timer_ > delta) {
                amount *= 0.75f;
            }
            PLATFORM.screen().schedule_fade(amount);
        }

        return null_scene();
    }


private:
    Time timer_ = 0;
    int circ_radius_ = 0;
};



void MenuOptionsScene::enter(macro::EngineImpl& state, Scene& prev)
{
    if (auto m = prev.cast_macrocosm_scene()) {
        m->drop_ui();
    }

    PLATFORM.set_tile(Layer::overlay, 1, 1, 393);
    PLATFORM.set_tile(Layer::overlay, 1, 2, 394);

    StringBuffer<32> mv(":");
    mv += SYSTR(start_menu_macroverse)->c_str();

    macroverse_text_.emplace(mv.c_str(), OverlayCoord{2, 1});
    next_turn_text_.emplace(SYSTR(macro_next_turn)->c_str(),
                            OverlayCoord{2, 2});

    harvest_text_.emplace(OverlayCoord{1, 3});
    harvest_text_->assign(
        "a", FontColors{custom_color(0xa3c447), ColorConstant::rich_black});
    harvest_text_->append(SYSTR(macro_check_harvest)->c_str());
}



void MenuOptionsScene::exit(macro::EngineImpl& state, Scene& next)
{

    MacrocosmScene::exit(state, next);

    macroverse_text_.reset();
    next_turn_text_.reset();
    harvest_text_.reset();
    message_text_.reset();

    PLATFORM.set_tile(Layer::overlay, 1, 1, 0);
    PLATFORM.set_tile(Layer::overlay, 1, 2, 0);
}



ScenePtr MenuOptionsScene::update(Player& player, macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(player, state)) {
        return scene;
    }

    ++frames_;

    if (exit_timer_) {
        // Show the menu for at least a few frames, as an indication to players
        // that they need to hold down the button.
        ++exit_timer_;
        if (exit_timer_ > 6) {
            return make_scene<SelectorScene>();
        } else {
            return null_scene();
        }
    }

    if (player.key_pressed(Key::alt_1) or player.key_pressed(Key::alt_2)) {

        if (player.key_down(Key::action_1)) {
            auto& s = state.sector();

            for (u8 z = 0; z < s.size().z; ++z) {
                for (u8 x = 0; x < s.size().x; ++x) {
                    for (u8 y = 0; y < s.size().y; ++y) {
                        auto tp = s.get_block({x, y, z}).type();
                        if (tp == terrain::Type::potatoes or
                            tp == terrain::Type::wheat_ripe or
                            tp == terrain::Type::rice_ripe) {
                            for (u8 zz = z + 1; zz < s.size().z; ++zz) {
                                auto tp = s.get_block({x, y, zz}).type();
                                if (tp not_eq terrain::Type::air and
                                    tp not_eq terrain::Type::selector) {
                                    s.set_z_view(zz + 1);
                                    break;
                                }
                            }
                            s.set_cursor({x, y, u8(z + 1)});
                            PLATFORM.speaker().play_sound("click_digital_1", 3);
                            return make_scene<SelectorScene>();
                        }
                    }
                }
            }
            PLATFORM.speaker().play_sound("cancel", 3);
            message_text_.emplace(OverlayCoord{0, 19});
            message_text_->assign(SYS_CSTR(macro_harvest_not_ready));
        }

        if (player.key_down(Key::right)) {
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            return make_scene<NextTurnScene>();
        }

        if (player.key_down(Key::up)) {
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            PLATFORM.fill_overlay(0);
            return make_scene<ExitIslandScene>();
        }

    } else {
        if (frames_ > 15) {
            return make_scene<SelectorScene>();
        } else {
            exit_timer_ = 1;
        }
    }

    return null_scene();
}



} // namespace skyland::macro
