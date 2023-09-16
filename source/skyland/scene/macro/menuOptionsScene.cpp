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
    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        pfrm.fill_overlay(112);
    }


    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        pfrm.fill_overlay(0);
    }


    void display(Platform& pfrm, App& app) override
    {
        int circ_center_x = pfrm.screen().size().x / 2;
        int circ_center_y = pfrm.screen().size().y / 2;
        int params[] = {circ_radius_, circ_center_x, circ_center_y};
        pfrm.system_call("iris-wipe-effect", params);
    }


    ScenePtr<Scene> update(Platform& pfrm, App&, Microseconds delta) override
    {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(400);
        if (timer_ > fade_duration) {
            pfrm.screen().schedule_fade(1);
            circ_radius_ = 0;
            pfrm.fill_overlay(0);
            return scene_pool::alloc<MacroverseScene>(true);
        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);
            circ_radius_ = 144 - int(144 * amount);
            if (timer_ > delta) {
                amount *= 0.75f;
            }
            pfrm.screen().schedule_fade(amount);
        }

        return null_scene();
    }


private:
    Microseconds timer_ = 0;
    int circ_radius_ = 0;
};



void MenuOptionsScene::enter(Platform& pfrm,
                             macro::EngineImpl& state,
                             Scene& prev)
{
    if (auto m = prev.cast_macrocosm_scene()) {
        m->drop_ui();
    }

    pfrm.set_tile(Layer::overlay, 1, 1, 393);
    pfrm.set_tile(Layer::overlay, 1, 2, 394);

    StringBuffer<32> mv(":");
    mv += SYSTR(start_menu_macroverse)->c_str();

    macroverse_text_.emplace(pfrm, mv.c_str(), OverlayCoord{2, 1});
    next_turn_text_.emplace(
        pfrm, SYSTR(macro_next_turn)->c_str(), OverlayCoord{2, 2});

    harvest_text_.emplace(pfrm, OverlayCoord{1, 3});
    harvest_text_->assign(
        "a", FontColors{custom_color(0xa3c447), ColorConstant::rich_black});
    harvest_text_->append(SYSTR(macro_check_harvest)->c_str());
}



void MenuOptionsScene::exit(Platform& pfrm,
                            macro::EngineImpl& state,
                            Scene& next)
{

    MacrocosmScene::exit(pfrm, state, next);

    macroverse_text_.reset();
    next_turn_text_.reset();
    harvest_text_.reset();
    message_text_.reset();

    pfrm.set_tile(Layer::overlay, 1, 1, 0);
    pfrm.set_tile(Layer::overlay, 1, 2, 0);
}



ScenePtr<Scene> MenuOptionsScene::update(Platform& pfrm,
                                         Player& player,
                                         macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    ++frames_;

    if (exit_timer_) {
        // Show the menu for at least a few frames, as an indication to players
        // that they need to hold down the button.
        ++exit_timer_;
        if (exit_timer_ > 6) {
            return scene_pool::alloc<SelectorScene>();
        } else {
            return null_scene();
        }
    }

    if (player.key_pressed(pfrm, Key::alt_1) or
        player.key_pressed(pfrm, Key::alt_2)) {

        if (player.key_down(pfrm, Key::action_1)) {
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
                            pfrm.speaker().play_sound("click_digital_1", 3);
                            return scene_pool::alloc<SelectorScene>();
                        }
                    }
                }
            }
            pfrm.speaker().play_sound("cancel", 3);
            message_text_.emplace(pfrm, OverlayCoord{0, 19});
            message_text_->assign(SYS_CSTR(macro_harvest_not_ready));
        }

        if (player.key_down(pfrm, Key::right)) {
            pfrm.speaker().play_sound("cursor_tick", 0);
            return scene_pool::alloc<NextTurnScene>();
        }

        if (player.key_down(pfrm, Key::up)) {
            pfrm.speaker().play_sound("cursor_tick", 0);
            pfrm.fill_overlay(0);
            return scene_pool::alloc<ExitIslandScene>();
        }

    } else {
        if (frames_ > 15) {
            return scene_pool::alloc<SelectorScene>();
        } else {
            exit_timer_ = 1;
        }
    }

    return null_scene();
}



} // namespace skyland::macro
