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


#include "macroverseScene.hpp"
#include "platform/color.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/startMenuScene.hpp"



namespace skyland::macro
{



void MacroverseScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (not app.macrocosm()) {
        pfrm.fatal("logic error while entering macroverse!");
    }

    pfrm.load_sprite_texture("spritesheet_macroverse");

    pfrm.screen().schedule_fade(0.f);
    pfrm.screen().schedule_fade(1.f, ColorConstant::rich_black, false);

    pfrm.screen().set_view({});

    // Clear the background texture.
    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");
}



void MacroverseScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    auto& sector = app.macrocosm()->sector();
    sector.rotate();
    sector.rotate();
    sector.rotate();
    sector.rotate(); // hack to force repaint.

    pfrm.screen().set_shader(passthrough_shader);

    pfrm.fill_overlay(0);

    pfrm.load_overlay_texture("overlay");
    pfrm.load_sprite_texture("spritesheet");
}



    static const auto reveal_time = milliseconds(150);



ScenePtr<Scene>
MacroverseScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.macrocosm()->data_->cloud_scroll_ += 0.000001f * delta;

    switch (state_) {
    case State::reveal: {
        timer_ += delta;
        if (timer_ > reveal_time) {
            timer_ = 0;
            state_ = State::wait;

            pfrm.screen().set_shader([](ShaderPalette p,
                                        ColorConstant k,
                                        int var,
                                        int index) -> ColorConstant {

                auto blend = [&](auto sc, auto d) {
                    const Color dst(custom_color(sc));
                    const Color src(custom_color(d));

                    Color result(fast_interpolate(dst.r_, src.r_, (u8)var),
                                 fast_interpolate(dst.g_, src.g_, (u8)var),
                                 fast_interpolate(dst.b_, src.b_, (u8)var));

                    return result.hex();
                };

                if (p == ShaderPalette::spritesheet) {
                    if (index == 3) {
                        return blend(0xffffff, 0xcec6ef);
                    } else if (index == 5) {
                        return blend(0xffa409, 0xe24920);
                    }
                }

                return k;
            });
        }
        break;
    }

    case State::wait:
        timer_ += delta;
        if (timer_ > milliseconds(400)) {
            timer_ = 0;
            state_ = State::fade_in;
        }
        break;

    case State::fade_in: {
        timer_ += delta;
        auto tm = milliseconds(500);
        if (timer_ > tm) {
            timer_ = 0;
            state_ = State::show;
            describe_selected(pfrm, *app.macrocosm());

        } else {
            const auto step = smoothstep(0.f, tm, timer_);
            const auto amount = 1.f - 0.6f * step;
            pfrm.screen().set_shader_argument(step * 255);
            pfrm.screen().schedule_fade(
                amount, ColorConstant::rich_black, false);
        }
        break;
    }

    case State::show:
        if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<StartMenuScene>(1);
        }
        break;
    }

    return null_scene();
}



void MacroverseScene::describe_selected(Platform& pfrm, macro::State& state)
{
    auto st = calc_screen_tiles(pfrm);

    if (selected_name_) {
        for (int i = 0; i < selected_name_->len(); ++i) {
            pfrm.set_tile(Layer::overlay, 1 + i, st.y - 3, 0);
        }
    }

    selected_name_.emplace(pfrm,
                           state.sector().name().c_str(),
                           OverlayCoord{1, (u8)(st.y - 4)});

    for (int i = 0; i < selected_name_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, 1 + i, st.y - 3, 86);
    }


    pfrm.set_tile(Layer::overlay, 1, st.y - 2, 85);

    if (not selected_population_) {
        selected_population_.emplace(pfrm,
                                     OverlayCoord{2, (u8)(st.y - 2)});
    }

    selected_population_->assign(state.sector().population());

}



void MacroverseScene::display(Platform& pfrm, App& app)
{

    pfrm.system_call(
        "_prlx_macro",
        (void*)(intptr_t)(int)app.macrocosm()->data_->cloud_scroll_);


    const auto sw = pfrm.screen().size();

    auto draw_node = [&] {
        Sprite spr;

        if (state_ == State::reveal) {
            spr.set_mix({ColorConstant::rich_black,
                         u8(255 * (1.f - (float(timer_) / reveal_time)))});
        }

        spr.set_position({sw.x / 2 - 16, sw.y / 2 - 16});
        spr.set_texture_index(6);
        pfrm.screen().draw(spr);
        spr.set_texture_index(7);
        spr.set_position({sw.x / 2 - 16, sw.y / 2 + 16});
        pfrm.screen().draw(spr);
    };

    draw_node();
}



} // namespace skyland::macro
