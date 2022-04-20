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


#include "macrocosmScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.macrocosm()) {
        Platform::fatal("macro state unbound!?");
    }

    app.player().update(pfrm, app, delta);
    app.camera()->update(pfrm, app, app.player_island(), {}, delta, true);


    if (auto scene = update(pfrm, app.player(), *app.macrocosm())) {
        return scene;
    }

    return null_scene();
}



void MacrocosmScene::display(Platform& pfrm, App& app)
{
    if (not app.macrocosm()) {
        return;
    }

    display(pfrm, *app.macrocosm());
}



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, Player& player, macro::State& state)
{
    state.sector().update();

    return null_scene();
}



void MacrocosmScene::display(Platform& pfrm, macro::State& state)
{
    state.sector().render(pfrm);
}



u32 format_ui_fraction(u16 avail, u16 used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



void MacrocosmScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    auto stat = app.macrocosm()->sector().stats();

    auto pop = app.macrocosm()->sector().population_;

    food_.emplace(
        pfrm,
        OverlayCoord{1, 1},
        414,
        format_ui_fraction(stat.food_, pop / terrain::food_consumption_factor),
        UIMetric::Align::left,
        UIMetric::Format::fraction_p_m);

    population_.emplace(
        pfrm, OverlayCoord{1, 3}, 413, pop, UIMetric::Align::left);

    // Text temp1(pfrm, OverlayCoord{1, 5});
    // Fixnum fmt(app.macrocosm()->sector().population_growth_rate());
    // temp1.append(fmt.numerator());
    // temp1.append(".");
    // temp1.append(fmt.denominator());
    // temp1.__detach();


    coins_.emplace(pfrm,
                   OverlayCoord{1, 2},
                   146,
                   (int)app.macrocosm()->data_->coins_,
                   UIMetric::Align::left);

    const auto year = app.macrocosm()->data_->year_ + 1;

    auto yr = SYSTR(macro_year);
    auto yr_len = utf8::len(yr->c_str());
    auto st = calc_screen_tiles(pfrm);
    Text temp(
        pfrm,
        OverlayCoord{u8(st.x - (yr_len + integer_text_length(year) + 1)), 1});
    temp.append(yr->c_str());
    temp.append(year);
    temp.__detach();

    draw_compass(pfrm, *app.macrocosm());
}



void MacrocosmScene::draw_compass(Platform& pfrm, macro::State& state)
{
    auto o = state.sector().orientation();
    int compass_tile = 434 + (int)o * 4;
    draw_image(pfrm, compass_tile, 27, 3, 2, 2, Layer::overlay);
}



void MacrocosmScene::exit(Platform& pfrm, App& app, Scene& next)
{
    food_.reset();
    population_.reset();
    coins_.reset();
}



} // namespace skyland::macro
