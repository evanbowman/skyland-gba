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
    state.data_->sector_.update(pfrm);

    return null_scene();
}



void MacrocosmScene::display(Platform& pfrm, macro::State& state)
{
    state.data_->sector_.render(pfrm);
}



u32 format_ui_fraction(u16 avail, u16 used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



void MacrocosmScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    auto pop = app.macrocosm()->data_->sector_.population_;

    food_.emplace(pfrm,
                  OverlayCoord{1, 1},
                  414,
                  format_ui_fraction(4, pop / 2),
                  UIMetric::Align::left,
                  UIMetric::Format::fraction_p_m);

    population_.emplace(
        pfrm, OverlayCoord{1, 2}, 413, pop, UIMetric::Align::left);

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
}



void MacrocosmScene::exit(Platform& pfrm, App& app, Scene& next)
{
    food_.reset();
    population_.reset();
}



} // namespace skyland::macro
