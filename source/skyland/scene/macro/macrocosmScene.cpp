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



MacrocosmScene::MacrocosmScene()
{
}



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.macrocosm()) {
        Platform::fatal("macro state unbound!?");
    }

    app.player().update(pfrm, app, delta);
    app.camera()->update(pfrm, app, app.player_island(), {}, delta, true);


    if (ui_) {
        (*ui_)->coins_->update(pfrm, delta);
        (*ui_)->population_->update(pfrm, delta);
        (*ui_)->food_->update(pfrm, delta);
        (*ui_)->employment_->update(pfrm, delta);
        (*ui_)->housing_->update(pfrm, delta);
    }


    if (auto scene = update(pfrm, app.player(), *app.macrocosm())) {
        return scene;
    }

    app.macrocosm()->data_->cloud_scroll_ += 0.000001f * delta;

    return null_scene();
}



void MacrocosmScene::display(Platform& pfrm, App& app)
{
    if (not app.macrocosm()) {
        return;
    }

    pfrm.system_call(
        "_prlx_macro",
        (void*)(intptr_t)(int)app.macrocosm()->data_->cloud_scroll_);

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



void MacrocosmScene::update_ui(macro::State& state)
{
    if (not ui_) {
        return;
    }


    auto& sector = state.sector();

    auto stat = sector.stats();
    auto pop = sector.population();


    (*ui_)->food_->sync_value(
        format_ui_fraction(stat.food_, pop / terrain::food_consumption_factor));

    (*ui_)->population_->sync_value(
        format_ui_fraction(pop, sector.population_growth_rate()));

    (*ui_)->coins_->sync_value(format_ui_fraction(
        (int)state.data_->p().coins_.get(), state.coin_yield()));

    (*ui_)->housing_->sync_value(stat.housing_);

    (*ui_)->employment_->sync_value(stat.employment_);
}



void MacrocosmScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (not app.macrocosm()) {
        Platform::fatal(format("logic error! % %", __FILE__, __LINE__).c_str());
    }

    auto m = dynamic_cast<MacrocosmScene*>(&prev);
    if (m and m->ui_) {
        ui_ = std::move(m->ui_);
        if (m->should_update_ui_after_exit()) {
            update_ui(*app.macrocosm());
        }
    } else {
        ui_ = allocate_dynamic<UIObjects>("macro-ui-objects");

        auto& sector = app.macrocosm()->sector();

        auto stat = sector.stats();
        auto pop = sector.population();


        (*ui_)->food_.emplace(
            pfrm,
            OverlayCoord{1, 1},
            414,
            format_ui_fraction(stat.food_,
                               pop / terrain::food_consumption_factor),
            UIMetric::Align::left,
            UIMetric::Format::fraction_p_m);

        (*ui_)->population_.emplace(
            pfrm,
            OverlayCoord{1, 3},
            413,
            format_ui_fraction(pop, sector.population_growth_rate()),
            UIMetric::Align::left,
            UIMetric::Format::integer_with_rate);

        (*ui_)->coins_.emplace(
            pfrm,
            OverlayCoord{1, 2},
            146,
            format_ui_fraction((int)app.macrocosm()->data_->p().coins_.get(),
                               app.macrocosm()->coin_yield()),
            UIMetric::Align::left,
            UIMetric::Format::integer_with_rate);

        (*ui_)->employment_.emplace(pfrm,
                                    OverlayCoord{1, 4},
                                    415,
                                    stat.employment_,
                                    UIMetric::Align::left);

        (*ui_)->housing_.emplace(pfrm,
                                 OverlayCoord{1, 5},
                                 416,
                                 stat.housing_,
                                 UIMetric::Align::left);
    }

    const auto year = app.macrocosm()->data_->p().year_.get() + 1;

    auto yr = SYSTR(macro_year);
    auto yr_len = utf8::len(yr->c_str());
    auto st = calc_screen_tiles(pfrm);
    Text temp(
        pfrm,
        OverlayCoord{u8(st.x - (yr_len + integer_text_length(year) + 1)), 1});
    temp.append(yr->c_str(),
                Text::OptColors{
                    {ColorConstant::med_blue_gray, ColorConstant::rich_black}});
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
    if (not dynamic_cast<MacrocosmScene*>(&next)) {
        ui_.reset();
    }
}



} // namespace skyland::macro
