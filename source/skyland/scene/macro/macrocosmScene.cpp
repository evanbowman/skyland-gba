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
#include "macroverseScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene/lispReplScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



MacrocosmScene::MacrocosmScene()
{
}



static const u8 sine8[256] = {
    0x80, 0x83, 0x86, 0x89, 0x8C, 0x90, 0x93, 0x96, 0x99, 0x9C, 0x9F, 0xA2,
    0xA5, 0xA8, 0xAB, 0xAE, 0xB1, 0xB3, 0xB6, 0xB9, 0xBC, 0xBF, 0xC1, 0xC4,
    0xC7, 0xC9, 0xCC, 0xCE, 0xD1, 0xD3, 0xD5, 0xD8, 0xDA, 0xDC, 0xDE, 0xE0,
    0xE2, 0xE4, 0xE6, 0xE8, 0xEA, 0xEB, 0xED, 0xEF, 0xF0, 0xF1, 0xF3, 0xF4,
    0xF5, 0xF6, 0xF8, 0xF9, 0xFA, 0xFA, 0xFB, 0xFC, 0xFD, 0xFD, 0xFE, 0xFE,
    0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFE, 0xFE, 0xFD,
    0xFD, 0xFC, 0xFB, 0xFA, 0xFA, 0xF9, 0xF8, 0xF6, 0xF5, 0xF4, 0xF3, 0xF1,
    0xF0, 0xEF, 0xED, 0xEB, 0xEA, 0xE8, 0xE6, 0xE4, 0xE2, 0xE0, 0xDE, 0xDC,
    0xDA, 0xD8, 0xD5, 0xD3, 0xD1, 0xCE, 0xCC, 0xC9, 0xC7, 0xC4, 0xC1, 0xBF,
    0xBC, 0xB9, 0xB6, 0xB3, 0xB1, 0xAE, 0xAB, 0xA8, 0xA5, 0xA2, 0x9F, 0x9C,
    0x99, 0x96, 0x93, 0x90, 0x8C, 0x89, 0x86, 0x83, 0x80, 0x7D, 0x7A, 0x77,
    0x74, 0x70, 0x6D, 0x6A, 0x67, 0x64, 0x61, 0x5E, 0x5B, 0x58, 0x55, 0x52,
    0x4F, 0x4D, 0x4A, 0x47, 0x44, 0x41, 0x3F, 0x3C, 0x39, 0x37, 0x34, 0x32,
    0x2F, 0x2D, 0x2B, 0x28, 0x26, 0x24, 0x22, 0x20, 0x1E, 0x1C, 0x1A, 0x18,
    0x16, 0x15, 0x13, 0x11, 0x10, 0x0F, 0x0D, 0x0C, 0x0B, 0x0A, 0x08, 0x07,
    0x06, 0x06, 0x05, 0x04, 0x03, 0x03, 0x02, 0x02, 0x02, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x02, 0x02, 0x02, 0x03, 0x03, 0x04, 0x05, 0x06,
    0x06, 0x07, 0x08, 0x0A, 0x0B, 0x0C, 0x0D, 0x0F, 0x10, 0x11, 0x13, 0x15,
    0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2B, 0x2D,
    0x2F, 0x32, 0x34, 0x37, 0x39, 0x3C, 0x3F, 0x41, 0x44, 0x47, 0x4A, 0x4D,
    0x4F, 0x52, 0x55, 0x58, 0x5B, 0x5E, 0x61, 0x64, 0x67, 0x6A, 0x6D, 0x70,
    0x74, 0x77, 0x7A, 0x7D};



void MacrocosmScene::draw_season(Platform& pfrm,
                                 macro::EngineImpl& state,
                                 int season)
{
    if (state.data_->checkers_mode_) {
        return;
    }

    auto str = [&] {
        switch (season) {
        case 0:
            return SYSTR(spring);
        case 1:
            return SYSTR(summer);
        case 2:
            return SYSTR(fall);
        case 3:
            return SYSTR(winter);
        default:
            return SYSTR(empty);
        }
    }();

    u8 pos = calc_screen_tiles(pfrm).x - (utf8::len(str->c_str()) + 1);
    for (int x = pos - 8; x < calc_screen_tiles(pfrm).x; ++x) {
        pfrm.set_tile(Layer::overlay, x, 2, 0);
    }
    Text season_text(pfrm, OverlayCoord{pos, 2});

    season_text.assign(str->c_str(),
                       Text::OptColors{{ColorConstant::med_blue_gray,
                                        ColorConstant::rich_black}});

    season_text.__detach();

    last_season_ = season;
}



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.macrocosm()) {
        Platform::fatal("macro state unbound!?");
    }

    if (state_bit_load(app, StateBit::launch_repl)) {
        state_bit_store(app, StateBit::launch_repl, false);
        return scene_pool::alloc<LispReplScene>();
    }

    const auto exit_cond = app.exit_condition();
    if (exit_cond not_eq App::ExitCondition::none) {
        app.exit_condition() = App::ExitCondition::none;
        return scene_pool::alloc<MacroverseScene>();
    }

    if (app.dialog_buffer()) {
        auto buffer = std::move(*app.dialog_buffer());
        app.dialog_buffer().reset();
        bool answer = state_bit_load(app, StateBit::dialog_expects_answer);
        state_bit_store(app, StateBit::dialog_expects_answer, false);
        auto next =
            scene_pool::alloc<BoxedDialogScene>(std::move(buffer), answer);
        next->set_next_scene(scene_pool::make_deferred_scene<SelectorScene>());
        return next;
    }

    app.player().update(pfrm, app, delta);
    app.camera()->update(pfrm, app, app.player_island(), {}, delta, true);

    app.environment().update(pfrm, app, delta);

    auto& m = macrocosm(app);

    m.data_->fluid_anim_timer_ += delta;
    bool was_gre = false;
    int val = 0;
    int val2 = 0;
    while (m.data_->fluid_anim_timer_ > milliseconds(16)) {
        m.data_->fluid_anim_timer_ -= milliseconds(16);
        was_gre = true;
        val = sine8[m.data_->water_anim_index_];
        val2 = sine8[m.data_->lava_anim_index_];

        m.data_->water_anim_index_ += 1;
        m.data_->lava_anim_index_ += 4;
    }
    if (was_gre) {
        pfrm.screen().set_shader_argument((val << 8) | val2);
        pfrm.system_call("psync", nullptr);
    }

    if (ui_ and not m.data_->freebuild_mode_) {
        m.data_->year_timer_ += delta;

        auto secs = year_length(m);
        auto secs_per_season = secs / 4;

        if (m.data_->year_timer_ > secs) {
            m.data_->year_timer_ = 0;
            m.advance(1);
            pfrm.speaker().play_sound("openbook", 1);
            draw_year(pfrm, m);
            if (ui_) {
                update_ui(m);
            }
        }

        const int season =
            current_season(m.data_->year_timer_, secs_per_season);

        if (season not_eq last_season_) {
            draw_season(pfrm, m, season);
        }
    }

    if (ui_) {
        (*ui_)->coins_->update(pfrm, delta);
        (*ui_)->population_->update(pfrm, delta);
        (*ui_)->food_->update(pfrm, delta);
        (*ui_)->employment_->update(pfrm, delta);
        (*ui_)->housing_->update(pfrm, delta);
        (*ui_)->happiness_->update(pfrm, delta);
    }


    auto next = update(pfrm, app.player(), m);


    m.sector().render_setup(pfrm);

    m.data_->cloud_scroll_ += 0.000001f * delta;

    return next;
}



int MacrocosmScene::current_season(Microseconds year_timer,
                                   Microseconds secs_per_season)
{
    if (year_timer > secs_per_season * 3) {
        return 3;
    } else if (year_timer > secs_per_season * 2) {
        return 2;
    } else if (year_timer > secs_per_season * 1) {
        return 1;
    } else {
        return 0;
    }
}


void MacrocosmScene::display(Platform& pfrm, App& app)
{
    if (not app.macrocosm()) {
        return;
    }

    auto& m = macrocosm(app);

    pfrm.system_call("_prlx_macro",
                     (void*)(intptr_t)(int)m.data_->cloud_scroll_);

    display(pfrm, macrocosm(app));
}



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, Player& player, macro::EngineImpl& state)
{
    state.sector().update();

    return null_scene();
}



void MacrocosmScene::display(Platform& pfrm, macro::EngineImpl& state)
{
    state.sector().render(pfrm);
}



u32 format_ui_fraction(u16 avail, u16 used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



void MacrocosmScene::update_ui(macro::EngineImpl& state)
{
    if (not ui_) {
        return;
    }


    auto& sector = state.sector();

    auto stat = sector.stats();
    auto pop = sector.population();


    (*ui_)->food_->sync_value(format_ui_fraction(
        stat.food_ + stat.food_exports_,
        stat.food_exports_ + pop / EngineImpl::food_consumption_factor()));

    (*ui_)->population_->sync_value(
        format_ui_fraction(pop, sector.population_growth_rate()));

    auto disp_coins = (int)state.data_->p().coins_.get();
    if (disp_coins > std::numeric_limits<u16>::max()) {
        disp_coins /= 1000;
        (*ui_)->coins_->use_large_numerator(true);
    } else {
        (*ui_)->coins_->use_large_numerator(false);
    }

    (*ui_)->coins_->sync_value(
        format_ui_fraction(disp_coins, state.coin_yield()));

    (*ui_)->housing_->sync_value(stat.housing_);

    (*ui_)->employment_->sync_value(stat.employment_);
    (*ui_)->happiness_->sync_value((s32)sector.get_happiness());
}



Microseconds MacrocosmScene::year_length(macro::EngineImpl& state)
{
    auto& b = *state.data_->bindings_;
    auto added_secs = seconds(b.mcr_added_seconds_per_year_per_island);
    added_secs *= 1 + state.data_->other_sectors_.size() +
                  state.data_->outpost_sectors_.size();
    return added_secs + seconds(b.mcr_base_seconds_per_year);
}



void MacrocosmScene::enter(Platform& pfrm,
                           macro::EngineImpl& state,
                           Scene& prev)
{
    auto secs = year_length(state);
    auto secs_per_season = secs / 4;

    const int season =
        current_season(state.data_->year_timer_, secs_per_season);

    auto m = dynamic_cast<MacrocosmScene*>(&prev);
    if (m and m->ui_) {
        ui_ = std::move(m->ui_);
        if (m->should_update_ui_after_exit()) {
            update_ui(state);
        }
    } else if (not state.data_->freebuild_mode_ and
               not state.data_->checkers_mode_) {

        ui_ = allocate_dynamic<UIObjects>("macro-ui-objects");

        auto& sector = state.sector();

        auto stat = sector.stats();
        auto pop = sector.population();

        (*ui_)->food_.emplace(
            pfrm,
            OverlayCoord{1, 1},
            414,
            format_ui_fraction(stat.food_exports_ + stat.food_,
                               stat.food_exports_ +
                                   pop / EngineImpl::food_consumption_factor()),
            UIMetric::Align::left,
            UIMetric::Format::fraction_p_m);

        (*ui_)->population_.emplace(
            pfrm,
            OverlayCoord{1, 3},
            413,
            format_ui_fraction(pop, sector.population_growth_rate()),
            UIMetric::Align::left,
            UIMetric::Format::integer_with_rate);


        auto disp_coins = (int)state.data_->p().coins_.get();
        bool coins_large_numerator = false;
        if (disp_coins > std::numeric_limits<u16>::max()) {
            disp_coins /= 1000;
            coins_large_numerator = true;
        }

        (*ui_)->coins_.emplace(
            pfrm,
            OverlayCoord{1, 2},
            146,
            format_ui_fraction(disp_coins, state.coin_yield()),
            UIMetric::Align::left,
            UIMetric::Format::integer_with_rate,
            coins_large_numerator);


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

        s32 happiness = sector.get_happiness();
        (*ui_)->happiness_.emplace(pfrm,
                                   OverlayCoord{1, 6},
                                   409,
                                   (u32)happiness,
                                   UIMetric::Align::left,
                                   UIMetric::Format::signed_integer);
    }

    draw_year(pfrm, state);
    draw_season(pfrm, state, season);

    draw_compass(pfrm, state);
    draw_keylock(pfrm, state);
}



void MacrocosmScene::draw_year(Platform& pfrm, macro::EngineImpl& state)
{
    if (not state.data_->freebuild_mode_ and not state.data_->checkers_mode_) {
        const auto year = state.data_->p().year_.get() + 1;

        auto yr = SYSTR(macro_year);
        auto yr_len = utf8::len(yr->c_str());
        auto st = calc_screen_tiles(pfrm);
        Text temp(pfrm,
                  OverlayCoord{
                      u8(st.x - (yr_len + integer_text_length(year) + 1)), 1});
        temp.append(yr->c_str(),
                    Text::OptColors{{ColorConstant::med_blue_gray,
                                     ColorConstant::rich_black}});
        temp.append(year);
        temp.__detach();
    }
}



void MacrocosmScene::exit(Platform& pfrm, macro::EngineImpl& state, Scene& next)
{
    if (not dynamic_cast<MacrocosmScene*>(&next)) {
        ui_.reset();
    }
}



void MacrocosmScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (not app.macrocosm()) {
        Platform::fatal(format("logic error! % %", __FILE__, __LINE__).c_str());
    }

    auto freebuild_flag = GlobalPersistentData::freebuild_unlocked;
    if (not app.gp_.stateflags_.get(freebuild_flag)) {
        app.gp_.stateflags_.set(freebuild_flag, true);
        save::store_global_data(pfrm, app.gp_);
    }

    enter(pfrm, macrocosm(app), prev);
}



void MacrocosmScene::draw_compass(Platform& pfrm, macro::EngineImpl& state)
{
    auto o = state.sector().orientation();
    int compass_tile = 434 + (int)o * 4;

    int start_y = 4;
    if (state.data_->freebuild_mode_ or state.data_->checkers_mode_) {
        start_y = 1;
    }

    draw_image(pfrm, compass_tile, 27, start_y, 2, 2, Layer::overlay);
}



void MacrocosmScene::draw_keylock(Platform& pfrm, macro::EngineImpl& state)
{
    int y = 6;
    if (state.data_->freebuild_mode_ or state.data_->checkers_mode_) {
        y = 4;
    }

    switch (state.data_->keylock_) {
    case Keylock::nolock:
        pfrm.set_tile(Layer::overlay, 27, y, 0);
        pfrm.set_tile(Layer::overlay, 28, y, 0);
        break;

    case Keylock::buildlock:
        pfrm.set_tile(Layer::overlay, 27, y, 388);
        pfrm.set_tile(Layer::overlay, 28, y, 390);
        break;

    case Keylock::improvelock:
        pfrm.set_tile(Layer::overlay, 27, y, 387);
        pfrm.set_tile(Layer::overlay, 28, y, 390);
        break;

    case Keylock::deletelock:
        pfrm.set_tile(Layer::overlay, 27, y, 389);
        pfrm.set_tile(Layer::overlay, 28, y, 390);
        break;
    }
}



void MacrocosmScene::exit(Platform& pfrm, App& app, Scene& next)
{
    exit(pfrm, macrocosm(app), next);
}



} // namespace skyland::macro
