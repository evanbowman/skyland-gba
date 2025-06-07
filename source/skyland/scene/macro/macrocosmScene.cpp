////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



u8 screenshake = 0;



ScenePtr MacrocosmScene::update(Time delta)
{
    if (not APP.macrocosm()) {
        Platform::fatal("macro state unbound!?");
    }

    if (screenshake) {
        APP.camera()->shake(screenshake);
        screenshake = 0;
    }

    const auto exit_cond = APP.exit_condition();
    if (exit_cond not_eq App::ExitCondition::none) {
        APP.exit_condition() = App::ExitCondition::none;
        return make_scene<MacroverseScene>();
    }

    if (APP.dialog_buffer()) {
        auto buffer = std::move(*APP.dialog_buffer());
        APP.dialog_buffer().reset();
        auto next = make_scene<BoxedDialogScene>(std::move(buffer));
        next->set_next_scene(make_deferred_scene<SelectorScene>());
        return next;
    }

    auto& m = macrocosm();

    APP.player().update(delta);
    APP.camera()->update(
        APP.player_island(), {}, delta, m.data_->checkers_mode_);

    APP.environment().update(delta);

    m.data_->fluid_anim_timer_ += delta;
    bool was_gre = false;
    int val = 0;
    int val2 = 0;
    while (m.data_->fluid_anim_timer_ > milliseconds(16)) {
        m.data_->fluid_anim_timer_ -= milliseconds(16);
        was_gre = true;
        val = sine8(m.data_->water_anim_index_);
        val2 = sine8(m.data_->lava_anim_index_);

        m.data_->water_anim_index_ += 1;
        m.data_->lava_anim_index_ += 4;
    }
    if (was_gre) {
        PLATFORM.screen().set_shader_argument((val << 8) | val2);

        PLATFORM_EXTENSION(palette_sync);
    }

    const bool is_night = raster::globalstate::is_night;
    int interval = day_frames;
    if (is_night) {
        interval = night_frames;
    }

    m.data_->p().day_night_cyc_.set(m.data_->p().day_night_cyc_.get() + 1);

    if (m.data_->checkers_mode_) {
        m.data_->p().day_night_cyc_.set(800);
    }

    if (m.data_->p().day_night_cyc_.get() > interval) {
        m.data_->p().day_night_cyc_.set(0);

        // PLATFORM.screen().schedule_fade(0.7f, custom_color(0x102447));
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        raster::globalstate::is_night = not raster::globalstate::is_night;
        if (raster::globalstate::is_night) {
            PLATFORM.load_background_texture("background_macro_night");
        } else {
            PLATFORM.load_background_texture("background_macro");
            m.data_->origin_sector_->on_day_transition();
            for (auto& s : m.data_->other_sectors_) {
                s->on_day_transition();
            }
        }
        raster::globalstate::_recalc_depth_test.fill();
        m.sector().shadowcast();
        raster::globalstate::_changed = true;
        m.sector().render();
        PLATFORM.screen().schedule_fade(0.1f, ColorConstant::rich_black);
        PLATFORM.screen().schedule_fade(0.f, ColorConstant::rich_black);
    }

    if (ui_ and not m.data_->freebuild_mode_) {
        // m.data_->year_timer_ += delta;
        // auto secs = year_length(m);
        // auto secs_per_season = secs / 4;

        // if (m.data_->year_timer_ > secs) {
        //     m.data_->year_timer_ = 0;
        //     m.advance(1);
        //     PLATFORM.speaker().play_sound("openbook", 1);
        //     draw_year(m);
        //     if (ui_) {
        //         update_ui(m);
        //     }
        // }

        // const int season =
        //     current_season(m.data_->year_timer_, secs_per_season);

        // if (season not_eq last_season_) {
        // }
    }

    if (ui_) {
        (*ui_)->food_->update(delta);
        (*ui_)->population_->update(delta);
        (*ui_)->productivity_->update(delta);
        (*ui_)->lumber_->update(delta);
        (*ui_)->stone_->update(delta);
        (*ui_)->crystal_->update(delta);
        (*ui_)->water_->update(delta);
        (*ui_)->clay_->update(delta);
    }


    auto next = update(APP.player(), m);

    auto& entities = m.data_->entities_;
    for (auto e = entities.begin(); e not_eq entities.end();) {
        if (not(*e)->alive()) {
            e = entities.erase(e);
        } else {
            (*e)->update(m, delta);
            ++e;
        }
    }


    m.sector().render_setup();

    m.data_->cloud_scroll_ += 0.000001f * delta;

    return next;
}



int MacrocosmScene::current_season(Time year_timer, Time secs_per_season)
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


void MacrocosmScene::display()
{
    if (not APP.macrocosm()) {
        return;
    }

    auto& m = macrocosm();

    PLATFORM_EXTENSION(update_parallax_macro, macrocosm().data_->cloud_scroll_);

    for (auto& e : m.data_->entities_) {
        PLATFORM.screen().draw(e->sprite());
    }

    const bool is_night = raster::globalstate::is_night;

    Sprite sunmoon_spr;
    sunmoon_spr.set_size(Sprite::Size::w16_h32);
    Fixnum sunmoon_y = 40.0_fixed;

    if (is_night) {
        sunmoon_y += Fixnum(float(m.data_->p().day_night_cyc_.get()) /
                            float(night_frames)) *
                     43.0_fixed;
        sunmoon_spr.set_texture_index(29);
    } else {
        sunmoon_y += Fixnum(float(m.data_->p().day_night_cyc_.get()) /
                            float(day_frames)) *
                     43.0_fixed;
        sunmoon_spr.set_texture_index(28);
    }

    sunmoon_spr.set_position({225.0_fixed, sunmoon_y});
    PLATFORM.screen().draw(sunmoon_spr);

    display(macrocosm());
}



u32 format_ui_fraction(u16 avail, u16 used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



ScenePtr MacrocosmScene::update(Player& player, macro::EngineImpl& state)
{
    state.sector().update();

    if (ui_) {
        auto prod = state.sector().productivity();
        auto pop = state.sector().population();
        auto& sector = state.sector();
        (*ui_)->food_->sync_value(
            format_ui_fraction(sector.food_storage(), sector.food()));
        (*ui_)->population_->sync_value(
            format_ui_fraction(sector.housing(), pop));
        (*ui_)->productivity_->sync_value(format_ui_fraction(pop * 10, prod));

        (*ui_)->lumber_->sync_value(state.data_->p().lumber_.get());
        (*ui_)->stone_->sync_value(state.data_->p().stone_.get());
        (*ui_)->crystal_->sync_value(state.data_->p().crystal_.get());
        (*ui_)->water_->sync_value(state.data_->p().water_.get());
        (*ui_)->clay_->sync_value(state.data_->p().clay_.get());
    }


    return null_scene();
}



void MacrocosmScene::display(macro::EngineImpl& state)
{
    state.sector().render();
}



void MacrocosmScene::update_ui(macro::EngineImpl& state)
{
    if (not ui_) {
        return;
    }


    auto& sector = state.sector();


    (*ui_)->food_->sync_value(
        format_ui_fraction(sector.food_storage(), sector.food()));
    (*ui_)->population_->sync_value(format_ui_fraction(
        state.sector().housing(), state.sector().population()));
    (*ui_)->productivity_->sync_value(
        format_ui_fraction(sector.population() * 10, sector.productivity()));

    (*ui_)->lumber_->sync_value(state.data_->p().lumber_.get());
    (*ui_)->stone_->sync_value(state.data_->p().stone_.get());
    (*ui_)->crystal_->sync_value(state.data_->p().crystal_.get());
    (*ui_)->water_->sync_value(state.data_->p().water_.get());
    (*ui_)->clay_->sync_value(state.data_->p().clay_.get());
}



Time MacrocosmScene::year_length(macro::EngineImpl& state)
{
    return 1;
}



void MacrocosmScene::enter(macro::EngineImpl& state, Scene& prev)
{
    // auto secs = year_length(state);
    // auto secs_per_season = secs / 4;

    // const int season =
    //     current_season(state.data_->year_timer_, secs_per_season);

    auto m = prev.cast_macrocosm_scene();
    if (m and m->ui_) {
        ui_ = std::move(m->ui_);
        if (m->should_update_ui_after_exit()) {
            update_ui(state);
        }
    } else if (not state.data_->freebuild_mode_ and
               not state.data_->checkers_mode_) {

        ui_ = allocate_dynamic<UIObjects>("macro-ui-objects");

        auto& sector = state.sector();

        auto pop = sector.population();

        (*ui_)->productivity_.emplace(

            OverlayCoord{1, 1},
            415,
            format_ui_fraction(pop * 10, state.sector().productivity()),
            UIMetric::Align::left,
            UIMetric::Format::fraction);

        (*ui_)->population_.emplace(

            OverlayCoord{1, 3},
            413,
            format_ui_fraction(state.sector().housing(), pop),
            UIMetric::Align::left,
            UIMetric::Format::fraction);

        (*ui_)->food_.emplace(

            OverlayCoord{1, 2},
            414,
            format_ui_fraction(sector.food_storage(), sector.food()),
            UIMetric::Align::left,
            UIMetric::Format::fraction);

        (*ui_)->stone_.emplace(OverlayCoord{1, 5},
                               417,
                               state.data_->p().stone_.get(),
                               UIMetric::Align::left);

        (*ui_)->lumber_.emplace(OverlayCoord{1, 6},
                                423,
                                state.data_->p().lumber_.get(),
                                UIMetric::Align::left);

        (*ui_)->water_.emplace(OverlayCoord{1, 7},
                               371,
                               state.data_->p().water_.get(),
                               UIMetric::Align::left);

        (*ui_)->clay_.emplace(OverlayCoord{1, 8},
                              370,
                              state.data_->p().clay_.get(),
                              UIMetric::Align::left);

        (*ui_)->crystal_.emplace(OverlayCoord{1, 9},
                                 424,
                                 state.data_->p().crystal_.get(),
                                 UIMetric::Align::left);
    }

    draw_compass(state);
    draw_keylock(state);

    for (int y = 5; y < 13; y += 2) {
        PLATFORM.set_tile(Layer::overlay, 28, y, 471);
    }
}



void MacrocosmScene::exit(macro::EngineImpl& state, Scene& next)
{
    if (not next.cast_macrocosm_scene()) {
        ui_.reset();
    }
}



void MacrocosmScene::enter(Scene& prev)
{
    if (not APP.macrocosm()) {
        Platform::fatal(format("logic error! % %", __FILE__, __LINE__).c_str());
    }

    auto freebuild_flag = GlobalPersistentData::freebuild_unlocked;
    if (not APP.gp_.stateflags_.get(freebuild_flag)) {
        APP.gp_.stateflags_.set(freebuild_flag, true);
        save::store_global_data(APP.gp_);
    }

    enter(macrocosm(), prev);
}



void MacrocosmScene::draw_compass(macro::EngineImpl& state)
{
    auto o = state.sector().orientation();
    int compass_tile = 434 + (int)o * 4;

    int start_y = 1;
    if (state.data_->freebuild_mode_ or state.data_->checkers_mode_) {
        start_y = 1;
    }

    draw_image(compass_tile, 27, start_y, 2, 2, Layer::overlay);
}



void MacrocosmScene::draw_keylock(macro::EngineImpl& state)
{
    int y = 6;
    if (state.data_->freebuild_mode_ or state.data_->checkers_mode_) {
        y = 4;
    }

    switch (state.data_->keylock_) {
    case Keylock::nolock:
        PLATFORM.set_tile(Layer::overlay, 27, y, 0);
        PLATFORM.set_tile(Layer::overlay, 28, y, 0);
        break;

    case Keylock::buildlock:
        PLATFORM.set_tile(Layer::overlay, 27, y, 388);
        PLATFORM.set_tile(Layer::overlay, 28, y, 390);
        break;

    case Keylock::improvelock:
        PLATFORM.set_tile(Layer::overlay, 27, y, 387);
        PLATFORM.set_tile(Layer::overlay, 28, y, 390);
        break;

    case Keylock::deletelock:
        PLATFORM.set_tile(Layer::overlay, 27, y, 389);
        PLATFORM.set_tile(Layer::overlay, 28, y, 390);
        break;
    }
}



void MacrocosmScene::exit(Scene& next)
{
    exit(macrocosm(), next);
}



} // namespace skyland::macro
