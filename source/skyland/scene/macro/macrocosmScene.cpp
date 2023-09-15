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



u8 screenshake = 0;



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.macrocosm()) {
        Platform::fatal("macro state unbound!?");
    }

    if (screenshake) {
        app.camera()->shake(screenshake);
        screenshake = 0;
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

    auto& m = macrocosm(app);

    app.player().update(pfrm, app, delta);
    app.camera()->update(
        pfrm, app, app.player_island(), {}, delta, m.data_->checkers_mode_);

    app.environment().update(pfrm, app, delta);

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

        // pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
        pfrm.screen().clear();
        pfrm.screen().display();
        raster::globalstate::is_night = not raster::globalstate::is_night;
        if (raster::globalstate::is_night) {
            pfrm.load_background_texture("background_macro_night");
        } else {
            pfrm.load_background_texture("background_macro");
            m.data_->origin_sector_->on_day_transition();
            for (auto& s : m.data_->other_sectors_) {
                s->on_day_transition();
            }
        }
        raster::globalstate::_recalc_depth_test.fill();
        m.sector().shadowcast();
        raster::globalstate::_changed = true;
        m.sector().render(pfrm);
        pfrm.screen().schedule_fade(0.1f, ColorConstant::rich_black);
        pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
    }

    if (ui_ and not m.data_->freebuild_mode_) {
        // m.data_->year_timer_ += delta;
        // auto secs = year_length(m);
        // auto secs_per_season = secs / 4;

        // if (m.data_->year_timer_ > secs) {
        //     m.data_->year_timer_ = 0;
        //     m.advance(1);
        //     pfrm.speaker().play_sound("openbook", 1);
        //     draw_year(pfrm, m);
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
        (*ui_)->food_->update(pfrm, delta);
        (*ui_)->population_->update(pfrm, delta);
        (*ui_)->productivity_->update(pfrm, delta);
        (*ui_)->lumber_->update(pfrm, delta);
        (*ui_)->stone_->update(pfrm, delta);
        (*ui_)->crystal_->update(pfrm, delta);
        (*ui_)->water_->update(pfrm, delta);
        (*ui_)->clay_->update(pfrm, delta);
    }


    auto next = update(pfrm, app.player(), m);

    auto& entities = m.data_->entities_;
    for (auto e = entities.begin(); e not_eq entities.end();) {
        if (not(*e)->alive()) {
            e = entities.erase(e);
        } else {
            (*e)->update(pfrm, m, delta);
            ++e;
        }
    }


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

    for (auto& e : m.data_->entities_) {
        pfrm.screen().draw(e->sprite());
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
    pfrm.screen().draw(sunmoon_spr);

    display(pfrm, macrocosm(app));
}



u32 format_ui_fraction(u16 avail, u16 used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



ScenePtr<Scene>
MacrocosmScene::update(Platform& pfrm, Player& player, macro::EngineImpl& state)
{
    state.sector().update();

    if (auto music = pfrm.speaker().completed_music()) {
        if (str_eq(music, "life_in_silco")) {
            pfrm.speaker().play_music("unaccompanied_wind", 0);
        } else {
            if (state.data_->frames_since_music_ > 6 and
                rng::choice<6>(rng::utility_state) == 0) {
                state.data_->frames_since_music_ = 0;
                pfrm.speaker().play_music("life_in_silco", 0);
            } else {
                state.data_->frames_since_music_++;
            }
        }
    }


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



void MacrocosmScene::display(Platform& pfrm, macro::EngineImpl& state)
{
    state.sector().render(pfrm);
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



Microseconds MacrocosmScene::year_length(macro::EngineImpl& state)
{
    return 1;
}



void MacrocosmScene::enter(Platform& pfrm,
                           macro::EngineImpl& state,
                           Scene& prev)
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
            pfrm,
            OverlayCoord{1, 1},
            415,
            format_ui_fraction(pop * 10, state.sector().productivity()),
            UIMetric::Align::left,
            UIMetric::Format::fraction);

        (*ui_)->population_.emplace(
            pfrm,
            OverlayCoord{1, 3},
            413,
            format_ui_fraction(state.sector().housing(), pop),
            UIMetric::Align::left,
            UIMetric::Format::fraction);

        (*ui_)->food_.emplace(
            pfrm,
            OverlayCoord{1, 2},
            414,
            format_ui_fraction(sector.food_storage(), sector.food()),
            UIMetric::Align::left,
            UIMetric::Format::fraction);

        (*ui_)->stone_.emplace(pfrm,
                               OverlayCoord{1, 5},
                               417,
                               state.data_->p().stone_.get(),
                               UIMetric::Align::left);

        (*ui_)->lumber_.emplace(pfrm,
                                OverlayCoord{1, 6},
                                423,
                                state.data_->p().lumber_.get(),
                                UIMetric::Align::left);

        (*ui_)->water_.emplace(pfrm,
                               OverlayCoord{1, 7},
                               371,
                               state.data_->p().water_.get(),
                               UIMetric::Align::left);

        (*ui_)->clay_.emplace(pfrm,
                              OverlayCoord{1, 8},
                              370,
                              state.data_->p().clay_.get(),
                              UIMetric::Align::left);

        (*ui_)->crystal_.emplace(pfrm,
                                 OverlayCoord{1, 9},
                                 424,
                                 state.data_->p().crystal_.get(),
                                 UIMetric::Align::left);
    }

    draw_compass(pfrm, state);
    draw_keylock(pfrm, state);

    for (int y = 5; y < 13; y += 2) {
        pfrm.set_tile(Layer::overlay, 28, y, 471);
    }
}



void MacrocosmScene::exit(Platform& pfrm, macro::EngineImpl& state, Scene& next)
{
    if (not next.cast_macrocosm_scene()) {
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

    int start_y = 1;
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
