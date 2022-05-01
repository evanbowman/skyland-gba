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
#include "selectorScene.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene/textEntryScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void MacroverseScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (not app.macrocosm()) {
        pfrm.fatal("logic error while entering macroverse!");
    }

    if (fastload_) {
        pfrm.load_overlay_texture("overlay_challenges");
        auto str = SYSTR(start_menu_macroverse);
        Text heading(
            pfrm,
            OverlayCoord{
                (u8)centered_text_margins(pfrm, utf8::len(str->c_str())), 1});
        heading.assign(str->c_str());
        heading.__detach();
    }

    pfrm.load_sprite_texture("spritesheet_macroverse");

    pfrm.screen().schedule_fade(0.f);
    pfrm.screen().schedule_fade(1.f, ColorConstant::rich_black, false);

    pfrm.screen().set_view({});

    // Clear the background texture.
    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    selected_ = app.macrocosm()->sector().coordinate();
    initial_sector_ = selected_;

    Text::platform_retain_alphabet(pfrm);
}



ColorConstant water_shader(ShaderPalette p, ColorConstant k, int var, int index)
{
    if (p == ShaderPalette::tile0 or p == ShaderPalette::tile1) {
        if (index == 11) {

            int v1 = (var & 0xff00) >> 8;

            static const Color input(k);
            static const Color k2(custom_color(0x4f92ff));

            Color result(fast_interpolate(k2.r_, input.r_, v1),
                         fast_interpolate(k2.g_, input.g_, v1),
                         fast_interpolate(k2.b_, input.b_, v1));

            return result.hex();

        } else if (index == 3) {

            int v2 = (var & 0xff);

            static const Color input(k);
            static const Color k2(custom_color(0xf7a634));

            Color result(fast_interpolate(k2.r_, input.r_, v2),
                         fast_interpolate(k2.g_, input.g_, v2),
                         fast_interpolate(k2.b_, input.b_, v2));

            return result.hex();
        }
    }

    return k;
}



void MacroverseScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    auto& sector = app.macrocosm()->sector();
    sector.rotate();
    sector.rotate();
    sector.rotate();
    sector.rotate(); // hack to force repaint.

    pfrm.screen().set_shader(water_shader);

    pfrm.sleep(1);
    pfrm.screen().fade(1.f);


    pfrm.fill_overlay(0);

    if (text_objs_.size()) {
        Platform::fatal("text objs not cleared, tearing will occur!");
    }
    pfrm.load_overlay_texture("overlay");

    pfrm.load_sprite_texture("spritesheet_macro");

    pfrm.screen().schedule_fade(0.f);

    pfrm.screen().set_view({});
}



static const auto reveal_time = milliseconds(90);



static Vec2<Fixnum> sector_map_display_pos(Platform& pfrm, Vec2<s8> coord)
{
    const auto sw = pfrm.screen().size();

    Vec2<Fixnum> origin{sw.x / 2 - 16, sw.y / 2 - 16};
    origin.x += coord.x * 32;
    origin.y += coord.y * 38;

    return origin;
}



ScenePtr<Scene>
MacroverseScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.macrocosm()) {
        Platform::fatal(format("% %", __FILE__, __LINE__).c_str());
    }

    if (exit_) {
        return scene_pool::alloc<SelectorScene>();
    }


    auto clear_description = [&] {
        text_objs_.clear();
        auto st_y = calc_screen_tiles(pfrm).y;
        for (int y = st_y - 6; y < st_y; ++y) {
            for (int x = 0; x < calc_screen_tiles(pfrm).x; ++x) {
                pfrm.set_tile(Layer::overlay, x, y, 0);
            }
        }
    };


    auto enter_opt_state = [&] {
        clear_description();
        state_ = State::options;
        text_objs_.clear();

        auto st = calc_screen_tiles(pfrm);

        auto push_opt = [&](SystemString str, u8 y) {
            auto s = loadstr(pfrm, str);
            u8 mg = centered_text_margins(pfrm, utf8::len(s->c_str())) + 1;
            text_objs_.emplace_back(pfrm, s->c_str(), OverlayCoord{mg, y});
        };

        push_opt(SystemString::macro_enter, st.y - 6);
        push_opt(SystemString::macro_create_colony, st.y - 4);
        push_opt(SystemString::macro_set_name, st.y - 2);
    };


    auto v = pfrm.screen().get_view();
    auto cam = sector_map_display_pos(pfrm, selected_);

    if (state_ == State::create_colony and selected_colony_) {
        cam = (cam + sector_map_display_pos(pfrm, *selected_colony_));
        cam.x = cam.x.as_float() / 2;
        cam.y = cam.y.as_float() / 2; // Fixed point division broke for this
                                      // calculation.
    }

    Vec2<Float> target{cam.x.as_float() - (pfrm.screen().size().x / 2 - 16),
                       cam.y.as_float() - (pfrm.screen().size().y / 2 - 16)};

    camera_ = interpolate(target, camera_, delta * 0.0000081f);

    v.set_center(camera_);
    pfrm.screen().set_view(v);


    auto& m = *app.macrocosm();

    app.macrocosm()->data_->cloud_scroll_ += 0.000001f * delta;

    auto reveal_time = macro::reveal_time;
    auto wait_time = milliseconds(300);
    auto fade_time = milliseconds(350);
    if (fastload_) {
        reveal_time = milliseconds(40);
        wait_time = milliseconds(200);
        fade_time = milliseconds(250);
    }

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
        if (timer_ > wait_time) {
            timer_ = 0;
            state_ = State::fade_in;
        }
        break;

    case State::fade_in: {
        timer_ += delta;
        auto tm = fade_time;
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

        if (app.player().key_down(pfrm, Key::action_1)) {
            enter_opt_state();
            pfrm.speaker().play_sound("button_wooden", 3);
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            m.bind_sector(initial_sector_);
            clear_description();
            pfrm.fill_overlay(0);
            exit_ = true;
            pfrm.speaker().play_sound("button_wooden", 3);
        }

        if (app.player().key_down(pfrm, Key::left)) {
            if (auto s = m.bind_sector({s8(selected_.x - 1), selected_.y})) {
                selected_ = s->coordinate();
                describe_selected(pfrm, m);
                pfrm.speaker().play_sound("click_wooden", 2);
            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        }

        if (app.player().key_down(pfrm, Key::right)) {
            if (auto s = m.bind_sector({s8(selected_.x + 1), selected_.y})) {
                selected_ = s->coordinate();
                describe_selected(pfrm, m);
                pfrm.speaker().play_sound("click_wooden", 2);
            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        }

        if (app.player().key_down(pfrm, Key::up)) {
            if (auto s = m.bind_sector({selected_.x, s8(selected_.y - 1)})) {
                selected_ = s->coordinate();
                describe_selected(pfrm, m);
                pfrm.speaker().play_sound("click_wooden", 2);
            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        }

        if (app.player().key_down(pfrm, Key::down)) {
            if (auto s = m.bind_sector({selected_.x, s8(selected_.y + 1)})) {
                selected_ = s->coordinate();
                describe_selected(pfrm, m);
                pfrm.speaker().play_sound("click_wooden", 2);
            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        }
        break;

    case State::options: {
        if (app.player().key_down(pfrm, Key::down) and
            opt_cursor_ < text_objs_.size() - 1) {
            pfrm.speaker().play_sound("click_wooden", 2);
            ++opt_cursor_;
        }

        if (app.player().key_down(pfrm, Key::up) and opt_cursor_ > 0) {
            pfrm.speaker().play_sound("click_wooden", 2);
            --opt_cursor_;
        }


        Vec2<u8> sel_pos;

        for (u32 i = 0; i < text_objs_.size(); ++i) {

            u8 x = text_objs_[i].coord().x - 2;
            u8 y = text_objs_[i].coord().y;

            auto cursor_tile = 0;
            if (opt_cursor_ == i) {
                cursor_tile = 87;
                sel_pos = {x, y};
            }

            pfrm.set_tile(Layer::overlay, x, y, cursor_tile);
        }

        if (app.player().key_down(pfrm, Key::action_2)) {
            pfrm.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
            text_objs_.clear();
            describe_selected(pfrm, m);
            state_ = State::show;
            opt_cursor_ = 0;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            pfrm.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
            switch (opt_cursor_) {
            case 0:
                text_objs_.clear();
                pfrm.fill_overlay(0);
                exit_ = true;
                pfrm.speaker().play_sound("button_wooden", 2);
                break;

            case 1: {
                colony_create_slots_.clear();
                auto push = [&](s8 x, s8 y) {
                    if (not m.load_sector({x, y})) {
                        colony_create_slots_.push_back({x, y});
                    }
                };

                push(selected_.x - 1, selected_.y);
                push(selected_.x + 1, selected_.y);
                push(selected_.x, selected_.y - 1);
                push(selected_.x, selected_.y + 1);

                if (colony_create_slots_.empty()) {
                    pfrm.speaker().play_sound("beep_error", 2);
                } else {
                    auto textline = [&](const StringBuffer<48>& str, u8 y) {
                        text_objs_.emplace_back(
                            pfrm, str.c_str(), OverlayCoord{1, y});
                        u32 i = 0;

                        bool coin_icon = false;

                        auto s = str.c_str();
                        while (*s not_eq '\0') {
                            if (*s == '_') {
                                if (not coin_icon) {
                                    coin_icon = true;
                                    pfrm.set_tile(Layer::overlay, 1 + i, y, 88);
                                } else {
                                    pfrm.set_tile(Layer::overlay, 1 + i, y, 85);
                                }
                            }
                            ++s;
                            ++i;
                        }
                    };


                    auto cost = m.colony_cost();

                    state_ = State::create_colony;
                    text_objs_.clear();
                    pfrm.speaker().play_sound("button_wooden", 2);

                    textline(format(SYSTR(macro_colony_cost)->c_str(),
                                    cost.first,
                                    cost.second)
                                 .c_str(),
                             calc_screen_tiles(pfrm).y - 2);

                    text_objs_.emplace_back(
                        pfrm,
                        stringify(m.data_->p().coins_.get()).c_str(),
                        OverlayCoord{2, 3});
                    pfrm.set_tile(Layer::overlay, 1, 3, 88);

                    text_objs_.emplace_back(
                        pfrm,
                        stringify(m.sector().population()).c_str(),
                        OverlayCoord{2, 4});
                    pfrm.set_tile(Layer::overlay, 1, 4, 85);
                }

                selected_colony_.reset();

                break;
            }

            case 2:
                text_objs_.clear();
                pfrm.fill_overlay(0);
                pfrm.speaker().play_sound("button_wooden", 2);
                state_ = State::text_prompt;
                break;
            }

            opt_cursor_ = 0;
        }
        break;
    }

    case State::text_prompt: {
        const char* prompt = "Rename island:";

        auto receive = [&m](const char* text) {
            m.sector().set_name(text);
            return scene_pool::alloc<MacroverseScene>(true);
        };

        return scene_pool::alloc<TextEntryScene>(
            prompt, receive, 1, 12, m.sector().name().c_str());
        break;
    }

    case State::create_colony: {

        if (app.player().key_down(pfrm, Key::action_1)) {
            if (selected_colony_) {
                auto cost = m.colony_cost();
                if (m.data_->p().coins_.get() >= cost.first and
                    m.sector().population() >= cost.second) {

                    if (m.make_sector(*selected_colony_)) {

                        m.data_->p().coins_.set(m.data_->p().coins_.get() -
                                                cost.first);

                        m.sector().set_population(m.sector().population() -
                                                  cost.second);

                        colony_create_slots_.clear();

                        pfrm.set_tile(Layer::overlay, 1, 3, 0);
                        pfrm.set_tile(Layer::overlay, 1, 4, 0);
                        text_objs_.clear();
                        state_ = State::show;
                        describe_selected(pfrm, m);

                        if (not m.bind_sector(*selected_colony_)) {
                            Platform::fatal("logic error (bind sector)");
                        }

                        m.sector().set_block({3, 3, 0}, terrain::Type::terrain);
                        m.sector().set_block({3, 3, 1},
                                             terrain::Type::building);

                        pfrm.speaker().play_sound("button_wooden", 2);
                    }


                } else {
                    pfrm.speaker().play_sound("beep_error", 2);
                }
            }
        }

        if (app.player().key_down(pfrm, Key::action_2)) {
            pfrm.set_tile(Layer::overlay, 1, 3, 0);
            pfrm.set_tile(Layer::overlay, 1, 4, 0);
            enter_opt_state();
        }

        if (app.player().key_down(pfrm, Key::left)) {
            for (auto& s : colony_create_slots_) {
                if (s.x < selected_.x) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::right)) {
            for (auto& s : colony_create_slots_) {
                if (s.x > selected_.x) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::up)) {
            for (auto& s : colony_create_slots_) {
                if (s.y < selected_.y) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::down)) {
            for (auto& s : colony_create_slots_) {
                if (s.y > selected_.y) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        break;
    }
    }

    return null_scene();
}



void MacroverseScene::describe_selected(Platform& pfrm, macro::State& state)
{
    auto st = calc_screen_tiles(pfrm);

    if (text_objs_.size() not_eq 2) {
        text_objs_.clear();

        text_objs_.emplace_back(pfrm, OverlayCoord{1, (u8)(st.y - 4)});
        text_objs_.emplace_back(pfrm, OverlayCoord{2, (u8)(st.y - 2)});
    }

    for (int i = 0; i < text_objs_[0].len(); ++i) {
        pfrm.set_tile(Layer::overlay, 1 + i, st.y - 3, 0);
    }

    text_objs_[0].assign(state.sector().name().c_str());

    for (int i = 0; i < text_objs_[0].len(); ++i) {
        pfrm.set_tile(Layer::overlay, 1 + i, st.y - 3, 86);
    }

    pfrm.set_tile(Layer::overlay, 1, st.y - 2, 85);

    text_objs_[1].assign(state.sector().population());
}



void MacroverseScene::display(Platform& pfrm, App& app)
{
    if (exit_ or state_ == State::text_prompt) {
        return;
    }

    pfrm.system_call(
        "_prlx_macro",
        (void*)(intptr_t)(int)app.macrocosm()->data_->cloud_scroll_);



    auto draw_node = [&](macro::terrain::Sector& s) {
        Sprite spr;

        auto c = s.coordinate();

        const bool darkened = manhattan_length(c, selected_) > 1;

        if (state_ == State::reveal) {
            spr.set_mix({ColorConstant::rich_black,
                         u8(255 * (1.f - (float(timer_) / reveal_time)))});
        } else if ((state_ == State::options or
                    state_ == State::create_colony) and
                   c not_eq selected_) {
            return;
        }

        auto origin = sector_map_display_pos(pfrm, c);

        int t_start = 8;

        if (c == selected_) {
            t_start = 6;
        } else if ((int)state_ < (int)State::show) {
            return;
        } else if (darkened) {
            t_start = 12;
        }

        spr.set_position(origin);
        spr.set_texture_index(t_start);
        pfrm.screen().draw(spr);
        spr.set_texture_index(t_start + 1);
        spr.set_position({origin.x, origin.y + 32});
        pfrm.screen().draw(spr);
    };

    if (state_ == State::create_colony) {
        for (auto& slot : colony_create_slots_) {
            auto origin = sector_map_display_pos(pfrm, slot);
            origin.x += 8;
            origin.y += 8;

            Sprite spr;
            spr.set_size(Sprite::Size::w16_h32);
            spr.set_position(origin);

            if (selected_colony_ and *selected_colony_ == slot) {
                spr.set_texture_index(21);
            } else if (not selected_colony_) {
                spr.set_texture_index(20);
            } else {
                spr.set_texture_index(22);
            }


            pfrm.screen().draw(spr);
        }
    }

    auto& m = *app.macrocosm();

    draw_node(m.data_->origin_sector_);

    for (auto& s : m.data_->other_sectors_) {
        draw_node(*s);
    }
}



} // namespace skyland::macro
