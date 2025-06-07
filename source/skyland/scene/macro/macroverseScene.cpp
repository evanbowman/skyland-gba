////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "macroverseScene.hpp"
#include "abandonColonyScene.hpp"
#include "exchangeColonyScene.hpp"
#include "platform/color.hpp"
#include "platform/platform.hpp"
#include "selectorScene.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene/textEntryScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void MacroverseScene::enter(Scene& prev)
{
    if (not APP.macrocosm()) {
        PLATFORM.fatal("logic error while entering macroverse!");
    }

    if (fastload_) {
        PLATFORM.load_overlay_texture("overlay_challenges");
        auto str = SYSTR(start_menu_macroverse);
        Text heading(

            OverlayCoord{(u8)centered_text_margins(utf8::len(str->c_str())),
                         1});
        heading.assign(str->c_str());
        heading.__detach();
    }

    PLATFORM.load_sprite_texture("spritesheet_macroverse");

    PLATFORM.screen().schedule_fade(0.f);
    PLATFORM.screen().schedule_fade(1.f, ColorConstant::rich_black, false);

    PLATFORM.screen().set_view({});

    // Clear the background texture.
    PLATFORM.load_tile0_texture("macro_rendertexture");
    PLATFORM.load_tile1_texture("macro_rendertexture");

    selected_ = macrocosm().sector().coordinate();
    initial_sector_ = selected_;
}



ColorConstant fluid_shader(ShaderPalette p, ColorConstant k, int var, int index)
{
    auto blend = [](ColorConstant from, ColorConstant to, u8 interp) {
        const Color input(from);
        const Color k2(to);
        Color result(fast_interpolate(input.r_, k2.r_, interp),
                     fast_interpolate(input.g_, k2.g_, interp),
                     fast_interpolate(input.b_, k2.b_, interp));
        return result.hex();
    };

    if (p == ShaderPalette::background) {
        auto& s = bound_state();

        auto cyc = s.data_->p().day_night_cyc_.get();

        if (raster::globalstate::is_night) {
            if (cyc > night_frames - 256) {
                auto interp = (cyc - (night_frames - 256));
                switch (index) {
                case 1:
                    return blend(
                        custom_color(0xe6b673), custom_color(0xe61c05), interp);
                case 4:
                    return blend(
                        custom_color(0x7e96ed), custom_color(0x404e82), interp);
                }
            } else if (cyc > night_frames - 512) {
                auto interp = (cyc - (night_frames - 512));
                switch (index) {
                case 1:
                    return blend(k, custom_color(0xe61c05), 255 - interp);

                case 4:
                    return blend(custom_color(0x404e82), k, interp);
                }
            } else if (cyc < 256) {
                auto interp = 255 - cyc;
                switch (index) {
                case 1:
                    return blend(
                        custom_color(0xe6b673), custom_color(0xe61c05), interp);
                case 4:
                    return blend(
                        custom_color(0x7e96ed), custom_color(0x404e82), interp);
                }
            } else if (cyc < 512) {
                auto interp = (cyc - 256);

                switch (index) {
                case 1:
                    return blend(k, custom_color(0xe61c05), interp);

                case 4:
                    return blend(custom_color(0x404e82), k, 255 - interp);
                }
            }
        } else {
            if (cyc < 256) {
                auto interp = cyc;
                switch (index) {
                case 1:
                    return blend(k, custom_color(0xe6b673), interp);
                case 4:
                    return blend(k, custom_color(0x7e96ed), interp);
                }
            } else if (cyc > day_frames - 256) {
                auto interp = (cyc - (day_frames - 256));
                switch (index) {
                case 1:
                    return blend(k, custom_color(0xe6b673), 255 - interp);
                case 4:
                    return blend(k, custom_color(0x7e96ed), 255 - interp);
                }
            }
        }
    }

    if (p == ShaderPalette::tile0 or p == ShaderPalette::tile1) {
        if (not raster::globalstate::is_night) {
            auto& s = bound_state();
            auto cyc = s.data_->p().day_night_cyc_.get();

            if (cyc < 256 or cyc > day_frames - 256) {
                auto interp = cyc;
                if (cyc > day_frames - 256) {
                    interp = 255 - (cyc - (day_frames - 256));
                }
                switch (index) {
                case 4:
                    return blend(k, custom_color(0x67b94b), interp);

                case 5:
                    return blend(k, custom_color(0xcc590c), interp);

                case 8:
                    return blend(k, custom_color(0xdee86d), interp);

                case 9:
                    return blend(k, custom_color(0xf5f5cb), interp);

                case 13:
                    return blend(k, custom_color(0xe0eb9d), interp);
                }
            }
        }
        if (index == 11) {

            int v1 = (var & 0xff00) >> 8;

            const Color input(k);
            static constexpr const Color k2(custom_color(0x4f92ff));

            Color result(fast_interpolate(k2.r_, input.r_, v1),
                         fast_interpolate(k2.g_, input.g_, v1),
                         fast_interpolate(k2.b_, input.b_, v1));

            return result.hex();

        } else if (index == 3) {

            int v2 = (var & 0xff);

            const Color input(k);
            static constexpr const Color k2(custom_color(0xf7ad36));

            Color result(fast_interpolate(k2.r_, input.r_, v2),
                         fast_interpolate(k2.g_, input.g_, v2),
                         fast_interpolate(k2.b_, input.b_, v2));

            return result.hex();
        }
    }

    return k;
}



void MacroverseScene::exit(Scene& prev)
{
    auto& sector = macrocosm().sector();
    sector.repaint();

    PLATFORM.screen().set_shader(fluid_shader);

    PLATFORM.sleep(1);
    PLATFORM.screen().fade(1.f);


    PLATFORM.fill_overlay(0);

    if (text_objs_.size()) {
        Platform::fatal("text objs not cleared, tearing will occur!");
    }
    PLATFORM.load_overlay_texture("overlay");

    PLATFORM.load_sprite_texture("spritesheet_macro");

    PLATFORM.screen().set_view({});
}



static const auto reveal_time = milliseconds(90);



static Vec2<Fixnum> sector_map_display_pos(Vec2<s8> coord)
{
    const auto sw = PLATFORM.screen().size();

    Vec2<Fixnum> origin{Fixnum::from_integer(sw.x / 2 - 16),
                        Fixnum::from_integer(sw.y / 2 - 16)};
    origin.x += Fixnum::from_integer(coord.x * 32);
    origin.y += Fixnum::from_integer(coord.y * 38);

    return origin;
}



static const u8 layout_icon_y_start_row = 6;
static const int layout_icon_width = 6;



u8 layout_icon_margins()
{
    auto st = calc_screen_tiles();
    // six tile wide gfx.

    st.x -= layout_icon_width * 3;
    auto margin = st.x / 4;

    return margin;
}



void MacroverseScene::show_layout_text()
{
    text_objs_.clear();

    Text::OptColors col_1_colors;
    Text::OptColors col_2_colors;
    Text::OptColors col_3_colors;

    if (shape_ == terrain::Sector::Shape::cube) {
        col_2_colors = Text::OptColors{
            {custom_color(0xa6a6c1), ColorConstant::rich_black}};
        col_3_colors = Text::OptColors{
            {custom_color(0xa6a6c1), ColorConstant::rich_black}};
    } else if (shape_ == terrain::Sector::Shape::pancake) {
        col_1_colors = Text::OptColors{
            {custom_color(0xa6a6c1), ColorConstant::rich_black}};
        col_3_colors = Text::OptColors{
            {custom_color(0xa6a6c1), ColorConstant::rich_black}};
    } else {
        col_1_colors = Text::OptColors{
            {custom_color(0xa6a6c1), ColorConstant::rich_black}};
        col_2_colors = Text::OptColors{
            {custom_color(0xa6a6c1), ColorConstant::rich_black}};
    }

    auto sstr1 = SYSTR(macro_cube);
    const auto sl1 = utf8::len(sstr1->c_str());
    auto m = layout_icon_margins();

    text_objs_.emplace_back(

        OverlayCoord{(u8)((m + layout_icon_width / 2) - sl1 / 2),
                     (u8)(layout_icon_y_start_row + 8)});

    text_objs_.back().assign(sstr1->c_str(), col_1_colors);


    const char* str1_1 = "8x8x8";
    const auto sl1_1 = utf8::len(str1_1);

    text_objs_.emplace_back(

        OverlayCoord{(u8)((m + layout_icon_width / 2) - sl1_1 / 2),
                     (u8)(layout_icon_y_start_row + 10)});

    text_objs_.back().assign(str1_1, col_1_colors);


    auto sstr2 = SYSTR(macro_pancake);
    const auto sl2 = utf8::len(sstr2->c_str());

    text_objs_.emplace_back(OverlayCoord{
        (u8)(((m * 2 + layout_icon_width) + layout_icon_width / 2) - sl2 / 2),
        (u8)(layout_icon_y_start_row + 8)});

    text_objs_.back().assign(sstr2->c_str(), col_2_colors);


    const char* str2_2 = "12x12x3";
    const auto sl2_2 = utf8::len(str2_2);

    text_objs_.emplace_back(OverlayCoord{
        (u8)(((m * 2 + layout_icon_width) + layout_icon_width / 2) - sl2_2 / 2),
        (u8)(layout_icon_y_start_row + 10)});

    text_objs_.back().assign(str2_2, col_2_colors);



    auto sstr3 = SYSTR(macro_pillar);
    const auto sl3 = utf8::len(sstr3->c_str());

    text_objs_.emplace_back(OverlayCoord{
        (u8)(((m * 3 + layout_icon_width * 2) + layout_icon_width / 2) -
             sl3 / 2),
        (u8)(layout_icon_y_start_row + 8)});

    text_objs_.back().assign(sstr3->c_str(), col_3_colors);



    const char* str3_2 = "6x6x16";
    const auto sl3_2 = utf8::len(str2_2);

    text_objs_.emplace_back(OverlayCoord{
        (u8)(((m * 3 + layout_icon_width * 2) + layout_icon_width / 2) -
             sl3_2 / 2),
        (u8)(layout_icon_y_start_row + 10)});

    text_objs_.back().assign(str3_2, col_3_colors);
}



class EnterIslandScene : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.screen().schedule_fade(1.f);

        auto& m = macrocosm();
        m.sector().render();

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

        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            PLATFORM.screen().fade(0.f);
            auto next = make_scene<SelectorScene>();
            next->show_island_size();
            circ_radius_ = 0;
            return next;
        } else {
            auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
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



ScenePtr MacroverseScene::update(Time delta)
{
    if (not APP.macrocosm()) {
        Platform::fatal(format("% %", __FILE__, __LINE__).c_str());
    }

    auto& m = macrocosm();

    if (exit_) {
        m.sector().shadowcast();
        raster::globalstate::_recalc_depth_test.fill();
        if (abandon_) {
            auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
            *buffer = SYSTR(grav_collapse_started)->c_str();
            auto next = make_scene<BoxedDialogScene>(std::move(buffer));
            next->set_next_scene(make_deferred_scene<AbandonColonyScene>());
            m.sector().render();
            return next;
        } else {
            auto next = make_scene<EnterIslandScene>();
            return next;
        }
    }


    auto clear_description = [&] {
        text_objs_.clear();
        auto st_y = calc_screen_tiles().y;
        for (int y = st_y - 6; y < st_y; ++y) {
            for (int x = 0; x < calc_screen_tiles().x; ++x) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
    };


    auto enter_opt_state = [&] {
        clear_description();
        state_ = State::options;
        text_objs_.clear();

        auto st = calc_screen_tiles();

        auto push_opt = [&](SystemString str, u8 y) {
            auto s = loadstr(str);
            u8 mg = centered_text_margins(utf8::len(s->c_str())) + 1;
            text_objs_.emplace_back(s->c_str(), OverlayCoord{mg, y});
        };

        push_opt(SystemString::macro_enter, st.y - 6);
        push_opt(SystemString::macro_create_colony, st.y - 4);
        push_opt(SystemString::options, st.y - 2);
    };

    auto enter_opt2_state = [&] {
        clear_description();
        state_ = State::options_2;
        text_objs_.clear();

        auto st = calc_screen_tiles();

        auto push_opt = [&](SystemString str, u8 y) {
            auto s = loadstr(str);
            u8 mg = centered_text_margins(utf8::len(s->c_str())) + 1;
            text_objs_.emplace_back(s->c_str(), OverlayCoord{mg, y});
        };

        push_opt(SystemString::macro_set_name, st.y - 6);

        const bool origin = selected_ == m.data_->origin_sector_->coordinate();
        if (not origin) {
            push_opt(SystemString::macro_abandon, st.y - 4);
            push_opt(SystemString::macro_trade, st.y - 2);
        }
    };


    auto v = PLATFORM.screen().get_view();
    auto cam = sector_map_display_pos(selected_);

    if (state_ == State::create_colony and selected_colony_) {
        cam = (cam + sector_map_display_pos(*selected_colony_));
        cam.x = cam.x.as_float() / 2;
        cam.y = cam.y.as_float() / 2; // Fixed point division broke for this
                                      // calculation.
    }

    Vec2<Float> target{cam.x.as_float() - (PLATFORM.screen().size().x / 2 - 16),
                       cam.y.as_float() -
                           (PLATFORM.screen().size().y / 2 - 16)};

    camera_ = interpolate(target, camera_, delta * 0.0000081f);

    v.set_center(camera_);
    PLATFORM.screen().set_view(v);


    m.data_->cloud_scroll_ += 0.000001f * delta;

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

            PLATFORM.screen().set_shader([](ShaderPalette p,
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
            describe_selected(macrocosm());

        } else {
            const auto step = smoothstep(0.f, tm, timer_);
            const auto amount = 1.f - 0.6f * step;
            PLATFORM.screen().set_shader_argument(step * 255);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, false);
        }
        break;
    }

    case State::show:

        if (APP.player().key_down(Key::action_1)) {
            enter_opt_state();
            PLATFORM.speaker().play_sound("button_wooden", 3);
        } else if (APP.player().key_down(Key::action_2)) {
            m.bind_sector(initial_sector_);
            clear_description();
            PLATFORM.fill_overlay(0);
            exit_ = true;
            PLATFORM.speaker().play_sound("button_wooden", 3);
        }

        if (APP.player().key_down(Key::left)) {
            if (auto s = m.bind_sector({s8(selected_.x - 1), selected_.y})) {
                selected_ = s->coordinate();
                describe_selected(m);
                PLATFORM.speaker().play_sound("click_wooden", 2);
            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        }

        if (APP.player().key_down(Key::right)) {
            if (auto s = m.bind_sector({s8(selected_.x + 1), selected_.y})) {
                selected_ = s->coordinate();
                describe_selected(m);
                PLATFORM.speaker().play_sound("click_wooden", 2);
            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        }

        if (APP.player().key_down(Key::up)) {
            if (auto s = m.bind_sector({selected_.x, s8(selected_.y - 1)})) {
                selected_ = s->coordinate();
                describe_selected(m);
                PLATFORM.speaker().play_sound("click_wooden", 2);
            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        }

        if (APP.player().key_down(Key::down)) {
            if (auto s = m.bind_sector({selected_.x, s8(selected_.y + 1)})) {
                selected_ = s->coordinate();
                describe_selected(m);
                PLATFORM.speaker().play_sound("click_wooden", 2);
            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        }
        break;

    case State::options_2: {
        if (APP.player().key_down(Key::down) and
            opt_cursor_ < text_objs_.size() - 1) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
            ++opt_cursor_;
        }

        if (APP.player().key_down(Key::up) and opt_cursor_ > 0) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
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

            PLATFORM.set_tile(Layer::overlay, x, y, cursor_tile);
        }

        if (APP.player().key_down(Key::action_2)) {
            enter_opt_state();
            PLATFORM.speaker().play_sound("click_wooden", 2);
            opt_cursor_ = 2;
        } else if (APP.player().key_down(Key::action_1)) {
            PLATFORM.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
            switch (opt_cursor_) {
            case 0:
                text_objs_.clear();
                PLATFORM.fill_overlay(0);
                PLATFORM.speaker().play_sound("button_wooden", 2);
                state_ = State::text_prompt;
                break;

            case 2:
                text_objs_.clear();
                return make_scene<ExchangeColonyScene>(selected_);

            case 1: {
                exit_ = true;
                abandon_ = true;
                clear_description();

                PLATFORM.speaker().play_sound("button_wooden", 2);

                auto sz = m.sector().size();
                m.sector().set_block({u8(sz.x / 2), u8(sz.y / 2), u8(sz.z / 2)},
                                     terrain::Type::singularity);

                PLATFORM.speaker().set_music_volume(5);
            }
            }
        }

        break;
    }

    case State::options: {
        if (APP.player().key_down(Key::down) and
            opt_cursor_ < text_objs_.size() - 1) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
            ++opt_cursor_;
        }

        if (APP.player().key_down(Key::up) and opt_cursor_ > 0) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
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

            PLATFORM.set_tile(Layer::overlay, x, y, cursor_tile);
        }

        if (APP.player().key_down(Key::action_2)) {
            PLATFORM.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
            text_objs_.clear();
            describe_selected(m);
            state_ = State::show;
            opt_cursor_ = 0;
        }

        if (APP.player().key_down(Key::action_1)) {
            PLATFORM.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
            switch (opt_cursor_) {
            case 0:
                text_objs_.clear();
                PLATFORM.fill_overlay(0);
                exit_ = true;
                PLATFORM.speaker().play_sound("button_wooden", 2);
                break;

            case 1: {
                state_ = State::create_colony_options;
                text_objs_.clear();
                PLATFORM.speaker().play_sound("button_wooden", 2);
                clear_description();
                text_objs_.clear();

                auto st = calc_screen_tiles();

                auto push_opt = [&](SystemString str, u8 y, u8 n1, u8 n2) {
                    auto s = loadstr(str);
                    *s += " (";
                    *s += stringify(n1);
                    *s += "/";
                    *s += stringify(n2);
                    *s += ")";
                    u8 mg = centered_text_margins(utf8::len(s->c_str())) + 1;
                    text_objs_.emplace_back(s->c_str(), OverlayCoord{mg, y});
                };

                push_opt(SystemString::macro_fullsize_colony,
                         st.y - 6,
                         1 + m.data_->other_sectors_.size(),
                         EngineImpl::max_sectors);

                break;
            }

            case 2:
                text_objs_.clear();
                opt_cursor_ = 0;
                enter_opt2_state();
                PLATFORM.speaker().play_sound("button_wooden", 3);
                break;
            }

            opt_cursor_ = 0;
        }
        break;
    }

    case State::create_colony_options: {

        Vec2<u8> sel_pos;

        for (u32 i = 0; i < text_objs_.size(); ++i) {

            u8 x = text_objs_[i].coord().x - 2;
            u8 y = text_objs_[i].coord().y;

            auto cursor_tile = 0;

            PLATFORM.set_tile(Layer::overlay, x, y, cursor_tile);
        }

        if (APP.player().key_down(Key::up) or
            APP.player().key_down(Key::down)) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
        }

        if (APP.player().key_down(Key::action_2)) {
            text_objs_.clear();
            opt_cursor_ = 0;
            enter_opt_state();
            PLATFORM.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
            break;
        }

        if (APP.player().key_down(Key::action_1)) {
            PLATFORM.set_tile(Layer::overlay, sel_pos.x, sel_pos.y, 0);
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
                PLATFORM.speaker().play_sound("beep_error", 2);
            } else {
                auto textline = [&](const StringBuffer<48>& str, u8 y) {
                    text_objs_.emplace_back(str.c_str(), OverlayCoord{1, y});
                    u32 i = 0;

                    auto s = str.c_str();
                    while (*s not_eq '\0') {
                        if (*s == '_') {
                            PLATFORM.set_tile(Layer::overlay, 1 + i, y, 111);
                        }
                        ++s;
                        ++i;
                    }
                };


                auto cost = m.colony_cost();

                state_ = State::create_colony;
                text_objs_.clear();
                PLATFORM.speaker().play_sound("button_wooden", 2);

                textline(format(SYSTR(macro_colony_cost)->c_str(), cost.second)
                             .c_str(),
                         calc_screen_tiles().y - 2);

                text_objs_.emplace_back(

                    stringify(m.sector().productivity()).c_str(),
                    OverlayCoord{2, 4});
                PLATFORM.set_tile(Layer::overlay, 1, 4, 111);
            }

            selected_colony_.reset();
        }
        break;
    }

    case State::text_prompt: {

        auto receive = [&m](const char* text) {
            m.sector().set_name(text);
            return make_scene<MacroverseScene>(true);
        };

        const auto prompt = SYSTR(macro_rename_island);

        return make_scene<TextEntryScene>(
            prompt->c_str(), receive, 1, 12, m.sector().name().c_str());
        break;
    }

    case State::create_colony: {

        if (APP.player().key_down(Key::action_1)) {
            if (selected_colony_) {
                auto cost = m.colony_cost();
                if (m.sector().productivity() >= cost.second) {

                    PLATFORM.speaker().play_sound("button_wooden", 3);

                    // state_ = State::select_colony_layout;

                    // PLATFORM.set_tile(Layer::overlay, 1, 3, 0);
                    // PLATFORM.set_tile(Layer::overlay, 1, 4, 0);
                    // text_objs_.clear();
                    // show_layout_text();

                    text_objs_.clear();

                    shape_ = terrain::Sector::Shape::freebuild_wide;

                    if (m.make_sector(*selected_colony_, shape_)) {

                        m.sector().set_productivity(m.sector().productivity() -
                                                    cost.second);

                        colony_create_slots_.clear();


                        text_objs_.clear();

                        state_ = State::show;
                        describe_selected(m);

                        if (not m.bind_sector(*selected_colony_)) {
                            Platform::fatal("logic error (bind sector)");
                        }

                        m.sector().generate_terrain(100, 2);
                        m.sector().set_food(5);
                        m.sector().set_population(3);
                        m.sector().on_day_transition();
                        m.sector().set_food(5);

                        PLATFORM.speaker().play_sound("button_wooden", 2);
                        PLATFORM.delta_clock().reset();
                    }

                    break;

                } else {
                    PLATFORM.speaker().play_sound("beep_error", 2);
                }
            }
        }

        if (APP.player().key_down(Key::action_2)) {
            PLATFORM.set_tile(Layer::overlay, 1, 3, 0);
            PLATFORM.set_tile(Layer::overlay, 1, 4, 0);
            enter_opt_state();
        }

        if (APP.player().key_down(Key::left)) {
            for (auto& s : colony_create_slots_) {
                if (s.x < selected_.x) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        if (APP.player().key_down(Key::right)) {
            for (auto& s : colony_create_slots_) {
                if (s.x > selected_.x) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        if (APP.player().key_down(Key::up)) {
            for (auto& s : colony_create_slots_) {
                if (s.y < selected_.y) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        if (APP.player().key_down(Key::down)) {
            for (auto& s : colony_create_slots_) {
                if (s.y > selected_.y) {
                    selected_colony_ = s;
                    break;
                }
            }
        }

        break;
    }

    case State::select_colony_layout: {

        auto cost = m.colony_cost();

        if (APP.player().key_down(Key::left) and (int) shape_ > 0) {
            shape_ = (terrain::Sector::Shape)((int)shape_ - 1);
            PLATFORM.speaker().play_sound("click_wooden", 2);
            show_layout_text();
        } else if (APP.player().key_down(Key::right) and (int) shape_ < 2) {
            shape_ = (terrain::Sector::Shape)((int)shape_ + 1);
            PLATFORM.speaker().play_sound("click_wooden", 2);
            show_layout_text();
        }

        if (APP.player().key_down(Key::action_1)) {

            if (m.make_sector(*selected_colony_, shape_)) {

                m.sector().set_productivity(m.sector().productivity() -
                                            cost.second);

                colony_create_slots_.clear();


                text_objs_.clear();

                state_ = State::show;
                describe_selected(m);

                if (not m.bind_sector(*selected_colony_)) {
                    Platform::fatal("logic error (bind sector)");
                }

                m.sector().generate_terrain(160, 1);
                m.sector().set_population(1);
                m.sector().set_food(5);

                PLATFORM.speaker().play_sound("button_wooden", 2);
            }
        }

        break;
    }
    }

    return null_scene();
}



void MacroverseScene::describe_selected(macro::EngineImpl& state)
{
    auto st = calc_screen_tiles();

    if (text_objs_.size() not_eq 2) {
        text_objs_.clear();

        text_objs_.emplace_back(OverlayCoord{1, (u8)(st.y - 4)});
        text_objs_.emplace_back(OverlayCoord{2, (u8)(st.y - 2)});
    }

    for (int i = 0; i < text_objs_[0].len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, 1 + i, st.y - 3, 0);
    }

    text_objs_[0].assign(state.sector().name().c_str());

    for (int i = 0; i < text_objs_[0].len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, 1 + i, st.y - 3, 86);
    }

    PLATFORM.set_tile(Layer::overlay, 1, st.y - 2, 85);

    text_objs_[1].assign(state.sector().population());
}



void MacroverseScene::display()
{
    if (exit_ or state_ == State::text_prompt) {
        return;
    }

    PLATFORM_EXTENSION(update_parallax_macro, macrocosm().data_->cloud_scroll_);


    if (state_ == State::select_colony_layout) {

        auto origin = camera_.cast<Fixnum>();
        origin.y += Fixnum::from_integer(8 * layout_icon_y_start_row);

        auto draw = [](int t_start, Vec2<Fixnum> origin) {
            Sprite spr;
            spr.set_texture_index(t_start++);
            spr.set_position(origin);
            PLATFORM.screen().draw(spr);

            origin.x += 32.0_fixed;
            spr.set_texture_index(t_start++);
            spr.set_position(origin);
            PLATFORM.screen().draw(spr);

            origin.x -= 32.0_fixed;
            origin.y += 32.0_fixed;
            spr.set_texture_index(t_start++);
            spr.set_position(origin);
            PLATFORM.screen().draw(spr);

            origin.x += 32.0_fixed;
            spr.set_texture_index(t_start);
            spr.set_position(origin);
            PLATFORM.screen().draw(spr);
        };


        auto mrgn = layout_icon_margins();

        int ic1 = 14;
        int ic2 = 18;
        int ic3 = 30;
        if (shape_ == terrain::Sector::Shape::cube) {
            ic2 = 22;
            ic3 = 34;
        } else if (shape_ == terrain::Sector::Shape::pancake) {
            ic1 = 26;
            ic3 = 34;
        } else {
            ic1 = 26;
            ic2 = 22;
        }

        draw(ic1,
             origin + Vec2<Fixnum>{Fixnum::from_integer(mrgn * 8), 0.0_fixed});
        draw(ic2,
             origin + Vec2<Fixnum>{Fixnum::from_integer(
                                       (mrgn * 2 + layout_icon_width) * 8),
                                   0.0_fixed});
        draw(ic3,
             origin + Vec2<Fixnum>{Fixnum::from_integer(
                                       (mrgn * 3 + layout_icon_width * 2) * 8),
                                   Fixnum::from_integer(-16)});

        return;
    }


    auto draw_node = [&](macro::terrain::Sector& s) {
        Sprite spr;

        auto c = s.coordinate();

        const bool darkened = manhattan_length(c, selected_) > 1;

        if (manhattan_length(c, selected_) > 3) {
            return;
        } else if (manhattan_length(c, selected_) > 2) {
            spr.set_alpha(Sprite::Alpha::translucent);
        }

        if (state_ == State::reveal) {
            spr.set_mix({ColorConstant::rich_black,
                         u8(255 * (1.f - (float(timer_) / reveal_time)))});
        } else if ((state_ == State::options or state_ == State::options_2 or
                    state_ == State::create_colony_options or
                    state_ == State::create_colony) and
                   c not_eq selected_) {
            return;
        }

        auto origin = sector_map_display_pos(c);

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
        PLATFORM.screen().draw(spr);
        spr.set_texture_index(t_start + 1);
        spr.set_position({origin.x, origin.y + 32.0_fixed});
        PLATFORM.screen().draw(spr);
    };


    auto draw_rng_node = [&](const Vec2<s8>& c) {
        Sprite spr;

        const bool darkened = manhattan_length(c, selected_) > 1;

        if (manhattan_length(c, selected_) > 3) {
            return;
        } else if (manhattan_length(c, selected_) > 2) {
            spr.set_alpha(Sprite::Alpha::translucent);
        }

        if (state_ == State::reveal) {
            spr.set_mix({ColorConstant::rich_black,
                         u8(255 * (1.f - (float(timer_) / reveal_time)))});
        } else if ((state_ == State::options or state_ == State::options_2 or
                    state_ == State::create_colony_options or
                    state_ == State::create_colony) and
                   c not_eq selected_) {
            return;
        }

        auto origin = sector_map_display_pos(c);
        origin.x += 8.0_fixed;
        origin.y += 8.0_fixed;

        int t_start = 79;

        spr.set_size(Sprite::Size::w16_h32);

        if (c == selected_) {
            t_start = 81;
        } else if ((int)state_ < (int)State::show) {
            return;
        } else if (darkened) {
            t_start = 80;
        }

        spr.set_position(origin);
        spr.set_texture_index(t_start);
        PLATFORM.screen().draw(spr);
    };


    if (state_ == State::create_colony) {
        for (auto& slot : colony_create_slots_) {
            auto origin = sector_map_display_pos(slot);
            origin.x += 8.0_fixed;
            origin.y += 8.0_fixed;

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


            PLATFORM.screen().draw(spr);
        }
    }

    auto& m = macrocosm();

    draw_node(*m.data_->origin_sector_);

    for (auto& s : m.data_->other_sectors_) {
        draw_node(*s);
    }

    (void)draw_rng_node;
    // for (auto& s : m.data_->random_encounters_.sectors_) {
    //     draw_rng_node(s.coordinate_);
    // }
}



} // namespace skyland::macro
