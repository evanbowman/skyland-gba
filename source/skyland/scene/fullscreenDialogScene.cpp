////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "fullscreenDialogScene.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



SHARED_VARIABLE(text_scroll_direction);



const int y_begin = 9;



u8 FullscreenDialogScene::y_start() const
{
    if (img_view_) {
        return y_begin - 8;
    } else {
        return y_begin;
    }
}



bool FullscreenDialogScene::advance_text(Time delta, bool sfx)
{
    const auto delay = milliseconds(80);

    text_state_.timer_ += delta;

    const auto st = calc_screen_tiles();

    if (text_state_.timer_ > delay) {
        text_state_.timer_ = 0;

        if (text_state_.current_word_remaining_ == 0) {
            while (*text_state_.current_word_ == ' ') {
                text_state_.current_word_++;
                if (text_state_.pos_ < st.x - 2) {
                    text_state_.pos_ += 1;
                }
            }
            if (*text_state_.current_word_ == '<') {
                process_command();
            }
            if (halt_text_) {
                halt_text_ = false;
                return false;
            }
            if (text_state_.timer_ < 0) {
                return true;
            }
            bool done = false;
            utf8::scan(
                [&](const utf8::Codepoint& cp, const char*, int) {
                    if (done) {
                        return false;
                    }
                    if (cp == ' ') {
                        done = true;
                    } else {
                        text_state_.current_word_remaining_++;
                    }
                    return true;
                },
                text_state_.current_word_,
                strlen(text_state_.current_word_));
        }

        if (sfx) {
            PLATFORM.speaker().play_sound("msg", 5);
        }

        // At this point, we know the length of the next space-delimited word in
        // the string. Now we can print stuff...

        const auto st = calc_screen_tiles();
        static const auto margin_sum = 4;
        const auto text_box_width = st.x - margin_sum;
        const auto remaining = (text_box_width - text_state_.pos_) -
                               (text_state_.line_ == 0 ? 0 : 2);

        if (text_state_.current_word_remaining_ > st.x) {
            text_state_.current_word_remaining_ = remaining;
        } else if (remaining < text_state_.current_word_remaining_) {
            if (text_state_.line_ == 0) {
                text_state_.line_++;
                text_state_.pos_ = 0;
                return true;
            } else {
                return false;
            }
        }

        int bytes_consumed = 0;
        const auto cp = utf8::getc(text_state_.current_word_, &bytes_consumed);

        const auto mapping_info = locale_texture_map()(cp);

        u16 t = 495; // bad glyph, FIXME: add a constant

        if (mapping_info) {
            t = PLATFORM.map_glyph(cp, *mapping_info);
        }

        const int y_offset =
            text_state_.line_ == 0 ? 4 + y_start() : 2 + y_start();
        int x_offset = text_state_.pos_ + 2;

        if (text_scroll_direction == 1) {
            x_offset = st.x - x_offset;
        }

        PLATFORM.set_tile(Layer::overlay, x_offset, st.y - (y_offset), t);

        text_state_.current_word_remaining_--;
        text_state_.current_word_ += bytes_consumed;
        text_state_.pos_++;

        if (*text_state_.current_word_ == '\0') {
            display_mode_ = DisplayMode::key_released_check2;
        }
    }

    return true;
}



void FullscreenDialogScene::clear_textbox()
{
    const auto st = calc_screen_tiles();

    for (int x = 1; x < st.x - 1; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, st.y - (4 + y_start()), 82);
        PLATFORM.set_tile(Layer::overlay, x, st.y - (3 + y_start()), 82);
        PLATFORM.set_tile(Layer::overlay, x, st.y - (2 + y_start()), 82);
    }

    text_state_.line_ = 0;
    text_state_.pos_ = 0;
}



void FullscreenDialogScene::enter(Scene& prev)
{
    PLATFORM.load_overlay_texture("overlay_dialog_fullscreen");

    PLATFORM.fill_overlay(0);

    PLATFORM.screen().fade(1.f, custom_color(0), {}, true, false);
    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, false);

    clear_textbox();

    text_state_.current_word_remaining_ = 0;
    text_state_.current_word_ = buffer_->c_str();
    text_state_.timer_ = 0;
    text_state_.line_ = 0;
    text_state_.pos_ = 0;
}



void FullscreenDialogScene::exit(Scene& prev)
{
    if (img_view_) {
        int frames = 25;
        for (int i = 0; i < frames; ++i) {
            PLATFORM.screen().schedule_fade(
                Float(i) / (frames - 1), ColorConstant::rich_black, true, true);
            PLATFORM.keyboard().poll();
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
        }

        PLATFORM.sleep(10);

        PLATFORM.fill_overlay(0);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.set_overlay_origin(0, 0);
        PLATFORM.load_overlay_texture("overlay");
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.fill_overlay(0);
        if (APP.opponent_island()) {
            show_island_exterior(APP.opponent_island());
        }
        APP.player_island().repaint();
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.delta_clock().reset();
    } else {
        PLATFORM.load_overlay_texture("overlay");
    }


    PLATFORM.fill_overlay(0);
}



ScenePtr FullscreenDialogScene::update(Time delta)
{
    auto animate_moretext_icon = [&] {
        static const auto duration = milliseconds(500);
        text_state_.timer_ += delta;
        if (text_state_.timer_ > duration) {
            text_state_.timer_ = 0;
            const auto st = calc_screen_tiles();
            int x = st.x - 3;
            if (text_scroll_direction == 1) {
                x = 3;
            }
            if (PLATFORM.get_tile(Layer::overlay, x, st.y - (2 + y_start())) ==
                91) {
                PLATFORM.set_tile(
                    Layer::overlay, x, st.y - (2 + y_start()), 92);
            } else {
                PLATFORM.set_tile(
                    Layer::overlay, x, st.y - (2 + y_start()), 91);
            }
        }
    };

    switch (display_mode_) {
    case DisplayMode::animate_in:
        display_mode_ = DisplayMode::busy;
        break;

    case DisplayMode::busy: {

        const bool text_busy = advance_text(delta, true);

        if (not text_busy) {
            display_mode_ = DisplayMode::key_released_check1;
        } else {
            if (key_down<Key::action_2>() or key_down<Key::action_1>()) {

                while (advance_text(delta, false)) {
                    if (display_mode_ not_eq DisplayMode::busy) {
                        break;
                    }
                }

                if (display_mode_ == DisplayMode::busy) {
                    display_mode_ = DisplayMode::key_released_check1;
                }
            }
        }
    } break;

    case DisplayMode::wait: {
        animate_moretext_icon();

        if (key_down<Key::action_2>() or key_down<Key::action_1>()) {

            text_state_.timer_ = 0;

            clear_textbox();
            display_mode_ = DisplayMode::busy;
        }
        break;
    }

    case DisplayMode::key_released_check1:
        // if (key_down<Key::action_2>() or
        //     key_down<Key::action_1>()) {

        text_state_.timer_ = seconds(1);
        display_mode_ = DisplayMode::wait;
        // }
        break;

    case DisplayMode::key_released_check2:
        // if (key_down<Key::action_2>() or
        //     key_down<Key::action_1>()) {

        text_state_.timer_ = seconds(1);
        display_mode_ = DisplayMode::done;
        // }
        break;

    case DisplayMode::done:
        animate_moretext_icon();
        if (key_down<Key::action_2>() or key_down<Key::action_1>()) {

            // if (text_[1] not_eq LocaleString::empty) {
            //     ++text_;
            //     init_text(*text_);
            //     display_mode_ = DisplayMode::animate_in;
            // } else {
            display_mode_ = DisplayMode::animate_out;
            // }
        }
        break;

    case DisplayMode::animate_out:
        display_mode_ = DisplayMode::clear;
        if (not img_view_) {
            PLATFORM.fill_overlay(0);
        }

        break;

    case DisplayMode::clear:
        return next_scene_();
    }

    return null_scene();
}



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void FullscreenDialogScene::process_command()
{
    ++text_state_.current_word_;

    const char c = *text_state_.current_word_;

    ++text_state_.current_word_;

    if (*text_state_.current_word_ not_eq ':') {
        PLATFORM.fatal("invalid command format!");
    }

    ++text_state_.current_word_;

    auto parse_command_str = [&] {
        StringBuffer<64> str;
        while (*text_state_.current_word_ not_eq ':' and
               *text_state_.current_word_ not_eq '>') {
            if (*text_state_.current_word_ == '\0') {
                PLATFORM.fatal("Unexpected null byte in command sequence!");
            }
            str.push_back(*text_state_.current_word_);
            ++text_state_.current_word_;
        }
        ++text_state_.current_word_;
        return str;
    };

    auto parse_command_int = [&] {
        auto str = parse_command_str();
        s32 result = 0;
        for (u32 i = 0; i < str.length(); ++i) {
            result = result * 10 + (str[i] - '0');
        }
        return result;
    };

    auto parse_command_noarg = [&] {
        if (*text_state_.current_word_ not_eq '>') {
            PLATFORM.fatal("invalid command format, expected >");
        }

        ++text_state_.current_word_;
    };


    switch (c) {
    case '\0':
        PLATFORM.fatal("Invalid null byte in command sequence!");

    case 'c': {
        parse_command_str();
        parse_command_int();
        break;
    }

    case 'B': {
        parse_command_int();
        halt_text_ = true;
        break;
    }

    case 's': {
        parse_command_int(); // TODO
        break;
    }

    case 'f': {
        PLATFORM.screen().schedule_fade(parse_command_int() / 100.f);
        break;
    }

    case 'b': {
        if (img_view_) {
            // We're already viewing an image. Fade it out before swapping the
            // texture.
            int frames = 25;
            for (int i = 0; i < frames; ++i) {
                PLATFORM.screen().schedule_fade(Float(i) / frames);
                PLATFORM.keyboard().poll();
                PLATFORM.screen().clear();
                PLATFORM.screen().display();
            }
        }

        const auto bkg_name = parse_command_str();
        PLATFORM.screen().set_shader(passthrough_shader);
        PLATFORM.screen().set_view(View{});

        PLATFORM.load_tile1_texture(bkg_name.c_str());
        PLATFORM.set_scroll(Layer::map_1_ext, 0, 0);
        __draw_image(0, 3, 3, 24, 10, Layer::map_1);
        PLATFORM.screen().schedule_fade(0);

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                PLATFORM.set_tile(Layer::map_0_ext, x, y, 0);
            }
        }

        // // Replace the textbox border with a tileset with an opaque dark
        // // background.
        // PLATFORM.load_overlay_chunk(83, 124, 8);
        img_view_ = true;

        PLATFORM.fill_overlay(0);

        for (int x = 0; x < 30; ++x) {
            for (int y = 0; y < 20; ++y) {
                if (y < 3 or y > 12 or x < 3 or x > 26) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 82);
                }
            }
        }

        int frames = 45;
        for (int i = 0; i < frames; ++i) {
            PLATFORM.screen().schedule_fade(1 - Float(i) / frames,
                                            ColorConstant::rich_black);
            PLATFORM.keyboard().poll();
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
        }

        break;
    }

    case 'd': {
        text_state_.timer_ = -milliseconds(parse_command_int());
        break;
    }

    case 'r': {
        parse_command_noarg();
        break;
    }

    case 'm': {
        PLATFORM.speaker().set_music_volume(parse_command_int());
        break;
    }


    default:
        PLATFORM.fatal(format("Invald command %", c).c_str());
    }

    while (*text_state_.current_word_ == ' ') {
        ++text_state_.current_word_;
    }
}



} // namespace skyland
