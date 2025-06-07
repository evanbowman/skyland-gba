////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "boxedDialogScene.hpp"
#include "graphics/overlay.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "scriptHookScene.hpp"
#include "selectTutorialScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



extern SharedVariable text_scroll_direction;



const int y_start = 1;



void BoxedDialogScene::process_command()
{
    ++text_state_.current_word_;

    const char c = *text_state_.current_word_;

    ++text_state_.current_word_;

    if (*text_state_.current_word_ not_eq ':') {
        PLATFORM.fatal("invalid command format!");
    }

    ++text_state_.current_word_;

    auto parse_command_str = [&] {
        StringBuffer<32> str;
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

    case 'S': {
        conlang_ = parse_command_int();
        break;
    }

    case 'c': {
        data_->character_.name_ = parse_command_str();
        data_->character_.image_ = parse_command_int();

        if (data_->character_.image_) {
            auto st = calc_screen_tiles();

            data_->character_name_text_.emplace(OverlayCoord{1, u8(st.y - 7)});

            data_->character_name_text_->assign(
                data_->character_.name_.c_str(),
                Text::OptColors{
                    {custom_color(0xf3ea55), custom_color(0x232390)}});

            clear_textbox();
        }
        break;
    }

    case 's': {
        text_state_.speed_ = parse_command_int();
        break;
    }

    case 'f': {
        PLATFORM.screen().schedule_fade(parse_command_int() / 100.f);
        break;
    }

    case 'B': {
        parse_command_int();
        halt_text_ = true;
        break;
    }

    case 'b': {
        const auto bkg_name = parse_command_str();
        PLATFORM.screen().set_shader(passthrough_shader);
        PLATFORM.screen().set_view(View{});
        APP.camera().emplace<Camera>();
        for (int i = 0; i < 16; ++i) {
            for (int j = 0; j < 16; ++j) {
                PLATFORM.set_tile(Layer::map_0_ext, i, j, 0);
                PLATFORM.set_tile(Layer::map_1_ext, i, j, 0);
            }
        }
        for (int x = 0; x < 30; ++x) {
            for (int y = 0; y < 2; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 123);
            }
            PLATFORM.set_tile(Layer::overlay, x, 13, 123);
            PLATFORM.set_tile(Layer::overlay, x, 19, 123);
        }
        PLATFORM.load_tile1_texture(bkg_name.c_str());
        PLATFORM.set_scroll(Layer::map_1_ext, 0, 0);
        __draw_image(1, 0, 2, 30, 14, Layer::map_1);

        // Replace the textbox border with a tileset with an opaque dark
        // background.
        PLATFORM.load_overlay_chunk(83, 124, 8);
        img_view_ = true;

        int frames = 45;
        for (int i = 0; i < frames; ++i) {
            PLATFORM.screen().schedule_fade(1 - Float(i) / frames,
                                            ColorConstant::rich_black);
            PLATFORM.keyboard().poll();
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            if (ambience_) {
                if (not PLATFORM.speaker().is_sound_playing(ambience_)) {
                    PLATFORM.speaker().play_sound(ambience_, 9);
                }
            }
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

    case 't': {
        goto_tutorial_ = 1 + parse_command_int();
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



static bool punctuation_or_whitespace(char c)
{
    return (c == '!' or c == '\'' or c == ' ' or c == ',' or c == '?' or
            c == '.');
}



bool BoxedDialogScene::advance_text(Time delta, bool sfx)
{
    const auto delay = [&] {
        switch (text_state_.speed_) {
        default:
        case 0:
            return milliseconds(80);

        case 1:
            return milliseconds(160);

        case 2:
            return milliseconds(240);

        case 3:
            return milliseconds(320);
        }
    }();

    text_state_.timer_ += delta;

    const auto st = calc_screen_tiles();

    if (text_state_.timer_ > delay) {
        text_state_.timer_ = 0;

        if (sfx) {
            PLATFORM.speaker().play_sound("msg", 5);
        }

        if (text_state_.current_word_remaining_ == 0) {
            while (*text_state_.current_word_ == ' ') {
                text_state_.current_word_++;
                if (text_state_.pos_ < st.x - 2) {
                    text_state_.pos_ += 1;
                }
            }
            while (*text_state_.current_word_ == '<') {
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
            bool seen_char = false;
            utf8::scan(
                [&](const utf8::Codepoint& cp, const char*, int) {
                    if (done) {
                        return false;
                    }
                    if (cp == ' ' or cp == '<' or cp == '\0') {
                        done = true;
                    } else {
                        seen_char = true;
                        text_state_.current_word_remaining_++;
                    }
                    return true;
                },
                text_state_.current_word_,
                strlen(text_state_.current_word_));

            if (not seen_char and *text_state_.current_word_ == '\0') {
                display_mode_ = DisplayMode::key_released_check2;
                return true;
            }
        }

        // At this point, we know the length of the next space-delimited word in
        // the string. Now we can print stuff...

        static const int character_graphics_width = 4;

        const auto st = calc_screen_tiles();
        static const auto margin_sum = 4;
        const auto text_box_width = st.x - margin_sum;
        const auto remaining =
            ((text_box_width - text_state_.pos_) -
             (text_state_.line_ == 0 ? 0 : 2)) -
            (data_->character_.image_ ? character_graphics_width : 0);

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
            auto current_char = *text_state_.current_word_;
            if (conlang_ and not punctuation_or_whitespace(current_char)) {
                t = (current_char - 'a') + 150;
            } else {
                t = PLATFORM.map_glyph(cp, *mapping_info);
            }
        }

        const int y_offset = text_state_.line_ == 0 ? 4 + y_start : 2 + y_start;
        int x_offset =
            text_state_.pos_ + 2 +
            (data_->character_.image_ ? character_graphics_width : 0);

        if (text_scroll_direction == 1) {
            x_offset = st.x - (text_state_.pos_ + 2);
        }

        if (cp == '@') {
            PLATFORM.set_tile(Layer::overlay, x_offset, st.y - (y_offset), 146);
        } else if (cp == '*') {
            PLATFORM.set_tile(Layer::overlay, x_offset, st.y - (y_offset), 149);
        } else {
            PLATFORM.set_tile(Layer::overlay, x_offset, st.y - (y_offset), t);
        }

        text_state_.current_word_remaining_--;
        text_state_.current_word_ += bytes_consumed;
        text_state_.pos_++;

        if (*text_state_.current_word_ == '\0') {
            display_mode_ = DisplayMode::key_released_check2;
        }
    }

    return true;
}



void BoxedDialogScene::clear_textbox()
{
    const auto st = calc_screen_tiles();

    for (int x = 1; x < st.x - 1; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, st.y - (5 + y_start), 84);
        PLATFORM.set_tile(Layer::overlay, x, st.y - (4 + y_start), 82);
        PLATFORM.set_tile(Layer::overlay, x, st.y - (3 + y_start), 82);
        PLATFORM.set_tile(Layer::overlay, x, st.y - (2 + y_start), 82);
        PLATFORM.set_tile(Layer::overlay, x, st.y - (1 + y_start), 85);
    }

    PLATFORM.set_tile(Layer::overlay, 0, st.y - (4 + y_start), 89);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - (3 + y_start), 89);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - (2 + y_start), 89);

    PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (4 + y_start), 88);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (3 + y_start), 88);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (2 + y_start), 88);

    PLATFORM.set_tile(Layer::overlay, 0, st.y - (5 + y_start), 83);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - (1 + y_start), 90);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (5 + y_start), 87);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (1 + y_start), 86);

    text_state_.line_ = 0;
    text_state_.pos_ = 0;

    if (data_->character_.image_) {
        const auto img = (data_->character_.image_ - 1) * 16;
        PLATFORM.load_overlay_chunk(184, img, 16, "character_art");
        draw_image(184, 1, st.y - 6, 4, 4, Layer::overlay);

        for (int i = 4; i < data_->character_name_text_->len() + 1; ++i) {
            PLATFORM.set_tile(Layer::overlay, 1 + i, st.y - 6, 113);
        }

        for (int i = 1; i < data_->character_name_text_->len() + 1; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 8, 114);
        }


        PLATFORM.set_tile(Layer::overlay, 0, st.y - 5, 122);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 4, 122);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 3, 122);

        PLATFORM.set_tile(Layer::overlay, 0, st.y - 6, 119);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 7, 120);
        PLATFORM.set_tile(Layer::overlay, 0, st.y - 8, 121);


        PLATFORM.set_tile(Layer::overlay,
                          data_->character_name_text_->len() + 1,
                          st.y - 6,
                          115);

        PLATFORM.set_tile(Layer::overlay,
                          data_->character_name_text_->len() + 1,
                          st.y - 8,
                          116);

        PLATFORM.set_tile(Layer::overlay,
                          1 + data_->character_name_text_->len(),
                          st.y - 7,
                          112);
    }
}



void BoxedDialogScene::enter(Scene& prev)
{
    PLATFORM.fill_overlay(0);

    PLATFORM.load_overlay_texture("overlay_dialog");

    if (data_->character_.image_) {
        auto st = calc_screen_tiles();

        data_->character_name_text_.emplace(OverlayCoord{1, u8(st.y - 7)});

        data_->character_name_text_->assign(
            data_->character_.name_.c_str(),
            Text::OptColors{{custom_color(0xf3ea55), custom_color(0x232390)}});
    }

    clear_textbox();

    text_state_.current_word_remaining_ = 0;
    text_state_.current_word_ = buffer_->c_str();
    text_state_.timer_ = 0;
    text_state_.line_ = 0;
    text_state_.pos_ = 0;

    if (*text_state_.current_word_ == '<') {
        // Advance the timer, the text buffer starts with a command!
        text_state_.timer_ = milliseconds(81);
    }
}



void BoxedDialogScene::exit(Scene& prev)
{
    if (img_view_) {
        PLATFORM.fill_overlay(123);
        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
            }
        }
        PLATFORM.load_tile1_texture("tilesheet");
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.fill_overlay(0);
    }
    PLATFORM.fill_overlay(0);

    PLATFORM.load_overlay_texture("overlay");
    data_->coins_.reset();
    data_->character_name_text_.reset();
}



static lisp::Value* get_dialog_opt_list()
{
    // See init.lisp for structure of dialog-opts. The option list consists of a
    // list of name+callback pairs:
    // e.g.: '(("yes" . <lambda>) ("no" . <lambda>))

    auto opts = lisp::get_var("dialog-opts");
    if (opts and opts->type() not_eq lisp::Value::Type::nil) {
        return opts;
    }
    return nullptr;
}



ScenePtr BoxedDialogScene::update(Time delta)
{
    if (data_->coins_) {
        data_->coins_->update(delta);
    }

    auto is_action_key_down = [&] {
        return key_down<Key::action_1>() or
               state_bit_load(StateBit::regression);
    };


    if (ambience_) {
        if (not PLATFORM.speaker().is_sound_playing(ambience_)) {
            PLATFORM.speaker().play_sound(ambience_, 9);
        }
    }

    auto animate_moretext_icon = [&] {
        static const auto duration = milliseconds(500);
        text_state_.timer_ += delta;
        if (text_state_.timer_ > duration) {
            text_state_.timer_ = 0;
            const auto st = calc_screen_tiles();
            int x = st.x - 3;
            if (text_scroll_direction == 1) {
                if (data_->character_.image_) {
                    x = 6;
                } else {
                    x = 3;
                }
            }
            if (PLATFORM.get_tile(Layer::overlay, x, st.y - (2 + y_start)) ==
                91) {
                PLATFORM.set_tile(Layer::overlay, x, st.y - (2 + y_start), 92);
            } else {
                PLATFORM.set_tile(Layer::overlay, x, st.y - (2 + y_start), 91);
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
            if (text_state_.speed_ == 0 and allow_fastforward_ and
                (key_down<Key::action_2>() or is_action_key_down())) {

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

        if (key_down<Key::action_2>() or is_action_key_down()) {

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

    case DisplayMode::key_released_check2: {
        text_state_.timer_ = seconds(1);

        if (auto opts = get_dialog_opt_list()) {

            const auto st = calc_screen_tiles();

            // int opt_count = lisp::length(opts);

            int y = st.y - (7 + y_start);

            u32 max_text_len = 0;
            int opt_count = lisp::length(opts);

            lisp::l_foreach(opts, [&](lisp::Value* elem) {
                auto text = elem->cons().car()->string().value();
                auto t_len = utf8::len(text);

                if (t_len > max_text_len) {
                    max_text_len = t_len;
                }
            });


            for (int x = (st.x - 1) - (max_text_len + 3); x < st.x - 1; ++x) {
                for (int y = st.y - (6 + (opt_count * 2 - 1) + y_start);
                     y < st.y - 7;
                     ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 82);
                }
            }



            lisp::l_foreach(opts, [&](lisp::Value* elem) {
                auto text = elem->cons().car()->string().value();

                const u8 t_y = y;
                y -= 2;
                const u8 t_x = (st.x - 2) - max_text_len;

                OverlayCoord pos{t_x, t_y};
                data_->text_opts_.emplace_back(text, pos);
            });

            PLATFORM.set_tile(
                Layer::overlay, st.x - 1, st.y - (6 + y_start), 86);
            int i;
            for (i = 0; i < opt_count * 2 - 1; ++i) {
                PLATFORM.set_tile(
                    Layer::overlay, st.x - 1, st.y - (7 + i + y_start), 88);
                PLATFORM.set_tile(Layer::overlay,
                                  (st.x - 1) - (max_text_len + 4),
                                  st.y - (7 + i + y_start),
                                  89);
            }
            PLATFORM.set_tile(
                Layer::overlay, st.x - 1, st.y - (7 + i + y_start), 87);
            PLATFORM.set_tile(Layer::overlay,
                              (st.x - 1) - (max_text_len + 4),
                              st.y - (7 + i + y_start),
                              83);

            const bool overlap =
                max_text_len + 4 + 1 + data_->character_.name_.length() >= 30;

            const bool overlap_edge =
                max_text_len + 4 + 1 + data_->character_.name_.length() >= 29;

            u16 corner_tile = 90;

            if (overlap) {
                corner_tile = 132;
                PLATFORM.set_tile(Layer::overlay,
                                  (st.x - 1) - (max_text_len + 4),
                                  st.y - (6 + y_start) - 1,
                                  133);
            } else if (overlap_edge) {
                corner_tile = 132;
                PLATFORM.set_tile(Layer::overlay,
                                  (st.x - 1) - (max_text_len + 4),
                                  st.y - (6 + y_start) - 1,
                                  134);
            }

            PLATFORM.set_tile(Layer::overlay,
                              (st.x - 1) - (max_text_len + 4),
                              st.y - (6 + y_start),
                              corner_tile);

            for (int j = 0; j < (int)max_text_len + 3; ++j) {
                PLATFORM.set_tile(
                    Layer::overlay, (st.x - 2) - j, st.y - (6 + y_start), 85);
                PLATFORM.set_tile(Layer::overlay,
                                  (st.x - 2) - j,
                                  st.y - (7 + i + y_start),
                                  84);
            }

            // PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (9 + y_start), 88);
            // PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (8 + y_start), 88);
            // PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (7 + y_start), 88);
            // PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - (10 + y_start), 87);

            // PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - (9 + y_start), 89);
            // PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - (8 + y_start), 89);
            // PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - (7 + y_start), 89);
            // PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - (6 + y_start), 90);

            data_->coins_.emplace(OverlayCoord{1, 2},
                                  146,
                                  (int)APP.coins(),
                                  UIMetric::Align::left);

            display_mode_ = DisplayMode::y_n_wait;
            wait_ = 0;
        } else {
            display_mode_ = DisplayMode::done;
        }
    } break;

    case DisplayMode::y_n_wait:
        if (++wait_ == 18) {
            display_mode_ = DisplayMode::done;
        }
        break;

    case DisplayMode::done:
        if (get_dialog_opt_list()) {
            display_mode_ = DisplayMode::boolean_choice;
            text_state_.timer_ = seconds(1);
            choice_sel_ = data_->text_opts_.size() - 1;
            break;
        }
        animate_moretext_icon();
        if (is_action_key_down() or key_down<Key::action_2>()) {
            invoke_hook("on-dialog-closed");
            display_mode_ = DisplayMode::animate_out;
        }
        break;

    case DisplayMode::boolean_choice: {
        static const auto duration = milliseconds(400);
        text_state_.timer_ += delta;

        int cursor_x = 30;

        auto update_opt_cursor = [&] {
            for (u32 i = 0; i < data_->text_opts_.size(); ++i) {
                if (cursor_x > data_->text_opts_[i].coord().x - 2) {
                    cursor_x = data_->text_opts_[i].coord().x - 2;
                }
            }
            for (u32 i = 0; i < data_->text_opts_.size(); ++i) {
                PLATFORM.set_tile(Layer::overlay,
                                  cursor_x,
                                  data_->text_opts_[i].coord().y,
                                  82);
            }
            PLATFORM.set_tile(Layer::overlay,
                              cursor_x,
                              data_->text_opts_[choice_sel_].coord().y,
                              cursor_anim_ ? 110 : 109);
        };

        if (text_state_.timer_ > duration) {
            text_state_.timer_ = 0;
            cursor_anim_ = not cursor_anim_;
            update_opt_cursor();
        }

        if (key_down<Key::action_2>()) {
            choice_sel_ = 0;
            PLATFORM.speaker().play_sound("click", 1);
            update_opt_cursor();
        }

        if (key_down<Key::down>()) {
            if (choice_sel_ == 0) {
                choice_sel_ = data_->text_opts_.size() - 1;
            } else {
                --choice_sel_;
            }
            PLATFORM.speaker().play_sound("click", 1);
            update_opt_cursor();
        }

        if (key_down<Key::up>()) {
            if (choice_sel_ == (int)data_->text_opts_.size() - 1) {
                choice_sel_ = 0;
            } else {
                ++choice_sel_;
            }
            PLATFORM.speaker().play_sound("click", 1);
            update_opt_cursor();
        }

        if (is_action_key_down()) {
            text_state_.timer_ = 0;

            if (APP.game_speed() not_eq GameSpeed::stopped) {
                PLATFORM.speaker().play_sound("button_wooden", 3);
            }

            if (auto opts = get_dialog_opt_list()) {
                lisp::Protected old_dialog_opts(opts);
                lisp::dostring("(dialog-opts-reset)");

                lisp::Value* cb = lisp::get_list(old_dialog_opts, choice_sel_);

                lisp::safecall(cb->cons().cdr(), 0);
            }

            display_mode_ = DisplayMode::animate_out;
        }
        break;
    }

    case DisplayMode::animate_out:
        display_mode_ = DisplayMode::clear;
        if (img_view_) {
            int frames = 30;
            for (int i = 0; i < frames; ++i) {
                PLATFORM.screen().schedule_fade(
                    Float(i) / frames, ColorConstant::rich_black, true, true);
                PLATFORM.screen().clear();
                PLATFORM.screen().display();
                if (ambience_) {
                    if (not PLATFORM.speaker().is_sound_playing(ambience_)) {
                        PLATFORM.speaker().play_sound(ambience_, 9);
                    }
                }
            }
            PLATFORM.sleep(20);
        }
        PLATFORM.fill_overlay(0);
        break;

    case DisplayMode::clear:
        if (goto_tutorial_) {
            auto next = make_scene<SelectTutorialScene>();
            next->quick_select(goto_tutorial_ - 1);
            return next;
        }
        return data_->next_scene_();
    }

    return null_scene();
}



void BoxedDialogScene::display()
{
}



ScenePtr
dialog_prompt(SystemString systr, DeferredScene next, const char* ambience)
{
    lisp::set_var("on-dialog-closed", L_NIL);
    PLATFORM.screen().fade(0.95f);
    PLATFORM.screen().fade(1.f);
    auto dialog = allocate_dynamic<DialogString>("dialog-buffer");
    *dialog = loadstr(systr)->c_str();
    auto s = make_scene<BoxedDialogScene>(std::move(dialog));
    s->set_next_scene(next);
    s->ambience_ = ambience;
    return s;
}



} // namespace skyland
