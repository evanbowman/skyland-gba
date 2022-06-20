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



extern SharedVariable text_scroll_direction;



const int y_start = 1;



void BoxedDialogScene::process_command(Platform& pfrm, App& app)
{
    ++text_state_.current_word_;

    const char c = *text_state_.current_word_;

    ++text_state_.current_word_;

    if (*text_state_.current_word_ not_eq ':') {
        pfrm.fatal("invalid command format!");
    }

    ++text_state_.current_word_;

    auto parse_command_str = [&] {
        StringBuffer<32> str;
        while (*text_state_.current_word_ not_eq ':' and
               *text_state_.current_word_ not_eq '>') {
            if (*text_state_.current_word_ == '\0') {
                pfrm.fatal("Unexpected null byte in command sequence!");
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
            pfrm.fatal("invalid command format, expected >");
        }

        ++text_state_.current_word_;
    };


    switch (c) {
    case '\0':
        pfrm.fatal("Invalid null byte in command sequence!");

    case 'c': {
        data_->character_.name_ = parse_command_str();
        data_->character_.image_ = parse_command_int();

        if (data_->character_.image_) {
            auto st = calc_screen_tiles(pfrm);

            character_name_text_.emplace(pfrm, OverlayCoord{1, u8(st.y - 7)});

            character_name_text_->assign(
                data_->character_.name_.c_str(),
                Text::OptColors{
                    {custom_color(0xf3ea55), custom_color(0x232390)}});

            clear_textbox(pfrm);
        }
        break;
    }

    case 's': {
        text_state_.speed_ = parse_command_int();
        break;
    }

    case 'f': {
        pfrm.screen().schedule_fade(parse_command_int() / 100.f);
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

    default:
        pfrm.fatal(format("Invald command %", c).c_str());
    }

    while (*text_state_.current_word_ == ' ') {
        ++text_state_.current_word_;
    }
}



bool BoxedDialogScene::advance_text(Platform& pfrm,
                                    App& app,
                                    Microseconds delta,
                                    bool sfx)
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

    const auto st = calc_screen_tiles(pfrm);

    if (text_state_.timer_ > delay) {
        text_state_.timer_ = 0;

        if (sfx) {
            pfrm.speaker().play_sound("msg", 5);
        }

        if (text_state_.current_word_remaining_ == 0) {
            while (*text_state_.current_word_ == ' ') {
                text_state_.current_word_++;
                if (text_state_.pos_ < st.x - 2) {
                    text_state_.pos_ += 1;
                }
            }
            if (*text_state_.current_word_ == '<') {
                process_command(pfrm, app);
            }
            if (text_state_.timer_ < 0) {
                return true;
            }
            bool done = false;
            utf8::scan(
                [&](const utf8::Codepoint& cp, const char*, int) {
                    if (done) {
                        return;
                    }
                    if (cp == ' ' or cp == '<') {
                        done = true;
                    } else {
                        text_state_.current_word_remaining_++;
                    }
                },
                text_state_.current_word_,
                str_len(text_state_.current_word_));
        }

        // At this point, we know the length of the next space-delimited word in
        // the string. Now we can print stuff...

        static const int character_graphics_width = 4;

        const auto st = calc_screen_tiles(pfrm);
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
            t = pfrm.map_glyph(cp, *mapping_info);
        }

        const int y_offset = text_state_.line_ == 0 ? 4 + y_start : 2 + y_start;
        int x_offset =
            text_state_.pos_ + 2 +
            (data_->character_.image_ ? character_graphics_width : 0);

        if (text_scroll_direction == 1) {
            x_offset = st.x - (text_state_.pos_ + 2);
        }

        if (cp == '@') {
            pfrm.set_tile(Layer::overlay, x_offset, st.y - (y_offset), 146);
        } else if (cp == '*') {
            pfrm.set_tile(Layer::overlay, x_offset, st.y - (y_offset), 149);
        } else {
            pfrm.set_tile(Layer::overlay, x_offset, st.y - (y_offset), t);
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



void BoxedDialogScene::clear_textbox(Platform& pfrm)
{
    const auto st = calc_screen_tiles(pfrm);

    for (int x = 1; x < st.x - 1; ++x) {
        pfrm.set_tile(Layer::overlay, x, st.y - (5 + y_start), 84);
        pfrm.set_tile(Layer::overlay, x, st.y - (4 + y_start), 82);
        pfrm.set_tile(Layer::overlay, x, st.y - (3 + y_start), 82);
        pfrm.set_tile(Layer::overlay, x, st.y - (2 + y_start), 82);
        pfrm.set_tile(Layer::overlay, x, st.y - (1 + y_start), 85);
    }

    pfrm.set_tile(Layer::overlay, 0, st.y - (4 + y_start), 89);
    pfrm.set_tile(Layer::overlay, 0, st.y - (3 + y_start), 89);
    pfrm.set_tile(Layer::overlay, 0, st.y - (2 + y_start), 89);

    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (4 + y_start), 88);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (3 + y_start), 88);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (2 + y_start), 88);

    pfrm.set_tile(Layer::overlay, 0, st.y - (5 + y_start), 83);
    pfrm.set_tile(Layer::overlay, 0, st.y - (1 + y_start), 90);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (5 + y_start), 87);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (1 + y_start), 86);

    text_state_.line_ = 0;
    text_state_.pos_ = 0;

    if (data_->character_.image_) {
        const auto img = 200 + (data_->character_.image_ - 1) * 16;
        draw_image(pfrm, img, 1, st.y - 6, 4, 4, Layer::overlay);

        for (int i = 4; i < character_name_text_->len() + 1; ++i) {
            pfrm.set_tile(Layer::overlay, 1 + i, st.y - 6, 113);
        }

        for (int i = 1; i < character_name_text_->len() + 1; ++i) {
            pfrm.set_tile(Layer::overlay, i, st.y - 8, 114);
        }


        pfrm.set_tile(Layer::overlay, 0, st.y - 5, 122);
        pfrm.set_tile(Layer::overlay, 0, st.y - 4, 122);
        pfrm.set_tile(Layer::overlay, 0, st.y - 3, 122);

        pfrm.set_tile(Layer::overlay, 0, st.y - 6, 119);
        pfrm.set_tile(Layer::overlay, 0, st.y - 7, 120);
        pfrm.set_tile(Layer::overlay, 0, st.y - 8, 121);


        pfrm.set_tile(
            Layer::overlay, character_name_text_->len() + 1, st.y - 6, 115);

        pfrm.set_tile(
            Layer::overlay, character_name_text_->len() + 1, st.y - 8, 116);

        pfrm.set_tile(
            Layer::overlay, 1 + character_name_text_->len(), st.y - 7, 112);
    }
}



void BoxedDialogScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);

    pfrm.load_overlay_texture("overlay_dialog");

    if (data_->character_.image_) {
        auto st = calc_screen_tiles(pfrm);

        character_name_text_.emplace(pfrm, OverlayCoord{1, u8(st.y - 7)});

        character_name_text_->assign(
            data_->character_.name_.c_str(),
            Text::OptColors{{custom_color(0xf3ea55), custom_color(0x232390)}});
    }

    clear_textbox(pfrm);

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



void BoxedDialogScene::exit(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);

    pfrm.load_overlay_texture("overlay");
    coins_.reset();
    character_name_text_.reset();
}



ScenePtr<Scene>
BoxedDialogScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (coins_) {
        coins_->update(pfrm, delta);
    }

    auto animate_moretext_icon = [&] {
        static const auto duration = milliseconds(500);
        text_state_.timer_ += delta;
        if (text_state_.timer_ > duration) {
            text_state_.timer_ = 0;
            const auto st = calc_screen_tiles(pfrm);
            int x = st.x - 3;
            if (text_scroll_direction == 1) {
                if (data_->character_.image_) {
                    x = 6;
                } else {
                    x = 3;
                }
            }
            if (pfrm.get_tile(Layer::overlay, x, st.y - (2 + y_start)) == 91) {
                pfrm.set_tile(Layer::overlay, x, st.y - (2 + y_start), 92);
            } else {
                pfrm.set_tile(Layer::overlay, x, st.y - (2 + y_start), 91);
            }
        }
    };

    switch (display_mode_) {
    case DisplayMode::animate_in:
        display_mode_ = DisplayMode::busy;
        break;

    case DisplayMode::busy: {

        const bool text_busy = advance_text(pfrm, app, delta, true);

        if (not text_busy) {
            display_mode_ = DisplayMode::key_released_check1;
        } else {
            if (text_state_.speed_ == 0 and
                (key_down<Key::action_2>(pfrm) or
                 key_down<Key::action_1>(pfrm))) {

                while (advance_text(pfrm, app, delta, false)) {
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

        if (key_down<Key::action_2>(pfrm) or key_down<Key::action_1>(pfrm)) {

            text_state_.timer_ = 0;

            clear_textbox(pfrm);
            display_mode_ = DisplayMode::busy;
        }
        break;
    }

    case DisplayMode::key_released_check1:
        // if (key_down<Key::action_2>(pfrm) or
        //     key_down<Key::action_1>(pfrm)) {

        text_state_.timer_ = seconds(1);
        display_mode_ = DisplayMode::wait;
        // }
        break;

    case DisplayMode::key_released_check2:
        text_state_.timer_ = seconds(1);
        display_mode_ = DisplayMode::done;
        break;

    case DisplayMode::done:
        if (expects_answer_y_n_) {
            const auto st = calc_screen_tiles(pfrm);

            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - (10 + y_start), 83);

            for (int i = 2; i < 8; ++i) {
                pfrm.set_tile(
                    Layer::overlay, st.x - i, st.y - (10 + y_start), 84);
                pfrm.set_tile(
                    Layer::overlay, st.x - i, st.y - (9 + y_start), 82);
                pfrm.set_tile(
                    Layer::overlay, st.x - i, st.y - (8 + y_start), 82);
                pfrm.set_tile(
                    Layer::overlay, st.x - i, st.y - (7 + y_start), 82);
                pfrm.set_tile(
                    Layer::overlay, st.x - i, st.y - (6 + y_start), 85);
            }

            pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (6 + y_start), 86);
            pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (9 + y_start), 88);
            pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (8 + y_start), 88);
            pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (7 + y_start), 88);
            pfrm.set_tile(Layer::overlay, st.x - 1, st.y - (10 + y_start), 87);

            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - (9 + y_start), 89);
            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - (8 + y_start), 89);
            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - (7 + y_start), 89);
            pfrm.set_tile(Layer::overlay, st.x - 8, st.y - (6 + y_start), 90);

            coins_.emplace(pfrm,
                           OverlayCoord{1, 2},
                           146,
                           (int)app.coins(),
                           UIMetric::Align::left);

            yes_text_.emplace(
                pfrm,
                "yes",
                OverlayCoord{u8(st.x - 5), u8(st.y - (9 + y_start))});

            no_text_.emplace(
                pfrm,
                "no",
                OverlayCoord{u8(st.x - 4), u8(st.y - (7 + y_start))});

            display_mode_ = DisplayMode::boolean_choice;
            break;
        }
        animate_moretext_icon();
        if (key_down<Key::action_1>(pfrm) or key_down<Key::action_2>(pfrm)) {
            invoke_hook(pfrm, app, "on-dialog-closed");
            display_mode_ = DisplayMode::animate_out;
        }
        break;

    case DisplayMode::boolean_choice:
        static const auto duration = milliseconds(400);
        text_state_.timer_ += delta;
        if (text_state_.timer_ > duration) {
            text_state_.timer_ = 0;
            const auto st = calc_screen_tiles(pfrm);
            if (pfrm.get_tile(Layer::overlay,
                              st.x - 7,
                              st.y - (7 + y_start + 2 * choice_sel_)) == 109) {
                pfrm.set_tile(Layer::overlay,
                              st.x - 7,
                              st.y - (7 + y_start + 2 * choice_sel_),
                              110);
            } else {
                pfrm.set_tile(Layer::overlay,
                              st.x - 7,
                              st.y - (7 + y_start + 2 * choice_sel_),
                              109);
            }
        }

        if (key_down<Key::up>(pfrm) or key_down<Key::down>(pfrm)) {
            const auto st = calc_screen_tiles(pfrm);
            pfrm.set_tile(Layer::overlay,
                          st.x - 7,
                          st.y - (7 + y_start + 2 * (choice_sel_)),
                          82);
            pfrm.set_tile(Layer::overlay,
                          st.x - 7,
                          st.y - (7 + y_start + 2 * (not choice_sel_)),
                          110);
            choice_sel_ = not choice_sel_;
            pfrm.speaker().play_sound("click", 1);
        }

        if (key_down<Key::action_1>(pfrm)) {
            if (choice_sel_) {
                invoke_hook(pfrm, app, "on-dialog-accepted");
            } else {
                invoke_hook(pfrm, app, "on-dialog-declined");
            }

            display_mode_ = DisplayMode::animate_out;
        }
        break;

    case DisplayMode::animate_out:
        display_mode_ = DisplayMode::clear;
        pfrm.fill_overlay(0);
        break;

    case DisplayMode::clear:
        if (goto_tutorial_) {
            auto next = scene_pool::alloc<SelectTutorialScene>();
            next->quick_select(goto_tutorial_ - 1);
            return next;
        }
        return data_->next_scene_();
    }

    return null_scene();
}



} // namespace skyland
