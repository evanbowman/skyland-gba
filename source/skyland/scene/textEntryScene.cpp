////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "textEntryScene.hpp"
#include "platform/platform.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/systemString.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



TextEntryScene::TextEntryScene(const char* prompt,
                               Receiver receiver,
                               int required_chars,
                               int char_limit,
                               const char* default_text)
    : state_(allocate_dynamic<State>("text-entry-buffer")), receiver_(receiver),
      required_chars_(required_chars), char_limit_(char_limit)
{
    state_->prompt_ = prompt;
    state_->buffer_ = default_text;
}



void TextEntryScene::enter(Scene& prev)
{
    render_keyboard();

    auto submit_str = SYSTR(submit_hint);
    auto submit_cstr = submit_str->c_str();
    auto slen = utf8::len(submit_cstr);

    submit_text_.emplace(OverlayCoord{u8(centered_text_margins(slen)), 18});

    submit_text_->assign(
        submit_cstr,
        FontColors{custom_color(0xcdc3eb), custom_color(0x007cbf)});

    entry_.emplace(OverlayCoord{u8(centered_text_margins(char_limit_)), 5});

    auto base_colors =
        FontColors{custom_color(0xffffff), custom_color(0x007cbf)};

    PLATFORM.screen().schedule_fade(1.f, custom_color(0x007cbf));

    auto mapping_info_lbrace = locale_texture_map()('<');
    const u16 lbrace = PLATFORM.map_glyph('<', *mapping_info_lbrace);
    PLATFORM.set_tile(
        entry_->coord().x - 2, entry_->coord().y, lbrace, base_colors);

    auto mapping_info_rbrace = locale_texture_map()('>');
    const u16 rbrace = PLATFORM.map_glyph('>', *mapping_info_rbrace);
    PLATFORM.set_tile(entry_->coord().x + char_limit_ + 1,
                      entry_->coord().y,
                      rbrace,
                      base_colors);



    prompt_text_.emplace(

        OverlayCoord{
            u8(centered_text_margins(utf8::len(state_->prompt_.c_str()))), 2});

    static const auto status_colors =
        FontColors{custom_color(0x007cbf), ColorConstant::silver_white};

    prompt_text_->assign(state_->prompt_.c_str(), status_colors);

    if (not state_->buffer_.empty()) {
        entry_->assign(
            state_->buffer_.c_str(),
            FontColors{custom_color(0xffffff), custom_color(0xbd7f14)});
    }
}


static const char* keyboard[4] = {"# 1 2 3 4 5 6 7 8 9 0 = ",
                                  "q w e r t y u i o p [ ] ",
                                  "a s d f g h j k l ; ' - ",
                                  "z x c v b n m , . _ / ? "};


ScenePtr TextEntryScene::update(Time delta)
{
    player().update(delta);

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    auto row_strlen = strlen(keyboard[keyboard_cursor_.y]);
    if (row_strlen % 2) {
        Platform::fatal("logic error: invalid spacing in keyboard");
    }

    const auto row_width = row_strlen / 2;

    if (test_key(Key::right)) {
        if (keyboard_cursor_.x < row_width - 1) {
            ++keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = 0;
        }
        render_keyboard();
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }

    if (test_key(Key::left)) {
        if (keyboard_cursor_.x > 0) {
            --keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = row_width - 1;
        }
        render_keyboard();
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }

    if (test_key(Key::up)) {
        if (keyboard_cursor_.y > 0) {
            --keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 3;
        }
        render_keyboard();
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }

    if (test_key(Key::down)) {
        if (keyboard_cursor_.y < 3) {
            ++keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 0;
        }
        render_keyboard();
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }

    if (player().key_down(Key::start)) {
        if (state_->buffer_.length() >= (u32)required_chars_) {
            return receiver_(state_->buffer_.c_str());
        } else {
            PLATFORM.speaker().play_sound("beep_error", 2);
        }
    }


    if (player().key_down(Key::action_2)) {
        if (state_->buffer_.empty()) {
            return null_scene();
        }
        state_->buffer_.pop_back();

        entry_->assign(
            state_->buffer_.c_str(),
            FontColors{custom_color(0xffffff), custom_color(0xbd7f14)});

    } else if (player().key_down(Key::action_1)) {
        if (state_->buffer_.length() == (u32)char_limit_) {
            return receiver_(state_->buffer_.c_str());
        }

        state_->buffer_.push_back(
            keyboard[keyboard_cursor_.y][keyboard_cursor_.x * 2]);

        entry_->assign(
            state_->buffer_.c_str(),
            FontColors{custom_color(0xffffff), custom_color(0xbd7f14)});
    }

    return null_scene();
}



static const auto status_colors =
    FontColors{ColorConstant::silver_white, custom_color(0x007cbf)};


void TextEntryScene::render_keyboard()
{
    for (int r = 0; r < 4; ++r) {
        auto l = strlen(keyboard[r]);
        for (u32 i = 0; i < l; ++i) {

            const char c = keyboard[r][i];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = PLATFORM.map_glyph(c, *mapping_info);

            auto colors = status_colors;
            if ((int)i / 2 == keyboard_cursor_.x and r == keyboard_cursor_.y) {
                colors =
                    FontColors{custom_color(0xffffff), custom_color(0xbd7f14)};
            }

            PLATFORM.set_tile((3) + i, (19 - 10) + r * 2, t, colors);
        }
    }
}



void TextEntryScene::exit(Scene& next)
{
    entry_.reset();
    submit_text_.reset();
    prompt_text_.reset();
    PLATFORM.fill_overlay(0);
}



} // namespace skyland
