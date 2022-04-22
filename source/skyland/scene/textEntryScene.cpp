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


#include "textEntryScene.hpp"
#include "platform/platform.hpp"
#include "skyland/player/playerP1.hpp"



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



void TextEntryScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    render_keyboard(pfrm);

    entry_.emplace(
        pfrm, OverlayCoord{u8(centered_text_margins(pfrm, char_limit_)), 5});

    auto base_colors =
        FontColors{custom_color(0xffffff), custom_color(0x392194)};

    pfrm.screen().schedule_fade(1.f, custom_color(0x392194));

    auto mapping_info_lbrace = locale_texture_map()('<');
    const u16 lbrace = pfrm.map_glyph('<', *mapping_info_lbrace);
    pfrm.set_tile(
        entry_->coord().x - 2, entry_->coord().y, lbrace, base_colors);

    auto mapping_info_rbrace = locale_texture_map()('>');
    const u16 rbrace = pfrm.map_glyph('>', *mapping_info_rbrace);
    pfrm.set_tile(entry_->coord().x + char_limit_ + 1,
                  entry_->coord().y,
                  rbrace,
                  base_colors);



    prompt_text_.emplace(
        pfrm,
        OverlayCoord{
            u8(centered_text_margins(pfrm, utf8::len(state_->prompt_.c_str()))),
            2});

    static const auto status_colors =
        FontColors{custom_color(0x392194), ColorConstant::silver_white};

    prompt_text_->assign(state_->prompt_.c_str(), status_colors);

    if (not state_->buffer_.empty()) {
        entry_->assign(
            state_->buffer_.c_str(),
            FontColors{custom_color(0xffffff), custom_color(0xbd7f14)});
    }
}


const char* keyboard[4] = {"# 1 2 3 4 5 6 7 8 9 0 = ",
                           "q w e r t y u i o p [ ] ",
                           "a s d f g h j k l ; ' - ",
                           "z x c v b n m , . _ / ? "};


ScenePtr<Scene>
TextEntryScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    player(app).update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    auto row_strlen = str_len(keyboard[keyboard_cursor_.y]);
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
        render_keyboard(pfrm);
    }

    if (test_key(Key::left)) {
        if (keyboard_cursor_.x > 0) {
            --keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = row_width - 1;
        }
        render_keyboard(pfrm);
    }

    if (test_key(Key::up)) {
        if (keyboard_cursor_.y > 0) {
            --keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 3;
        }
        render_keyboard(pfrm);
    }

    if (test_key(Key::down)) {
        if (keyboard_cursor_.y < 3) {
            ++keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 0;
        }
        render_keyboard(pfrm);
    }

    if (player(app).key_down(pfrm, Key::start)) {
        if (state_->buffer_.length() >= (u32)required_chars_) {
            return receiver_(state_->buffer_.c_str());
        } else {
            pfrm.speaker().play_sound("beep_error", 2);
        }
    }


    if (player(app).key_down(pfrm, Key::action_2)) {
        if (state_->buffer_.empty()) {
            return null_scene();
        }
        state_->buffer_.pop_back();

        entry_->assign(
            state_->buffer_.c_str(),
            FontColors{custom_color(0xffffff), custom_color(0xbd7f14)});

    } else if (player(app).key_down(pfrm, Key::action_1)) {
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
    FontColors{ColorConstant::silver_white, custom_color(0x392194)};


void TextEntryScene::render_keyboard(Platform& pfrm)
{
    for (int r = 0; r < 4; ++r) {
        auto l = str_len(keyboard[r]);
        for (u32 i = 0; i < l; ++i) {

            const char c = keyboard[r][i];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = pfrm.map_glyph(c, *mapping_info);

            auto colors = status_colors;
            if ((int)i / 2 == keyboard_cursor_.x and r == keyboard_cursor_.y) {
                colors =
                    FontColors{custom_color(0xffffff), custom_color(0xbd7f14)};
            }

            pfrm.set_tile((3) + i, (19 - 10) + r * 2, t, colors);
        }
    }
}



void TextEntryScene::exit(Platform& pfrm, App& app, Scene& next)
{
    entry_.reset();
    prompt_text_.reset();
    pfrm.fill_overlay(0);
}



} // namespace skyland
