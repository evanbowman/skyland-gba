////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "lispReplScene.hpp"
#include "skyland/scene/macro/selectorScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/script_defs.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const u32 max_history_entries = 10;



LispReplScene::LispReplScene()
    : command_(allocate_dynamic<Command>("lisp-repl-command-buffer")),
      cpl_(allocate_dynamic<Completions>("lisp-repl-completion-buffer"))
{
}



static int paren_balance(const char* ptr)
{
    int balance = 0;
    while (*ptr not_eq '\0') {
        if (*ptr == '(') {
            ++balance;
        } else if (*ptr == ')') {
            --balance;
        }
        ++ptr;
    }
    return balance;
}



Vec2<int> calc_screen_tiles()
{
    return {30, 20};
}



// Inspired by the dvorak keyboard layout, redesigned for use with a gameboy
// dpad. Optimized for the smallest horizontal _and_ vertical travel between key
// presses.
static const char* const keyboard[7][6] = {{"z", "y", "g", "f", "v", "q"},
                                           {"m", "b", "i", "d", "l", "j"},
                                           {"w", "a", "o", "e", "u", "k"},
                                           {"p", "h", "t", "n", "s", "r"},
                                           {"x", "c", "(", ")", "-", " "},
                                           {"$", "'", "0", "1", "2", "3"},
                                           {"4", "5", "6", "7", "8", "9"}};

static const char* const alt_keyboard[7][6] = {{"Z", "Y", "G", "F", "V", "Q"},
                                               {"M", "B", "I", "D", "L", "J"},
                                               {"W", "A", "O", "E", "U", "K"},
                                               {"P", "H", "T", "N", "S", "R"},
                                               {"X", "C", "*", "/", "_", "+"},
                                               {"$", "\"", "<", ">", ".", ","},
                                               {"@", "*", ";", ":", "%", "#"}};


void LispReplScene::repaint_entry(bool show_cursor)
{
    const auto screen_tiles = calc_screen_tiles();

    auto darker_clr = Text::OptColors{
        {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

    if (gui_mode_) {
        darker_clr->foreground_ = custom_color(0x9595b9);
        darker_clr->background_ = custom_color(0x00002e);
    }

    entry_->assign(":", darker_clr);

    if (gui_mode_) {
        for (int i = 1; i < 32; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 97);
        }
    } else {
        for (int i = 1; i < 32; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
        }
    }


    auto colors = [this, darker_clr]() -> Text::OptColors {
        switch (display_mode_) {
        default:
        case DisplayMode::entry:
            if (gui_mode_) {
                return darker_clr;
            }
            return std::nullopt;

        case DisplayMode::show_result:
            return darker_clr;
        }
    }();

    int scroll;
    if (display_mode_ == DisplayMode::show_result) {
        scroll = scroll_counter_;
    } else {
        scroll = std::max(0, (int)command_->length() - (screen_tiles.x - 1));
    }

    const int balance = paren_balance(command_->c_str());
    if (balance < 0 and command_->length() and
        (*command_)[command_->length() - 1] == ')') {
        // Give a hint to the user, that he/she entered too many closing parens.
        command_->pop_back();
        entry_->append(command_->c_str() + scroll, colors);
        command_->push_back(')');

        entry_->append(")",
                       Text::OptColors{{ColorConstant::aerospace_orange,
                                        ColorConstant::rich_black}});
    } else {
        StringBuffer<30> truncate = command_->c_str() + scroll;
        entry_->append(truncate.c_str(), colors);
    }

    keyboard_.clear();

    u8 k_top = 2;
    u8 k_bot = 10;
    u8 k_mid = 3;

    if (gui_mode_) {
        k_top = 4;
        k_bot = 12;
        k_mid = 5;
    }

    keyboard_top_.emplace(OverlayCoord{2, k_top});
    keyboard_bottom_.emplace(OverlayCoord{2, k_bot});

    auto& kb = alt_ ? alt_keyboard : keyboard;

    if (not gui_mode_) {
        for (int x = 0; x < 6; ++x) {
            keyboard_top_->append(kb[6][x], darker_clr);
            keyboard_bottom_->append(kb[0][x], darker_clr);
        }
    }

    for (int i = 0; i < 7; ++i) {
        keyboard_.emplace_back(OverlayCoord{1, u8(k_mid + i)});
        if (not gui_mode_) {
            keyboard_.back().append(kb[i][5], darker_clr);
        } else {
            keyboard_.back().append(" ", darker_clr);
        }

        for (int j = 0; j < 6; ++j) {
            if (show_cursor and j == keyboard_cursor_.x and
                keyboard_cursor_.y == i) {
                const auto colors =
                    Text::OptColors{{ColorConstant::rich_black,
                                     ColorConstant::aerospace_orange}};
                keyboard_.back().append(kb[i][j], colors);
            } else {
                if (gui_mode_) {
                    keyboard_.back().append(kb[i][j], darker_clr);
                } else {
                    keyboard_.back().append(kb[i][j]);
                }
            }
        }
        if (not gui_mode_) {
            keyboard_.back().append(kb[i][0], darker_clr);
        }
    }
}


void LispReplScene::enter(Scene& prev)
{
    if (not gui_mode_) {
        *command_ = "try: (help)";
    }

    enable_text_icon_glyphs(false);

    if (not gui_mode_) {
        PLATFORM.fill_overlay(0);
        Text::print("A: enter text", {11, 3});
        Text::print("B: backspace", {11, 4});
        Text::print("R: shift", {11, 6});
        Text::print("L: autocomplete", {11, 5});
        Text::print("START: submit", {11, 7});
        Text::print("SELECT: history", {11, 8});
    }

    keyboard_cursor_ = {2, 4}; // For convenience, place cursor at left paren

    const auto screen_tiles = calc_screen_tiles();

    u8 offset = 1;
    if (gui_mode_) {
        offset = 4;
    }
    entry_.emplace(OverlayCoord{0, u8(screen_tiles.y - offset)});

    const char* version_text = "Skyland LISP v05";
    const auto vrsn_coord =
        OverlayCoord{u8((screen_tiles.x - 2) - strlen(version_text)), 0};

    version_text_.emplace(vrsn_coord);

    if (not gui_mode_) {

        for (int i = 0; i < 31; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
        }

        version_text_->assign(version_text);
    }

    repaint_entry();
}


void LispReplScene::repaint(bool focused)
{
    repaint_entry(focused);
}


void LispReplScene::exit(Scene& next)
{
    entry_.reset();
    keyboard_.clear();
    version_text_.reset();
    keyboard_top_.reset();
    keyboard_bottom_.reset();

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().fade(0.f);

    enable_text_icon_glyphs(true);
}


namespace
{
class Printer : public lisp::Printer
{
public:
    Printer(LispReplScene::Command& cmd) : cmd_(cmd)
    {
    }

    void put_str(const char* str) override
    {
        cmd_ += str;
    }

private:
    LispReplScene::Command& cmd_;
};
} // namespace


void LispReplScene::repaint_completions()
{
    cpl_->completions_.clear();

    u32 throttle = 9999;
    if (gui_mode_) {
        throttle = 10;
    }

    for (u32 i = 0; i < cpl_->completion_strs_.size() and
                    i < cpl_->completions_.capacity() and i < throttle;
         ++i) {
        Text::OptColors opts;
        if (i == cpl_->completion_cursor_) {
            opts = Text::OptColors{
                {ColorConstant::rich_black, ColorConstant::aerospace_orange}};
        }

        u8 offset = 2;
        if (gui_mode_) {
            offset = 4;
        }

        cpl_->completions_.emplace_back(OverlayCoord{10, u8(offset + i)});

        const auto str = cpl_->completion_strs_[i];
        int j;

        char tempstr[2] = {'\0', '\0'};

        for (j = 0; j < cpl_->completion_prefix_len_; ++j) {
            tempstr[0] = str[j];

            auto shade_opts = Text::OptColors{
                {custom_color(0x766df7), ColorConstant::rich_black}};

            if (gui_mode_) {
                shade_opts->background_ = custom_color(0xbfccde);
            }


            cpl_->completions_.back().append(
                tempstr, i == cpl_->completion_cursor_ ? opts : shade_opts);
        }

        const int len = strlen(str);
        if (len > 20) {
            for (; j < len and j < 19; ++j) {
                tempstr[0] = str[j];
                cpl_->completions_.back().append(tempstr, opts);
            }
            cpl_->completions_.back().append("…", opts);
        } else {
            for (; j < len; ++j) {
                tempstr[0] = str[j];
                cpl_->completions_.back().append(tempstr, opts);
            }
        }


        tempstr[0] = ' ';
        for (; j < 20; ++j) {
            cpl_->completions_.back().append(tempstr, opts);
        }
    }
}



bool LispReplScene::entry_empty() const
{
    return command_->empty();
}



ScenePtr LispReplScene::update(Time delta)
{
TOP:
    if (not gui_mode_) {
        constexpr auto fade_duration = milliseconds(500);
        if (timer_ < fade_duration) {
            if (timer_ + delta > fade_duration) {
                PLATFORM.screen().fade(0.34f);
            }
            timer_ += delta;

            const auto amount = 0.34f * smoothstep(0.f, fade_duration, timer_);

            if (timer_ < fade_duration) {
                PLATFORM.screen().fade(amount);
            }
        }
    }

    player().update(delta);

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (auto next = process_script_menu_request()) {
        return next;
    }

    switch (display_mode_) {
    case DisplayMode::completion_list:
        if (PLATFORM.keyboard().down_transition<Key::down>() and
            cpl_->completion_cursor_ < cpl_->completions_.size() - 1) {
            ++cpl_->completion_cursor_;
            repaint_completions();
            PLATFORM.speaker().play_sound("cursor_tick", 1);
        } else if (PLATFORM.keyboard().down_transition<Key::up>() and
                   cpl_->completion_cursor_ > 0) {
            --cpl_->completion_cursor_;
            repaint_completions();
            PLATFORM.speaker().play_sound("cursor_tick", 1);
        } else if (PLATFORM.keyboard().down_transition<Key::action_2>()) {
            repaint_entry();
            cpl_->completion_strs_.clear();
            cpl_->completions_.clear();
            if (gui_mode_) {
                clobbered_tiles_ = true;
            }
            display_mode_ = DisplayMode::entry;
        } else if (PLATFORM.keyboard().down_transition<Key::action_1>()) {
            *command_ += (cpl_->completion_strs_[cpl_->completion_cursor_] +
                          cpl_->completion_prefix_len_);
            repaint_entry();
            cpl_->completion_strs_.clear();
            cpl_->completions_.clear();
            if (gui_mode_) {
                clobbered_tiles_ = true;
            }
            PLATFORM.speaker().play_sound("typewriter", 2);
            display_mode_ = DisplayMode::entry;
        }
        break;

    case DisplayMode::entry:
        if (PLATFORM.keyboard().pressed<Key::alt_2>()) {
            auto prev_alt = alt_;
            alt_ = true;
            if (not prev_alt) {
                repaint_entry();
            }
        } else {
            auto prev_alt = alt_;
            alt_ = false;
            if (prev_alt) {
                repaint_entry();
            }
        }
        if (PLATFORM.keyboard().down_transition<Key::action_2>()) {
            if (command_->empty()) {
                if (APP.macrocosm()) {
                    return make_scene<macro::SelectorScene>();
                } else {
                    return make_scene<ReadyScene>();
                }
            }
            command_->pop_back();
            reset_history_index();
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::action_1>()) {
            auto& kb = alt_ ? alt_keyboard : keyboard;
            command_->push_back(kb[keyboard_cursor_.y][keyboard_cursor_.x][0]);
            repaint_entry();
            PLATFORM.speaker().play_sound("typewriter", 2);
            reset_history_index();
        } else if (PLATFORM.keyboard().down_transition<Key::alt_1>()) {
            // Try to isolate an identifier from the command buffer, for autocomplete.

            auto is_delimiter = [](char c) {
                return c == ' ' or c == ')' or c == '(' or c == '\'';
            };

            if (not command_->empty() and
                not is_delimiter((*command_)[command_->length() - 1])) {
                Optional<int> ident_start;

                for (int i = command_->length() - 1; i >= 0; --i) {
                    if (is_delimiter((*command_)[i])) {
                        ident_start = i + 1;
                        break;
                    } else if (i == 0 and not is_delimiter((*command_)[i])) {
                        ident_start = 0;
                    }
                }

                if (ident_start) {
                    StringBuffer<8> ident(command_->c_str() + *ident_start);
                    // We need to store this value for later, if a user selects
                    // a completion, we need to know how many characters of the
                    // substitution to skip, unless we want to parse the
                    // identifier out of the command buffer again.
                    cpl_->completion_prefix_len_ = ident.length();

                    lisp::apropos(ident.c_str(), cpl_->completion_strs_);

                    if (cpl_->completion_strs_.size() not_eq 0) {
                        if (not gui_mode_) {
                            for (int x = 11; x < 30; ++x) {
                                for (int y = 3; y < 27; ++y) {
                                    PLATFORM.set_tile(Layer::overlay, x, y, 0);
                                }
                            }
                        }
                        display_mode_ = DisplayMode::completion_list;
                        cpl_->completion_cursor_ = 0;
                        repaint_completions();
                        repaint_entry(false);
                    }
                }
            } else {
                error("command empty or recent delimiter");
            }
        }

        if (PLATFORM.keyboard().down_transition<Key::start>()) {

            PLATFORM.speaker().play_sound("tw_bell", 2);

            lisp::BasicCharSequence seq(command_->c_str());
            lisp::read(seq);
            lisp::eval(lisp::get_op(0));

            if (not str_eq(*command_, history_.back())) {
                history_.push_back(*command_);
            }

            if (history_.size() > max_history_entries) {
                history_.erase(history_.begin());
            }

            command_->clear();
            Printer p(*command_);
            format(lisp::get_op(0), p);
            info(command_->c_str());

            lisp::pop_op();
            lisp::pop_op();

            display_mode_ = DisplayMode::show_result;
            scroll_counter_ = 0;

            repaint_entry();
            reset_history_index();
        }

        if (PLATFORM.keyboard().down_transition<Key::select>()) {
            auto history_count = history_.size();
            if (history_count) {
                if (history_index_ == 0 and history_insert_pos_ < 0) {
                    history_insert_pos_ = command_->length();
                }
                auto prev = history_[(history_.size() - 1) - history_index_++];
                if (history_index_ > history_.size() - 1) {
                    history_index_ = 0;
                }
                while ((int)command_->length() > history_insert_pos_) {
                    command_->pop_back();
                }
                *command_ += prev;
                repaint_entry();
            }
        } else if (PLATFORM.keyboard().down_transition<Key::left>()) {
            if (keyboard_cursor_.x == 0) {
                keyboard_cursor_.x = 5;
            } else {
                keyboard_cursor_.x -= 1;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 1);
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::right>()) {
            if (keyboard_cursor_.x == 5) {
                keyboard_cursor_.x = 0;
            } else {
                keyboard_cursor_.x += 1;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 1);
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::up>()) {
            if (keyboard_cursor_.y == 0) {
                keyboard_cursor_.y = 6;
            } else {
                keyboard_cursor_.y -= 1;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 1);
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::down>()) {
            if (keyboard_cursor_.y == 6) {
                keyboard_cursor_.y = 0;
            } else {
                keyboard_cursor_.y += 1;
            }
            PLATFORM.speaker().play_sound("cursor_tick", 1);
            repaint_entry();
        }
        break;

    case DisplayMode::show_result:

        if ((int)command_->length() > calc_screen_tiles().x) {
            if (test_key(Key::right)) {
                ++scroll_counter_;
                repaint_entry();
            } else if (test_key(Key::left)) {
                if (scroll_counter_ > 0) {
                    --scroll_counter_;
                    repaint_entry();
                }
            } else if (test_key(Key::up) or test_key(Key::down) or
                       test_key(Key::start) or test_key(Key::action_1) or
                       test_key(Key::action_2)) {
                display_mode_ = DisplayMode::entry;
                command_->clear();
                repaint_entry();
                goto TOP;
                return null_scene();
            }
            return null_scene();
        }

        if (PLATFORM.keyboard()
                .down_transition<Key::action_2,
                                 Key::action_1,
                                 Key::start,
                                 Key::left,
                                 Key::right,
                                 Key::up,
                                 Key::down>()) {

            display_mode_ = DisplayMode::entry;
            command_->clear();
            repaint_entry();
            goto TOP;
            return null_scene();
        }
        break;
    }
    return null_scene();
}



void LispReplScene::inject_command(const char* str)
{
    command_->clear();
    display_mode_ = DisplayMode::entry;
    while (*str not_eq '\0') {
        command_->push_back(*(str++));
        reset_history_index();
    }
    repaint_entry();
    PLATFORM.speaker().play_sound("typewriter", 2);
}



} // namespace skyland
