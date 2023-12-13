////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
    *command_ = "try: (help)";
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
static const char* keyboard[7][6] = {{"z", "y", "g", "f", "v", "q"},
                                     {"m", "b", "i", "d", "l", "j"},
                                     {"w", "a", "o", "e", "u", "k"},
                                     {"p", "h", "t", "n", "s", "r"},
                                     {"x", "c", "(", ")", "-", " "},
                                     {"$", "'", "0", "1", "2", "3"},
                                     {"4", "5", "6", "7", "8", "9"}};

static const char* alt_keyboard[7][6] = {{"Z", "Y", "G", "F", "V", "Q"},
                                         {"M", "B", "I", "D", "L", "J"},
                                         {"W", "A", "O", "E", "U", "K"},
                                         {"P", "H", "T", "N", "S", "R"},
                                         {"X", "C", "*", "/", "_", "+"},
                                         {"$", "\"", "<", ">", ".", ","},
                                         {"@", "*", ";", ":", "%", "#"}};


void LispReplScene::repaint_entry(bool show_cursor)
{
    const auto screen_tiles = calc_screen_tiles();

    const auto darker_clr = Text::OptColors{
        {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

    entry_->assign(":", darker_clr);

    for (int i = 1; i < 32; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
    }

    auto colors = [this]() -> Text::OptColors {
        switch (display_mode_) {
        default:
        case DisplayMode::entry:
            return std::nullopt;

        case DisplayMode::show_result:
            return {{ColorConstant::med_blue_gray, ColorConstant::rich_black}};
        }
    }();
    const int scroll =
        std::max(0, (int)command_->length() - (screen_tiles.x - 1));


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
        entry_->append(command_->c_str() + scroll, colors);
    }

    keyboard_.clear();

    keyboard_top_.emplace(OverlayCoord{2, 2});
    keyboard_bottom_.emplace(OverlayCoord{2, 10});

    auto& kb = alt_ ? alt_keyboard : keyboard;

    for (int x = 0; x < 6; ++x) {
        keyboard_top_->append(kb[6][x], darker_clr);
        keyboard_bottom_->append(kb[0][x], darker_clr);
    }

    for (int i = 0; i < 7; ++i) {
        keyboard_.emplace_back(OverlayCoord{1, u8(3 + i)});
        keyboard_.back().append(kb[i][5], darker_clr);

        for (int j = 0; j < 6; ++j) {
            if (show_cursor and j == keyboard_cursor_.x and
                keyboard_cursor_.y == i) {
                const auto colors =
                    Text::OptColors{{ColorConstant::rich_black,
                                     ColorConstant::aerospace_orange}};
                keyboard_.back().append(kb[i][j], colors);
            } else {
                keyboard_.back().append(kb[i][j]);
            }
        }
        keyboard_.back().append(kb[i][0], darker_clr);
    }
}


void LispReplScene::enter(Scene& prev)
{
    enable_text_icon_glyphs(false);

    PLATFORM.fill_overlay(0);

    Text::print("A: enter text", {11, 3});
    Text::print("B: backspace", {11, 4});
    Text::print("R: shift", {11, 6});
    Text::print("L: autocomplete", {11, 5});
    Text::print("START: submit", {11, 7});
    Text::print("SELECT: history", {11, 8});

    keyboard_cursor_ = {2, 4}; // For convenience, place cursor at left paren

    const auto screen_tiles = calc_screen_tiles();

    entry_.emplace(OverlayCoord{0, u8(screen_tiles.y - 1)});

    const char* version_text = "Skyland LISP v04";

    for (int i = 0; i < 31; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 0, 112);
    }

    const auto vrsn_coord =
        OverlayCoord{u8((screen_tiles.x - 2) - strlen(version_text)), 0};

    version_text_.emplace(vrsn_coord);

    version_text_->assign(version_text);

    repaint_entry();
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

    for (u32 i = 0; i < cpl_->completion_strs_.size() and
                    i < cpl_->completions_.capacity();
         ++i) {
        Text::OptColors opts;
        if (i == cpl_->completion_cursor_) {
            opts = Text::OptColors{
                {ColorConstant::rich_black, ColorConstant::aerospace_orange}};
        }

        cpl_->completions_.emplace_back(OverlayCoord{10, u8(2 + i)});

        const auto str = cpl_->completion_strs_[i];
        int j;

        char tempstr[2] = {'\0', '\0'};

        for (j = 0; j < cpl_->completion_prefix_len_; ++j) {
            tempstr[0] = str[j];
            cpl_->completions_.back().append(
                tempstr,
                i == cpl_->completion_cursor_
                    ? opts
                    : Text::OptColors{
                          {custom_color(0x766df7), ColorConstant::rich_black}});
        }

        const int len = strlen(str);
        for (; j < len; ++j) {
            tempstr[0] = str[j];
            cpl_->completions_.back().append(tempstr, opts);
        }

        tempstr[0] = ' ';
        for (; j < 20; ++j) {
            cpl_->completions_.back().append(tempstr, opts);
        }
    }
}


ScenePtr<Scene> LispReplScene::update(Time delta)
{
TOP:
    constexpr auto fade_duration = milliseconds(700);
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

    if (auto next = process_script_menu_request()) {
        return next;
    }

    switch (display_mode_) {
    case DisplayMode::completion_list:
        if (PLATFORM.keyboard().down_transition<Key::down>() and
            cpl_->completion_cursor_ < cpl_->completions_.size() - 1) {
            ++cpl_->completion_cursor_;
            repaint_completions();
            PLATFORM.speaker().play_sound("scroll", 1);
        } else if (PLATFORM.keyboard().down_transition<Key::up>() and
                   cpl_->completion_cursor_ > 0) {
            --cpl_->completion_cursor_;
            repaint_completions();
            PLATFORM.speaker().play_sound("scroll", 1);
        } else if (PLATFORM.keyboard().down_transition<Key::action_2>()) {
            repaint_entry();
            cpl_->completion_strs_.clear();
            cpl_->completions_.clear();
            display_mode_ = DisplayMode::entry;
        } else if (PLATFORM.keyboard().down_transition<Key::action_1>()) {
            *command_ += (cpl_->completion_strs_[cpl_->completion_cursor_] +
                          cpl_->completion_prefix_len_);
            repaint_entry();
            cpl_->completion_strs_.clear();
            cpl_->completions_.clear();
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
                    return scene_pool::alloc<macro::SelectorScene>();
                } else {
                    return scene_pool::alloc<ReadyScene>();
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
                std::optional<int> ident_start;

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
                        for (int x = 11; x < 30; ++x) {
                            for (int y = 3; y < 27; ++y) {
                                PLATFORM.set_tile(Layer::overlay, x, y, 0);
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

            lisp::pop_op();
            lisp::pop_op();

            display_mode_ = DisplayMode::show_result;

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
            PLATFORM.speaker().play_sound("scroll", 1);
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::right>()) {
            if (keyboard_cursor_.x == 5) {
                keyboard_cursor_.x = 0;
            } else {
                keyboard_cursor_.x += 1;
            }
            PLATFORM.speaker().play_sound("scroll", 1);
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::up>()) {
            if (keyboard_cursor_.y == 0) {
                keyboard_cursor_.y = 6;
            } else {
                keyboard_cursor_.y -= 1;
            }
            PLATFORM.speaker().play_sound("scroll", 1);
            repaint_entry();
        } else if (PLATFORM.keyboard().down_transition<Key::down>()) {
            if (keyboard_cursor_.y == 6) {
                keyboard_cursor_.y = 0;
            } else {
                keyboard_cursor_.y += 1;
            }
            PLATFORM.speaker().play_sound("scroll", 1);
            repaint_entry();
        }
        break;

    case DisplayMode::show_result:
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



} // namespace skyland
