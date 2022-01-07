#include "lispReplScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {



LispReplScene::LispReplScene(Platform& pfrm)
    : command_(allocate_dynamic<Command>(pfrm)),
      cpl_(allocate_dynamic<Completions>(pfrm))
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
static const char* keyboard[7][6] = {{"z", "y", "g", "f", "v", "q"},
                                     {"m", "b", "i", "d", "l", "j"},
                                     {"w", "a", "o", "e", "u", "k"},
                                     {"p", "h", "t", "n", "s", "r"},
                                     {"x", "c", "(", ")", "-", " "},
                                     {"$", "'", "0", "1", "2", "3"},
                                     {"4", "5", "6", "7", "8", "9"}};


void LispReplScene::repaint_entry(Platform& pfrm, bool show_cursor)
{
    const auto screen_tiles = calc_screen_tiles(pfrm);

    const auto darker_clr = Text::OptColors{
        {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

    entry_->assign(":", darker_clr);

    for (int i = 1; i < 32; ++i) {
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
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

    keyboard_top_.emplace(pfrm, OverlayCoord{2, 2});
    keyboard_bottom_.emplace(pfrm, OverlayCoord{2, 10});

    for (int x = 0; x < 6; ++x) {
        keyboard_top_->append(keyboard[6][x], darker_clr);
        keyboard_bottom_->append(keyboard[0][x], darker_clr);
    }

    for (int i = 0; i < 7; ++i) {
        keyboard_.emplace_back(pfrm, OverlayCoord{1, u8(3 + i)});
        keyboard_.back().append(keyboard[i][5], darker_clr);

        for (int j = 0; j < 6; ++j) {
            if (show_cursor and j == keyboard_cursor_.x and
                keyboard_cursor_.y == i) {
                const auto colors =
                    Text::OptColors{{ColorConstant::rich_black,
                                     ColorConstant::aerospace_orange}};
                keyboard_.back().append(keyboard[i][j], colors);
            } else {
                keyboard_.back().append(keyboard[i][j]);
            }
        }
        keyboard_.back().append(keyboard[i][0], darker_clr);
    }
}


void LispReplScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);

    keyboard_cursor_ = {2, 4}; // For convenience, place cursor at left paren

    const auto screen_tiles = calc_screen_tiles(pfrm);

    entry_.emplace(pfrm, OverlayCoord{0, u8(screen_tiles.y - 1)});

    const char* version_text = "Skyland LISP v03";

    for (int i = 0; i < 31; ++i) {
        pfrm.set_tile(Layer::overlay, i, 0, 112);
    }

    const auto vrsn_coord =
        OverlayCoord{u8((screen_tiles.x - 2) - str_len(version_text)), 0};

    version_text_.emplace(pfrm, vrsn_coord);

    version_text_->assign(version_text);

    repaint_entry(pfrm);
}


void LispReplScene::exit(Platform& pfrm, App& app, Scene& next)
{
    entry_.reset();
    keyboard_.clear();
    version_text_.reset();
    keyboard_top_.reset();
    keyboard_bottom_.reset();

    pfrm.fill_overlay(0);
    pfrm.screen().fade(0.f);
}


namespace {
class Printer : public lisp::Printer {
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


void LispReplScene::repaint_completions(Platform& pfrm)
{
    cpl_->completions_.clear();

    for (u32 i = 0; i < cpl_->completion_strs_.size(); ++i) {
        Text::OptColors opts;
        if (i == cpl_->completion_cursor_) {
            opts = Text::OptColors{
                {ColorConstant::rich_black, ColorConstant::aerospace_orange}};
        }

        cpl_->completions_.emplace_back(pfrm, OverlayCoord{10, u8(2 + i)});

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

        const int len = str_len(str);
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


ScenePtr<Scene>
LispReplScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    constexpr auto fade_duration = milliseconds(700);
    if (timer_ < fade_duration) {
        if (timer_ + delta > fade_duration) {
            pfrm.screen().fade(0.34f);
        }
        timer_ += delta;

        const auto amount = 0.34f * smoothstep(0.f, fade_duration, timer_);

        if (timer_ < fade_duration) {
            pfrm.screen().fade(amount);
        }
    }

    switch (display_mode_) {
    case DisplayMode::completion_list:
        if (pfrm.keyboard().down_transition<Key::down>() and
            cpl_->completion_cursor_ < cpl_->completion_strs_.size() - 1) {
            ++cpl_->completion_cursor_;
            repaint_completions(pfrm);
            pfrm.speaker().play_sound("scroll", 1);
        } else if (pfrm.keyboard().down_transition<Key::up>() and
                   cpl_->completion_cursor_ > 0) {
            --cpl_->completion_cursor_;
            repaint_completions(pfrm);
            pfrm.speaker().play_sound("scroll", 1);
        } else if (pfrm.keyboard().down_transition<Key::action_2>()) {
            repaint_entry(pfrm);
            cpl_->completion_strs_.clear();
            cpl_->completions_.clear();
            display_mode_ = DisplayMode::entry;
        } else if (pfrm.keyboard().down_transition<Key::action_1>()) {
            *command_ += (cpl_->completion_strs_[cpl_->completion_cursor_] +
                          cpl_->completion_prefix_len_);
            repaint_entry(pfrm);
            cpl_->completion_strs_.clear();
            cpl_->completions_.clear();
            pfrm.speaker().play_sound("typewriter", 2);
            display_mode_ = DisplayMode::entry;
        }
        break;

    case DisplayMode::entry:
        if (pfrm.keyboard().down_transition<Key::action_2>()) {
            if (command_->empty()) {
                return scene_pool::alloc<ReadyScene>();
            }
            command_->pop_back();
            repaint_entry(pfrm);
        } else if (pfrm.keyboard().down_transition<Key::action_1>()) {
            command_->push_back(
                keyboard[keyboard_cursor_.y][keyboard_cursor_.x][0]);
            repaint_entry(pfrm);
            pfrm.speaker().play_sound("typewriter", 2);
        } else if (pfrm.keyboard().down_transition<Key::alt_1>()) {
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

                    lisp::get_interns([&ident, this](const char* intern) {
                        if (cpl_->completion_strs_.full()) {
                            return;
                        }

                        const auto intern_len = str_len(intern);
                        if (intern_len <= ident.length()) {
                            // I mean, there's no reason to autocomplete
                            // to something shorter or the same length...
                            return;
                        }

                        for (u32 i = 0; i < ident.length() and i < intern_len;
                             ++i) {
                            if (ident[i] not_eq intern[i]) {
                                return;
                            }
                        }

                        cpl_->completion_strs_.push_back(intern);
                    });

                    if (not cpl_->completion_strs_.empty()) {
                        display_mode_ = DisplayMode::completion_list;
                        cpl_->completion_cursor_ = 0;
                        repaint_completions(pfrm);
                        repaint_entry(pfrm, false);
                    }
                }
            } else {
                error(pfrm, "command empty or recent delimiter");
            }
        }

        if (pfrm.keyboard().down_transition<Key::start>()) {

            pfrm.speaker().play_sound("tw_bell", 2);

            lisp::BasicCharSequence seq(command_->c_str());
            lisp::read(seq);
            lisp::eval(lisp::get_op(0));

            command_->clear();
            Printer p(*command_);
            format(lisp::get_op(0), p);

            lisp::pop_op();
            lisp::pop_op();

            display_mode_ = DisplayMode::show_result;

            repaint_entry(pfrm);
        }

        if (pfrm.keyboard().down_transition<Key::left>()) {
            if (keyboard_cursor_.x == 0) {
                keyboard_cursor_.x = 5;
            } else {
                keyboard_cursor_.x -= 1;
            }
            pfrm.speaker().play_sound("scroll", 1);
            repaint_entry(pfrm);
        } else if (pfrm.keyboard().down_transition<Key::right>()) {
            if (keyboard_cursor_.x == 5) {
                keyboard_cursor_.x = 0;
            } else {
                keyboard_cursor_.x += 1;
            }
            pfrm.speaker().play_sound("scroll", 1);
            repaint_entry(pfrm);
        } else if (pfrm.keyboard().down_transition<Key::up>()) {
            if (keyboard_cursor_.y == 0) {
                keyboard_cursor_.y = 6;
            } else {
                keyboard_cursor_.y -= 1;
            }
            pfrm.speaker().play_sound("scroll", 1);
            repaint_entry(pfrm);
        } else if (pfrm.keyboard().down_transition<Key::down>()) {
            if (keyboard_cursor_.y == 6) {
                keyboard_cursor_.y = 0;
            } else {
                keyboard_cursor_.y += 1;
            }
            pfrm.speaker().play_sound("scroll", 1);
            repaint_entry(pfrm);
        }
        break;

    case DisplayMode::show_result:
        if (pfrm.keyboard()
                .down_transition<Key::action_2,
                                 Key::action_1,
                                 Key::start,
                                 Key::left,
                                 Key::right,
                                 Key::up,
                                 Key::down>()) {

            display_mode_ = DisplayMode::entry;
            command_->clear();
            repaint_entry(pfrm);
            return null_scene();
        }
        break;
    }
    return null_scene();
}



} // namespace skyland
