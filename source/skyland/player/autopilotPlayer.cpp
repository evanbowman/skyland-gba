////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "autopilotPlayer.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



AutopilotPlayer::AutopilotPlayer(lisp::Value* buttons_list)
    : buttons_list_(buttons_list)
{
    for (auto& s : prev_) {
        s = false;
    }

    for (auto& s : states_) {
        s = false;
    }
}



void AutopilotPlayer::on_room_damaged(Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : APP.birds()) {
        if (bird->island() == island) {
            bird->signal();
        }
    }
}



static std::pair<Button, bool> button_name_to_button(const char* name)
{
    if (str_cmp(name, "Right") == 0) {
        return {Button::right, true};
    } else if (str_cmp(name, "Left") == 0) {
        return {Button::left, true};
    } else if (str_cmp(name, "Up") == 0) {
        return {Button::up, true};
    } else if (str_cmp(name, "Down") == 0) {
        return {Button::down, true};
    } else if (str_cmp(name, "A") == 0) {
        return {Button::action_1, true};
    } else if (str_cmp(name, "B") == 0) {
        return {Button::action_2, true};
    } else if (str_eq(name, "Start-p")) {
        return {Button::start, true};
    } else if (str_eq(name, "Start-np")) {
        return {Button::start, false};
    } else if (str_cmp(name, "Select") == 0) {
        return {Button::select, true};
    } else if (str_cmp(name, "L-p") == 0) {
        return {Button::alt_1, true};
    } else if (str_cmp(name, "L-np") == 0) {
        return {Button::alt_1, false};
    } else if (str_cmp(name, "R") == 0) {
        return {Button::alt_2, true};
    } else {
        Platform::fatal(name);
    }
}



static bool is_button_level_triggered(Button k)
{
    return k == Button::start or k == Button::alt_1;
}



void AutopilotPlayer::update(Time delta)
{
    next_button_timeout_ -= delta;

    draw_image(290, 20, 8, 8, 6, Layer::overlay);

    button_tap_timeout_ -= delta;
    if (button_tap_timeout_ > 0) {
        if (taps_[(int)(Button::alt_1)]) {
            PLATFORM.set_tile(Layer::overlay, 20, 8, 338);
            PLATFORM.set_tile(Layer::overlay, 21, 8, 339);
        } else if (taps_[(int)Button::alt_2]) {
            PLATFORM.set_tile(Layer::overlay, 26, 8, 340);
            PLATFORM.set_tile(Layer::overlay, 27, 8, 341);
        } else if (taps_[(int)Button::right]) {
            PLATFORM.set_tile(Layer::overlay, 21, 10, 342);
        } else if (taps_[(int)Button::left]) {
            PLATFORM.set_tile(Layer::overlay, 20, 10, 343);
        } else if (taps_[(int)Button::up]) {
            PLATFORM.set_tile(Layer::overlay, 20, 10, 344);
            PLATFORM.set_tile(Layer::overlay, 21, 10, 345);
        } else if (taps_[(int)Button::down]) {
            PLATFORM.set_tile(Layer::overlay, 20, 10, 346);
            PLATFORM.set_tile(Layer::overlay, 21, 10, 347);
            PLATFORM.set_tile(Layer::overlay, 20, 11, 348);
            PLATFORM.set_tile(Layer::overlay, 21, 11, 349);
        } else if (taps_[(int)Button::action_1]) {
            PLATFORM.set_tile(Layer::overlay, 27, 10, 350);
        } else if (taps_[(int)Button::action_2]) {
            PLATFORM.set_tile(Layer::overlay, 26, 10, 351);
            PLATFORM.set_tile(Layer::overlay, 26, 11, 352);
        } else if (taps_[(int)Button::select]) {
            PLATFORM.set_tile(Layer::overlay, 21, 12, 353);
        } else if (taps_[(int)Button::start]) {
            PLATFORM.set_tile(Layer::overlay, 21, 11, 412);
        }
    }

    for (int i = 0; i < (int)Button::count; ++i) {
        if (not is_button_level_triggered((Button)i)) {
            prev_[i] = false;
        }
    }

    for (int i = 0; i < static_cast<int>(Button::count); ++i) {
        prev_[i] = states_[i];
    }

    for (int i = 0; i < (int)Button::count; ++i) {
        // These events are level-triggered, so we only want to keep this state
        // set for one update of the game loop, or else the button pressed events
        // will fire repeatedly. The start button is handled separately.
        // NOTE: alt_1 also handled separately.
        if (not is_button_level_triggered((Button)i)) {
            states_[i] = false;
        }
    }

    if (next_button_timeout_ <= 0) {

        if (next_timeout_button_) {
            const auto next = *next_timeout_button_;
            if (is_button_level_triggered(next)) {
                for (auto& t : taps_) {
                    t = false;
                }

                states_[(int)next] = not next_timeout_release_;
                taps_[(int)next] = not next_timeout_release_;
                if (next_timeout_release_) {
                    button_tap_timeout_ = milliseconds(300);
                } else {
                    button_tap_timeout_ = seconds(5);
                }
            } else {
                states_[(int)*next_timeout_button_] = true;
                for (int i = 0; i < (int)Button::count; ++i) {
                    if (not is_button_level_triggered((Button)i)) {
                        taps_[i] = false;
                    }
                }
                button_tap_timeout_ = milliseconds(300);
                taps_[(int)*next_timeout_button_] = true;
            }
            next_timeout_button_.reset();
        }

        auto first = ((lisp::Value*)buttons_list_);
        if (first->type() == lisp::Value::Type::cons) {
            auto current = first->cons().car();
            if (current->type() == lisp::Value::Type::cons) {
                auto tm = current->cons().car();
                if (tm->type() == lisp::Value::Type::integer) {
                    next_button_timeout_ = milliseconds(lisp::to_integer(tm));
                } else {
                    PLATFORM.fatal("invalid autopilot list format");
                }

                current = current->cons().cdr();
                if (current->type() == lisp::Value::Type::cons) {
                    auto button = current->cons().car();
                    if (button->type() == lisp::Value::Type::symbol) {
                        const char* name = button->symbol().name();
                        auto [button, state] = button_name_to_button(name);
                        next_timeout_button_ = button;
                        next_timeout_release_ = not state;
                    } else if (button->type() == lisp::Value::Type::string) {
                        PLATFORM.fill_overlay(0);
                        APP.dialog_buffer().emplace(
                            allocate<DialogString>("dialog-buffer"));
                        **APP.dialog_buffer() += button->string().value();
                    }
                } else {
                    PLATFORM.fatal("invalid autopilot list format");
                }

            } else {
                PLATFORM.fatal("invalid autopilot list format");
            }

            buttons_list_.set(first->cons().cdr());
        } else {
            APP.exit_condition() = App::ExitCondition::misc;
        }
    }
}



bool AutopilotPlayer::button_down(Button k)
{
    return states_[int(k)] and not prev_[int(k)];
}



bool AutopilotPlayer::button_up(Button k)
{
    return not states_[int(k)] and prev_[int(k)];
}



bool AutopilotPlayer::button_pressed(Button k)
{
    return states_[int(k)];
}



} // namespace skyland
