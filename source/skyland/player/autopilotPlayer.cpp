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



AutopilotPlayer::AutopilotPlayer(lisp::Value* keys_list) : keys_list_(keys_list)
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



static std::pair<Key, bool> button_name_to_key(const char* name)
{
    if (str_cmp(name, "Right") == 0) {
        return {Key::right, true};
    } else if (str_cmp(name, "Left") == 0) {
        return {Key::left, true};
    } else if (str_cmp(name, "Up") == 0) {
        return {Key::up, true};
    } else if (str_cmp(name, "Down") == 0) {
        return {Key::down, true};
    } else if (str_cmp(name, "A") == 0) {
        return {Key::action_1, true};
    } else if (str_cmp(name, "B") == 0) {
        return {Key::action_2, true};
    } else if (str_eq(name, "Start-p")) {
        return {Key::start, true};
    } else if (str_eq(name, "Start-np")) {
        return {Key::start, false};
    } else if (str_cmp(name, "Select") == 0) {
        return {Key::select, true};
    } else if (str_cmp(name, "L-p") == 0) {
        return {Key::alt_1, true};
    } else if (str_cmp(name, "L-np") == 0) {
        return {Key::alt_1, false};
    } else if (str_cmp(name, "R") == 0) {
        return {Key::alt_2, true};
    } else {
        Platform::fatal(name);
    }
}



static bool is_key_level_triggered(Key k)
{
    return k == Key::start or k == Key::alt_1;
}



void AutopilotPlayer::update(Time delta)
{
    next_key_timeout_ -= delta;

    draw_image(290, 20, 8, 8, 6, Layer::overlay);

    key_tap_timeout_ -= delta;
    if (key_tap_timeout_ > 0) {
        if (taps_[(int)(Key::alt_1)]) {
            PLATFORM.set_tile(Layer::overlay, 20, 8, 338);
            PLATFORM.set_tile(Layer::overlay, 21, 8, 339);
        } else if (taps_[(int)Key::alt_2]) {
            PLATFORM.set_tile(Layer::overlay, 26, 8, 340);
            PLATFORM.set_tile(Layer::overlay, 27, 8, 341);
        } else if (taps_[(int)Key::right]) {
            PLATFORM.set_tile(Layer::overlay, 21, 10, 342);
        } else if (taps_[(int)Key::left]) {
            PLATFORM.set_tile(Layer::overlay, 20, 10, 343);
        } else if (taps_[(int)Key::up]) {
            PLATFORM.set_tile(Layer::overlay, 20, 10, 344);
            PLATFORM.set_tile(Layer::overlay, 21, 10, 345);
        } else if (taps_[(int)Key::down]) {
            PLATFORM.set_tile(Layer::overlay, 20, 10, 346);
            PLATFORM.set_tile(Layer::overlay, 21, 10, 347);
            PLATFORM.set_tile(Layer::overlay, 20, 11, 348);
            PLATFORM.set_tile(Layer::overlay, 21, 11, 349);
        } else if (taps_[(int)Key::action_1]) {
            PLATFORM.set_tile(Layer::overlay, 27, 10, 350);
        } else if (taps_[(int)Key::action_2]) {
            PLATFORM.set_tile(Layer::overlay, 26, 10, 351);
            PLATFORM.set_tile(Layer::overlay, 26, 11, 352);
        } else if (taps_[(int)Key::select]) {
            PLATFORM.set_tile(Layer::overlay, 21, 12, 353);
        } else if (taps_[(int)Key::start]) {
            PLATFORM.set_tile(Layer::overlay, 21, 11, 412);
        }
    }

    for (int i = 0; i < (int)Key::count; ++i) {
        if (not is_key_level_triggered((Key)i)) {
            prev_[i] = false;
        }
    }

    for (int i = 0; i < static_cast<int>(Key::count); ++i) {
        prev_[i] = states_[i];
    }

    for (int i = 0; i < (int)Key::count; ++i) {
        // These events are level-triggered, so we only want to keep this state
        // set for one update of the game loop, or else the key pressed events
        // will fire repeatedly. The start key is handled separately.
        // NOTE: alt_1 also handled separately.
        if (not is_key_level_triggered((Key)i)) {
            states_[i] = false;
        }
    }

    if (next_key_timeout_ <= 0) {

        if (next_timeout_key_) {
            const auto next = *next_timeout_key_;
            if (is_key_level_triggered(next)) {
                for (auto& t : taps_) {
                    t = false;
                }

                states_[(int)next] = not next_timeout_release_;
                taps_[(int)next] = not next_timeout_release_;
                if (next_timeout_release_) {
                    key_tap_timeout_ = milliseconds(300);
                } else {
                    key_tap_timeout_ = seconds(5);
                }
            } else {
                states_[(int)*next_timeout_key_] = true;
                for (int i = 0; i < (int)Key::count; ++i) {
                    if (not is_key_level_triggered((Key)i)) {
                        taps_[i] = false;
                    }
                }
                key_tap_timeout_ = milliseconds(300);
                taps_[(int)*next_timeout_key_] = true;
            }
            next_timeout_key_.reset();
        }

        auto first = ((lisp::Value*)keys_list_);
        if (first->type() == lisp::Value::Type::cons) {
            auto current = first->cons().car();
            if (current->type() == lisp::Value::Type::cons) {
                auto tm = current->cons().car();
                if (tm->type() == lisp::Value::Type::integer) {
                    next_key_timeout_ = milliseconds(tm->integer().value_);
                } else {
                    PLATFORM.fatal("invalid autopilot list format");
                }

                current = current->cons().cdr();
                if (current->type() == lisp::Value::Type::cons) {
                    auto key = current->cons().car();
                    if (key->type() == lisp::Value::Type::symbol) {
                        const char* name = key->symbol().name();
                        auto [key, state] = button_name_to_key(name);
                        next_timeout_key_ = key;
                        next_timeout_release_ = not state;
                    } else if (key->type() == lisp::Value::Type::string) {
                        PLATFORM.fill_overlay(0);
                        APP.dialog_buffer().emplace(
                            allocate_dynamic<DialogString>("dialog-buffer"));
                        **APP.dialog_buffer() += key->string().value();
                    }
                } else {
                    PLATFORM.fatal("invalid autopilot list format");
                }

            } else {
                PLATFORM.fatal("invalid autopilot list format");
            }

            keys_list_.set(first->cons().cdr());
        } else {
            APP.exit_condition() = App::ExitCondition::misc;
        }
    }
}



bool AutopilotPlayer::key_down(Key k)
{
    return states_[int(k)] and not prev_[int(k)];
}



bool AutopilotPlayer::key_up(Key k)
{
    return not states_[int(k)] and prev_[int(k)];
}



bool AutopilotPlayer::key_pressed(Key k)
{
    return states_[int(k)];
}



} // namespace skyland
