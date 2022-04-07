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



void AutopilotPlayer::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : app.birds()) {
        if (bird->island(app) == island) {
            bird->signal(pfrm, app);
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
    } else if (str_cmp(name, "L") == 0) {
        return {Key::alt_1, true};
    } else {
        return {Key::alt_2, true};
    }
}



void AutopilotPlayer::update(Platform& pfrm, App& app, Microseconds delta)
{
    next_key_timeout_ -= delta;

    draw_image(pfrm, 290, 20, 8, 8, 6, Layer::overlay);

    key_tap_timeout_ -= delta;
    if (key_tap_timeout_ > 0) {
        if (taps_[(int)(Key::alt_1)]) {
            pfrm.set_tile(Layer::overlay, 20, 8, 338);
            pfrm.set_tile(Layer::overlay, 21, 8, 339);
        } else if (taps_[(int)Key::alt_2]) {
            pfrm.set_tile(Layer::overlay, 26, 8, 340);
            pfrm.set_tile(Layer::overlay, 27, 8, 341);
        } else if (taps_[(int)Key::right]) {
            pfrm.set_tile(Layer::overlay, 21, 10, 342);
        } else if (taps_[(int)Key::left]) {
            pfrm.set_tile(Layer::overlay, 20, 10, 343);
        } else if (taps_[(int)Key::up]) {
            pfrm.set_tile(Layer::overlay, 20, 10, 344);
            pfrm.set_tile(Layer::overlay, 21, 10, 345);
        } else if (taps_[(int)Key::down]) {
            pfrm.set_tile(Layer::overlay, 20, 10, 346);
            pfrm.set_tile(Layer::overlay, 21, 10, 347);
            pfrm.set_tile(Layer::overlay, 20, 11, 348);
            pfrm.set_tile(Layer::overlay, 21, 11, 349);
        } else if (taps_[(int)Key::action_1]) {
            pfrm.set_tile(Layer::overlay, 27, 10, 350);
        } else if (taps_[(int)Key::action_2]) {
            pfrm.set_tile(Layer::overlay, 26, 10, 351);
            pfrm.set_tile(Layer::overlay, 26, 11, 352);
        } else if (taps_[(int)Key::select]) {
            pfrm.set_tile(Layer::overlay, 21, 12, 353);
        } else if (taps_[(int)Key::start]) {
            pfrm.set_tile(Layer::overlay, 21, 11, 412);
        }
    }

    for (int i = 0; i < (int)Key::count; ++i) {
        if ((Key)i not_eq Key::start) {
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
        if ((Key)i not_eq Key::start) {
            states_[i] = false;
        }
    }

    if (next_key_timeout_ <= 0) {

        if (next_timeout_key_) {
            if (*next_timeout_key_ == Key::start) {
                for (auto& t : taps_) {
                    t = false;
                }

                states_[(int)Key::start] = not next_timeout_release_;
                taps_[(int)Key::start] = not next_timeout_release_;
                if (next_timeout_release_) {
                    key_tap_timeout_ = milliseconds(300);
                } else {
                    key_tap_timeout_ = seconds(5);
                }
            } else {
                states_[(int)*next_timeout_key_] = true;
                for (int i = 0; i < (int)Key::count; ++i) {
                    if ((Key)i not_eq Key::start) {
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
                    pfrm.fatal("invalid autopilot list format");
                }

                current = current->cons().cdr();
                if (current->type() == lisp::Value::Type::cons) {
                    auto key = current->cons().car();
                    if (key->type() == lisp::Value::Type::symbol) {
                        const char* name = key->symbol().name_;
                        auto [key, state] = button_name_to_key(name);
                        next_timeout_key_ = key;
                        next_timeout_release_ = not state;
                    } else if (key->type() == lisp::Value::Type::string) {
                        app.dialog_buffer().emplace(
                            allocate_dynamic<DialogString>("dialog-buffer"));
                        **app.dialog_buffer() += key->string().value();
                    }
                } else {
                    pfrm.fatal("invalid autopilot list format");
                }

            } else {
                pfrm.fatal("invalid autopilot list format");
            }

            keys_list_.set(first->cons().cdr());
        } else {
            app.exit_condition() = App::ExitCondition::misc;
        }
    }
}



bool AutopilotPlayer::key_down(Platform&, Key k)
{
    return states_[int(k)] and not prev_[int(k)];
}



bool AutopilotPlayer::key_up(Platform&, Key k)
{
    return not states_[int(k)] and prev_[int(k)];
}



bool AutopilotPlayer::key_pressed(Platform&, Key k)
{
    return states_[int(k)];
}



} // namespace skyland
