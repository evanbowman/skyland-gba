#include "autopilotPlayer.hpp"
#include "skyland.hpp"
#include "localization.hpp"




namespace skyland {



AutopilotPlayer::AutopilotPlayer(lisp::Value* keys_list) :
    keys_list_(keys_list)
{
    for (auto& s : prev_) {
        s = false;
    }

    for (auto& s : states_) {
        s = false;
    }

}



static Key button_name_to_key(const char* name)
{
    if (str_cmp(name, "Right") == 0) {
        return Key::right;
    } else if (str_cmp(name, "Left") == 0) {
        return Key::left;
    } else if (str_cmp(name, "Up") == 0) {
        return Key::up;
    } else if (str_cmp(name, "Down") == 0) {
        return Key::down;
    } else if (str_cmp(name, "A") == 0) {
        return Key::action_1;
    } else if (str_cmp(name, "B") == 0) {
        return Key::action_2;
    } else if (str_cmp(name, "Start") == 0) {
        return Key::start;
    } else if (str_cmp(name, "Select") == 0) {
        return Key::select;
    } else if (str_cmp(name, "L") == 0) {
        return Key::alt_1;
    } else {
        return Key::alt_2;
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
        }
    }

    for (auto& s : prev_) {
        s = false;
    }

    for (int i = 0; i < static_cast<int>(Key::count); ++i) {
        prev_[i] = states_[i];
    }

    for (auto& s : states_) {
        // These events are level-triggered, so we only want to keep this state
        // set for one update of the game loop, or else the key pressed events
        // will fire repeatedly.
        s = false;
    }

    if (next_key_timeout_ <= 0) {

        if (next_timeout_key_) {
            states_[(int)*next_timeout_key_] = true;
            for (auto& k : taps_) {
                k = false;
            }
            key_tap_timeout_ = milliseconds(300);
            taps_[(int)*next_timeout_key_] = true;
            next_timeout_key_.reset();
        }

        auto first = ((lisp::Value*)keys_list_);
        if (first->type_ == lisp::Value::Type::cons) {
            auto current = first->cons_.car();
            if (current->type_ == lisp::Value::Type::cons) {
                auto tm = current->cons_.car();
                if (tm->type_ == lisp::Value::Type::integer) {
                    next_key_timeout_ = milliseconds(tm->integer_.value_);
                } else {
                    pfrm.fatal("invalid autopilot list format");
                }

                current = current->cons_.cdr();
                if (current->type_ == lisp::Value::Type::cons) {
                    auto key = current->cons_.car();
                    if (key->type_ == lisp::Value::Type::symbol) {
                        const char* name = key->symbol_.name_;
                        next_timeout_key_ = button_name_to_key(name);
                    } else if (key->type_ == lisp::Value::Type::string) {
                        app.dialog_buffer().emplace(allocate_dynamic<DialogString>(pfrm));
                        **app.dialog_buffer() += key->string_.value();
                    }
                } else {
                    pfrm.fatal("invalid autopilot list format");
                }

            } else {
                pfrm.fatal("invalid autopilot list format");
            }

            keys_list_.set(first->cons_.cdr());
        } else {
            app.exit_level() = true;
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



}
