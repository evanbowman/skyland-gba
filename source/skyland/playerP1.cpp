#include "playerP1.hpp"
#include "room_metatable.hpp"
#include "skyland.hpp"



namespace skyland {



void PlayerP1::update(Platform& pfrm, App& app, Microseconds delta)
{
    // Really dumb keylogger, for the tutorial levels. Dump lisp code to SRAM.

    if (app.game_mode() == App::GameMode::tutorial) {
        StringBuffer<48> out = "(";

        if (pfrm.keyboard().down_transition<Key::left>()) {
            out += stringify(last_key_ / 1000);
            out += " Left)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::right>()) {
            out += stringify(last_key_ / 1000);
            out += " Right)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::up>()) {
            out += stringify(last_key_ / 1000);
            out += " Up)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::down>()) {
            out += stringify(last_key_ / 1000);
            out += " Down)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::action_1>()) {
            out += stringify(last_key_ / 1000);
            out += " A)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::action_2>()) {
            out += stringify(last_key_ / 1000);
            out += " B)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::alt_1>()) {
            out += stringify(last_key_ / 1000);
            out += " L)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::alt_2>()) {
            out += stringify(last_key_ / 1000);
            out += " R)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::start>()) {
            out += stringify(last_key_ / 1000);
            out += " Start)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::select>()) {
            out += stringify(last_key_ / 1000);
            out += " Select)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        }

        for (auto& timer : key_held_timers_) {
            timer = 0;
        }

    } else /* not a tutorial level */ {

        // Our tutorial keylogger does not log press&held, so we should not
        // record held keys unless the keylogger is off.

        for (int i = 0; i < static_cast<int>(Key::count); ++i) {
            if (pfrm.keyboard().pressed(static_cast<Key>(i))) {
                key_held_timers_[i] += delta;
            } else {
                key_held_timers_[i] = 0;
            }
        }
    }



    last_key_ += delta;
}



void PlayerP1::on_room_destroyed(Platform& pfrm, App& app, Room& room)
{
    if (room.parent() not_eq &app.player_island()) {
        app.score().set(app.score().get() + (*room.metaclass())->cost());
    }
}



void PlayerP1::on_room_plundered(Platform& pfrm, App& app, Room& room)
{
    if (room.parent() not_eq &app.player_island()) {
        app.score().set(app.score().get() + 1.2f * (*room.metaclass())->cost());

        // Unlock the decimator structure if the player plunders it from an
        // opponent castle.
        if (str_eq((*room.metaclass())->name(), "decimator")) {
            set_enabled(metaclass_index("decimator"), true);
        }
    }
}



bool PlayerP1::key_down(Platform& pfrm, Key k)
{
    return pfrm.keyboard().down_transition(k);
}



bool PlayerP1::key_up(Platform& pfrm, Key k)
{
    return pfrm.keyboard().up_transition(k);
}



bool PlayerP1::key_pressed(Platform& pfrm, Key k)
{
    return pfrm.keyboard().pressed(k);
}



bool PlayerP1::key_held(Key k, Microseconds duration)
{
    return key_held_timers_[static_cast<int>(k)] >= duration;
}



void PlayerP1::key_held_reset(Key k, Microseconds decrement)
{
    key_held_timers_[static_cast<int>(k)] -= decrement;
}



void PlayerP1::key_held_distribute(Platform& pfrm)
{
    int max = 0;
    for (auto tm : key_held_timers_) {
        if (tm > max) {
            max = tm;
        }
    }


    for (int i = 0; i < static_cast<int>(Key::count); ++i) {
        if (pfrm.keyboard().pressed(static_cast<Key>(i))) {
            key_held_timers_[i] = max;
        }
    }
}



} // namespace skyland
