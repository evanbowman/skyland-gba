#include "playerP1.hpp"
#include "skyland.hpp"
#include "localization.hpp"



namespace skyland {



void PlayerP1::update(Platform& pfrm, App& app, Microseconds delta)
{
    // Really dumb keylogger, for the tutorial levels. Dump lisp code to SRAM.

    if (app.tutorial_mode()) {
        StringBuffer<48> out = "(";

        if (pfrm.keyboard().down_transition<Key::left>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " Left)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::right>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " Right)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::up>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " Up)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::down>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " Down)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::action_1>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " A)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::action_2>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " B)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::alt_1>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " L)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::alt_2>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " R)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::start>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " Start)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        } else if (pfrm.keyboard().down_transition<Key::select>()) {
            out += to_string<20>(last_key_ / 1000);
            out += " Select)";
            debug(pfrm, out.c_str());
            last_key_ = 0;
        }
    }


    last_key_ += delta;
}




void PlayerP1::on_room_destroyed(Platform& pfrm, App& app, Room& room)
{

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



}
