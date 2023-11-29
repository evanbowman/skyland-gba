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


#include "playerP1.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/mycelium.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void PlayerP1::update(Microseconds delta)
{
    // Really dumb keylogger, for the tutorial levels. Dump lisp code to SRAM.

    if (APP.game_mode() == App::GameMode::tutorial) {
        StringBuffer<48> out = "(";

        if (PLATFORM.keyboard().down_transition<Key::left>()) {
            out += stringify(last_key_ / 1000);
            out += " Left)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::right>()) {
            out += stringify(last_key_ / 1000);
            out += " Right)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::up>()) {
            out += stringify(last_key_ / 1000);
            out += " Up)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::down>()) {
            out += stringify(last_key_ / 1000);
            out += " Down)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::action_1>()) {
            out += stringify(last_key_ / 1000);
            out += " A)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::action_2>()) {
            out += stringify(last_key_ / 1000);
            out += " B)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::alt_1>()) {
            out += stringify(last_key_ / 1000);
            out += " L-p)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::alt_2>()) {
            out += stringify(last_key_ / 1000);
            out += " R)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::start>()) {
            out += stringify(last_key_ / 1000);
            out += " Start-p)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().down_transition<Key::select>()) {
            out += stringify(last_key_ / 1000);
            out += " Select)";
            debug(out.c_str());
            last_key_ = 0;
        }

        if (PLATFORM.keyboard().up_transition<Key::start>()) {
            out = "(";
            out += stringify(last_key_ / 1000);
            out += " Start-np)";
            debug(out.c_str());
            last_key_ = 0;
        } else if (PLATFORM.keyboard().up_transition<Key::alt_1>()) {
            out = "(";
            out += stringify(last_key_ / 1000);
            out += " L-np)";
            debug(out.c_str());
            last_key_ = 0;
        }

        for (auto& timer : key_held_timers_) {
            timer = 0;
        }

    } else /* not a tutorial level */ {

        if (delta > 0) {
            if (touch_held(milliseconds(200))) {
                if (auto p = touch_current()) {
                    auto last = last_touch_;
                    auto velocity =
                        (p->cast<s32>() - last.cast<s32>()).cast<Float>();
                    velocity.x /= delta;
                    velocity.y /= delta;
                    // info(format("p__ % %", p->x, p->y));
                    // info(format("lst % %", last.x, last.y));
                    touch_velocity_.x =
                        ((19 * touch_velocity_.x) + velocity.x) / 20;
                    touch_velocity_.y =
                        ((19 * touch_velocity_.y) + velocity.y) / 20;


                    // info(format("vel % %",
                    //                   touch_velocity_.x,
                    //                   touch_velocity_.y));
                }
            } else {
                touch_velocity_ = {};
            }
        }


        if (auto t = PLATFORM.screen().touch()) {
            if (auto pos = t->read()) {
                last_touch_ = *pos;
                touch_held_time_ += delta;
            } else {
                last_touch_held_time_ = touch_held_time_;
                touch_held_time_ = 0;
            }
        } else {
            last_touch_held_time_ = touch_held_time_;
            touch_held_time_ = 0;
        }



        touch_invalidate_ = false;


        // Our tutorial keylogger does not log press&held, so we should not
        // record held keys unless the keylogger is off.

        for (int i = 0; i < static_cast<int>(Key::count); ++i) {
            if (PLATFORM.keyboard().pressed(static_cast<Key>(i))) {
                key_held_timers_[i] += delta;
            } else {
                key_held_timers_[i] = 0;
            }
        }

        if (PLATFORM.keyboard()
                .down_transition<Key::up, Key::down, Key::left, Key::right>()) {
            APP.camera()->reset_default();
        }
    }

    last_key_ += delta;
}



void PlayerP1::touch_consume()
{
    touch_invalidate_ = true;
}



SharedVariable score_multiplier("score_multiplier", 1);



void PlayerP1::on_room_destroyed(Room& room)
{
    if (room.parent() not_eq &APP.player_island()) {
        if (room.cast<Mycelium>()) {
            int mcount = 0;
            for (auto& oroom : APP.opponent_island()->rooms()) {
                if (oroom->metaclass_index() == room.metaclass_index()) {
                    ++mcount;
                }
            }
            if (mcount > 1) {
                // Only give player score for destroying the last remaining
                // mycelium block. Otherwise you could cheat by running up a
                // huge score.
                return;
            }
        }

        APP.score().set((APP.score().get() +
                         (score_multiplier * (*room.metaclass())->cost())));
    }
}



void PlayerP1::on_room_damaged(Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : APP.birds()) {
        if (bird->island() == island) {
            bird->signal();
        }
    }
}



void PlayerP1::on_room_plundered(Room& room)
{
    if (room.parent() not_eq &APP.player_island()) {
        APP.score().set(
            (APP.score().get() +
             1.5f * (score_multiplier * (*room.metaclass())->cost())));

        if (str_eq((*room.metaclass())->name(), "decimator")) {
            achievements::raise(achievements::Achievement::ancient_weapon);
        }
    }
}



bool PlayerP1::key_down(Key k)
{
    return PLATFORM.keyboard().down_transition(k);
}



bool PlayerP1::key_up(Key k)
{
    return PLATFORM.keyboard().up_transition(k);
}



bool PlayerP1::key_pressed(Key k)
{
    return PLATFORM.keyboard().pressed(k);
}



bool PlayerP1::key_held(Key k, Microseconds duration)
{
    return key_held_timers_[static_cast<int>(k)] >= duration;
}



void PlayerP1::key_held_reset(Key k, Microseconds decrement)
{
    key_held_timers_[static_cast<int>(k)] -= decrement;
}



void PlayerP1::key_held_distribute(const Key* include_list)
{
    // If any key in the include_list is pressed, distribute that key press to
    // all keys in the include list.

    int max = 0;

    auto l = include_list;

    while (*l not_eq Key::null) {
        if (key_held_timers_[static_cast<int>(*l)] > max) {
            max = key_held_timers_[static_cast<int>(*l)];
        }
        ++l;
    }

    l = include_list;

    while (*l not_eq Key::null) {
        if (PLATFORM.keyboard().pressed(*l)) {
            key_held_timers_[static_cast<int>(*l)] = max;
        }
        ++l;
    }
}



std::optional<std::tuple<Vec2<u32>, Microseconds>> PlayerP1::touch_released()
{
    if (touch_invalidate_) {
        return {};
    }

    if (auto t = PLATFORM.screen().touch()) {
        if (auto pos = t->up_transition()) {
            return std::make_tuple(*pos, last_touch_held_time_);
        }
    }
    return {};
}



std::optional<Vec2<u32>> PlayerP1::touch_current()
{
    if (touch_invalidate_) {
        return {};
    }

    if (auto t = PLATFORM.screen().touch()) {
        return t->read();
    }
    return {};
}



bool PlayerP1::touch_held(Microseconds duration)
{
    if (touch_invalidate_) {
        return false;
    }

    return touch_held_time_ >= duration;
}



} // namespace skyland
