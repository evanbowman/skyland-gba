#include "playerP1.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/touchscreenFreeformCamera.hpp"



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

        if (delta > 0) {
            if (touch_held(milliseconds(200))) {
                if (auto p = touch_current(pfrm)) {
                    auto last = last_touch_;
                    auto velocity =
                        (p->cast<s32>() - last.cast<s32>()).cast<Float>();
                    velocity.x /= delta;
                    velocity.y /= delta;
                    // info(pfrm, format("p__ % %", p->x, p->y));
                    // info(pfrm, format("lst % %", last.x, last.y));
                    touch_velocity_.x =
                        ((19 * touch_velocity_.x) + velocity.x) / 20;
                    touch_velocity_.y =
                        ((19 * touch_velocity_.y) + velocity.y) / 20;


                    // info(pfrm, format("vel % %",
                    //                   touch_velocity_.x,
                    //                   touch_velocity_.y));
                }
            } else {
                touch_velocity_ = {};
            }
        }


        if (auto t = pfrm.screen().touch()) {
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
            if (pfrm.keyboard().pressed(static_cast<Key>(i))) {
                key_held_timers_[i] += delta;
            } else {
                key_held_timers_[i] = 0;
            }
        }

        if (pfrm.keyboard()
                .down_transition<Key::up, Key::down, Key::left, Key::right>()) {
            app.camera()->reset_default(app);
        }
    }

    last_key_ += delta;
}



void PlayerP1::touch_consume()
{
    touch_invalidate_ = true;
}



static SharedVariable score_multiplier("score_multiplier", 1);



void PlayerP1::on_room_destroyed(Platform& pfrm, App& app, Room& room)
{
    if (room.parent() not_eq &app.player_island()) {
        app.score().set((app.score().get() +
                         (score_multiplier * (*room.metaclass())->cost())));
    }
}



void PlayerP1::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : app.birds()) {
        if (bird->island(app) == island) {
            bird->signal(pfrm, app);
        }
    }
}



void PlayerP1::on_room_plundered(Platform& pfrm, App& app, Room& room)
{
    if (room.parent() not_eq &app.player_island()) {
        app.score().set(
            (app.score().get() +
             1.2f * (score_multiplier * (*room.metaclass())->cost())));

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



void PlayerP1::key_held_distribute(Platform& pfrm, const Key* include_list)
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
        if (pfrm.keyboard().pressed(*l)) {
            key_held_timers_[static_cast<int>(*l)] = max;
        }
        ++l;
    }
}



std::optional<std::tuple<Vec2<u32>, Microseconds>>
PlayerP1::touch_released(Platform& pfrm)
{
    if (touch_invalidate_) {
        return {};
    }

    if (auto t = pfrm.screen().touch()) {
        if (auto pos = t->up_transition()) {
            return std::make_tuple(*pos, last_touch_held_time_);
        }
    }
    return {};
}



std::optional<Vec2<u32>> PlayerP1::touch_current(Platform& pfrm)
{
    if (touch_invalidate_) {
        return {};
    }

    if (auto t = pfrm.screen().touch()) {
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
