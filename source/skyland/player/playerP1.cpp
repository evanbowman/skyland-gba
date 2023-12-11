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


#include "playerP1.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/mycelium.hpp"
#include "skyland/rooms/targetingComputer.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



PlayerP1::PlayerP1() : chr_ai_(allocate_dynamic<ChrAIState>("crew-ai-control"))
{
}



void PlayerP1::update(Time delta)
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

        // Our tutorial keylogger does not log press&held, so we should not
        // record held keys unless the keylogger is off.

        for (int i = 0; i < static_cast<int>(Key::count); ++i) {
            if (PLATFORM.keyboard().pressed(static_cast<Key>(i))) {
                key_held_timers_[i] += delta;
            } else {
                key_held_timers_[i] = 0;
            }
        }

        update_chr_ai(delta);

        if (PLATFORM.keyboard()
                .down_transition<Key::up, Key::down, Key::left, Key::right>()) {
            APP.camera()->reset_default();
        }
    }

    last_key_ += delta;
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

        for (auto& r : APP.player_island().rooms()) {
            if (auto tc = r->cast<TargetingComputer>()) {
                tc->rescan();
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



bool PlayerP1::key_held(Key k, Time duration)
{
    return key_held_timers_[static_cast<int>(k)] >= duration;
}



void PlayerP1::key_held_reset(Key k, Time decrement)
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



void PlayerP1::update_chr_ai(Time delta)
{
    chr_ai_->update(delta);
}



void PlayerP1::ChrAIState::update(Time delta)
{
    if (not APP.opponent_island()) {
        return;
    }

    if (PLATFORM.screen().fade_active()) {
        return;
    }

    if (APP.game_speed() == GameSpeed::stopped) {
        return;
    }

    next_action_timer_ -= delta;
    if (next_action_timer_ <= 0) {
        next_action_timer_ = milliseconds(1500);

        if (local_chrs_.empty() or local_buffer_index_ >= local_chrs_.size()) {
            local_buffer_index_ = 0;
            local_chrs_.clear();

            for (auto& room : APP.player_island().rooms()) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &APP.player() and
                        not chr->co_op_locked()) {
                        local_chrs_.push_back(chr->id());
                    }
                }
            }
        }

        if (not local_chrs_.empty()) {
            auto chr_id = (local_chrs_)[local_buffer_index_++];
            auto info = APP.player_island().find_character_by_id(chr_id);
            if (info.first and info.first->ai_automated() and
                not info.first->is_superpinned()) {
                EnemyAI::assign_local_character(*info.first,
                                                &APP.player(),
                                                &APP.player_island(),
                                                APP.opponent_island(),
                                                true);
            }
        }

        if (boarded_chrs_.empty() or
            boarded_buffer_index_ >= boarded_chrs_.size()) {
            boarded_buffer_index_ = 0;
            boarded_chrs_.clear();

            if (APP.opponent_island()) {
                for (auto& room : APP.opponent_island()->rooms()) {
                    for (auto& chr : room->characters()) {
                        if (chr->owner() == &APP.player() and
                            not chr->co_op_locked()) {
                            boarded_chrs_.push_back(chr->id());
                        }
                    }
                }
            }
        }

        if (not boarded_chrs_.empty()) {
            auto chr_id = (boarded_chrs_)[boarded_buffer_index_++];
            if (APP.opponent_island()) {
                auto info = APP.opponent_island()->find_character_by_id(chr_id);
                if (info.first and info.first->ai_automated() and
                    not info.first->is_superpinned()) {
                    EnemyAI::assign_boarded_character(*info.first,
                                                      &APP.player(),
                                                      &APP.player_island(),
                                                      APP.opponent_island());
                }
            }
        }
    }
}



} // namespace skyland
