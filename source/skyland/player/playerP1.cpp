////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "playerP1.hpp"
#include "skyland/minimap.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/mycelium.hpp"
#include "skyland/rooms/targetingComputer.hpp"
#include "skyland/rooms/warhead.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



PlayerP1::PlayerP1() : ai_state_(allocate_dynamic<AIState>("island-ai-control"))
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

        update_ai(delta);

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

        ai_state_->rescan_ = true;

        auto add_score = score_multiplier * (*room.metaclass())->cost();

        if (not(APP.persistent_data().state_flags_.get() &
                PersistentData::permadeath_on)) {
            add_score /= 2;
        }

        APP.score().set(APP.score().get() + add_score);

    } else {

        auto p = room.position();
        auto sz = room.size();

        for (int x = p.x; x < p.x + sz.x; ++x) {
            for (int y = p.y; y < p.y + sz.y; ++y) {
                minimap::player_destroyed_rooms.set(x, y, true);
            }
        }
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



void PlayerP1::update_ai(Time delta)
{
    ai_state_->update(delta);
}



void PlayerP1::AIState::update_weapon_targets(Time delta)
{
    if (PLATFORM.screen().fade_active()) {
        return;
    }

    if (not opponent_island()) {
        return;
    }

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        // Bugfix: If stuff is even slightly de-syncd upon level entry, one game
        // can jump ahead of another. Add in an additional seconds buffer to
        // increase the liklihood that the targeting computer assigns weapon
        // targets at the same time.
        next_weapon_action_timer_ = seconds(4);
        return;
    }

    if (APP.opponent().is_friendly()) {
        // Don't start selecting targets when playing the level entry animation
        // and stuff.
        for (auto& r : APP.player_island().rooms()) {
            if (not r->target_pinned()) {
                r->unset_target();
            }
        }
        return;
    }

    if (next_weapon_action_timer_ > 0) {
        next_weapon_action_timer_ -= delta;
    } else {
        if (weapon_update_index_ >= player_island().rooms().size()) {
            weapon_update_index_ = 0;
            next_weapon_action_timer_ = seconds(3);
        } else {
            auto& room = *player_island().rooms()[weapon_update_index_++];

            const auto category = (*room.metaclass())->category();

            if (auto opt_drone = room.drone()) {
                PlayerP1::autoassign_drone_target(**opt_drone);
                next_weapon_action_timer_ = milliseconds(64);
            } else if (category == Room::Category::weapon) {
                PlayerP1::autoassign_weapon_target(room);
                next_weapon_action_timer_ = milliseconds(64);
            } else {
                next_weapon_action_timer_ = milliseconds(32);
            }
        }
    }

    // A block was destroyed. Attempt to re-assign some weapon targets, for
    // weapons that no longer have a valid target block.
    if (rescan_) {

        ATP highest_weight = 0.0_atp;
        Optional<RoomCoord> highest_weighted_target;

        // Find the highest-weighted target still in existence.
        for (auto& room : APP.player_island().rooms()) {
            const auto category = (*room->metaclass())->category();
            if (category == Room::Category::weapon) {
                if (auto target = room->get_target()) {
                    if (auto o = APP.opponent_island()->get_room(*target)) {
                        if (o->get_atp() > highest_weight) {
                            highest_weight = o->get_atp();
                            highest_weighted_target = target;
                        }
                    }
                }
            }
        }

        // Assign the highest weighted extant target to all rooms that no longer
        // have a valid target block.
        if (highest_weighted_target) {
            for (auto& room : APP.player_island().rooms()) {
                const auto category = (*room->metaclass())->category();
                if (category == Room::Category::weapon) {
                    if (auto target = room->get_target()) {
                        if (not APP.opponent_island()->get_room(*target)) {
                            room->set_target(*highest_weighted_target, false);
                        }
                    }
                }
            }
        }

        rescan_ = false;
    }
}



void PlayerP1::autoassign_drone_target(Drone& drone)
{
    const bool has_pinned_target = drone.target_pinned() and drone.get_target();

    if (has_pinned_target and
        (not drone.target_near() and
         (APP.opponent_island()->get_drone(*drone.get_target()) or
          APP.opponent_island()->get_room(*drone.get_target())))) {
        // Do not override the pinned target
    } else {
        EnemyAI::drone_set_target(APP.opponent_island()->rooms_plot(),
                                  drone,
                                  &APP.player_island(),
                                  APP.opponent_island());
    }
}



void PlayerP1::autoassign_weapon_target(Room& room)
{
    if (room.is_powered_down()) {
        return;
    }

    // Even when the targeting AI is active, the game allows you to
    // pin targets manually, and the AI won't try to assign them
    // again until the block to which the target is pinned is
    // destroyed.

    const bool has_pinned_target =
        room.target_pinned() and room.get_target() and
        APP.opponent_island()->get_room(*room.get_target());

    if (not has_pinned_target and room.target_count() < 2 and
        not room.cast<Warhead>()) {
        EnemyAI::update_room(room,
                             APP.opponent_island()->rooms_plot(),
                             &APP.player(),
                             &APP.player_island(),
                             APP.opponent_island());
    }
}



void PlayerP1::reassign_all_weapon_targets()
{
    for (auto& room : APP.player_island().rooms()) {
        const auto category = (*(*room).metaclass())->category();
        if (category == Room::Category::weapon) {
            autoassign_weapon_target(*room);
        }
    }
}



void PlayerP1::update_weapon_targets(Time delta)
{
    ai_state_->update_weapon_targets(delta);
}



void PlayerP1::AIState::update(Time delta)
{
    if (PLATFORM.screen().fade_active()) {
        return;
    }

    if (APP.game_speed() == GameSpeed::stopped) {
        return;
    }

    // if (APP.gp_.stateflags_.get(GlobalPersistentData::autofire_on)) {
    //     update_weapon_targets(APP.delta_fp().as_integer());
    // }

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
                EnemyAI::assign_local_character(
                    *info.first, &APP.player(), &APP.player_island(), true);
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



void PlayerP1::on_level_start()
{
    ai_state_->next_weapon_action_timer_ = seconds(3);
    ai_state_->rescan_ = false;
}



void PlayerP1::delay_autofire(Time duration)
{
    ai_state_->next_weapon_action_timer_ = duration;
    ai_state_->rescan_ = false;
}



void PlayerP1::delay_crew_automation(Time duration)
{
    ai_state_->next_action_timer_ = duration;
}



} // namespace skyland
