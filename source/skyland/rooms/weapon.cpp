////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "weapon.hpp"
#include "globals.hpp"
#include "skyland/minimap.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Weapon::Weapon(Island* parent,
               const char* name,
               const RoomCoord& position,
               Time reload_time)
    : Room(parent, name, position)
{
    parent->bulk_timer().schedule(this, reload_time);
}



void Weapon::on_powerchange()
{
    override_reload_timer(reload());

    if (is_powered_down()) {
        parent()->bulk_timer().deschedule(this);
    } else {
        parent()->bulk_timer().schedule(this, 0);
    }
}



Weapon::~Weapon()
{
    parent()->bulk_timer().deschedule(this);
}



Weapon* Weapon::cast_weapon()
{
    return this;
}



bool Weapon::allows_powerdown()
{
    return true;
}



void Weapon::finalize()
{
    Room::finalize();

    finalize_weapon();

    clear_target_queue();
}



void Weapon::finalize_weapon()
{
}



void Weapon::timer_expired()
{
    if (Timer::interval() == reload()) {
        // We need to store the point at which we finished reloading. A
        // weapon does not fire if it does not have a target, or if the
        // parent island's power drain exceeds its power supply. So,
        // basically, we have no way of knowing how to roll back a reload
        // timer in certain cases. We could perhaps save some space by not
        // pushing this event in cases where we know that it's most likely
        // unnecessary.
        if (is_player_island(parent())) {
            time_stream::event::PlayerRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            APP.time_stream().push(APP.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            APP.time_stream().push(APP.level_timer(), e);
        }
    }

    if (not target_queue_.empty()) {

        if (is_powered_down()) {
            unset_target();
            parent()->bulk_timer().schedule(this, reload() - 1);
            Timer::__override_clock(0);
            return;
        }

        if (parent()->power_supply() < parent()->power_drain()) {
            parent()->bulk_timer().schedule(this, reload() - 1);
            Timer::__override_clock(0);
            return;
        }

        if (parent()->phase()) {
            parent()->bulk_timer().schedule(this, reload() - 1);
            Timer::__override_clock(0);
            return;
        }

        auto island = other_island();

        if (island and not island->is_destroyed()) {

            update_targets();

            // This check for friendly status is partly just a sanity check. It
            // shouldn't be possible to set a weapon target if the opposing
            // island is friendly. But, just in case...
            if (not APP.opponent().is_friendly()) {
                fire();
            }

            // If we emit a projectile, the AI player pretty much knows what
            // block we are, even if we're concealed by cloaking.
            set_ai_aware(true);

            parent()->bulk_timer().schedule(this, reload());
            return;
        }
    }

    parent()->bulk_timer().schedule(this, reload() - 1); // FIXME: using
                                                         // reload() above to
                                                         // record events.
    Timer::__override_clock(0);
}



void Weapon::update_targets()
{
    auto island = other_island();
    if (not island) {
        return;
    }

    while (target_queue_.size() > 1 and
           not island->get_room(target_queue_.back().coord())) {

        time_stream::event::TargetQueuePop e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;
        e.queue_elem_x_ = target_queue_.back().x_;
        e.queue_elem_y_ = target_queue_.back().y_;
        APP.time_stream().push(APP.level_timer(), e);

        target_queue_.pop_back();
    }
}



void Weapon::__rewind_push_target_queue(const RoomCoord& target)
{
    target_queue_.push_back(PackedTarget::pack(target));
}



void Weapon::update(Time delta)
{
    Room::update(delta);
}



void Weapon::rewind(Time delta)
{
    Room::rewind(delta);
}



void Weapon::___rewind___finished_reload()
{
    // NOTE: the reload logic, above, increments the reload counter if the
    // reload timer is greater than zero. A bit hacky, but better than wasting
    // memory with a state variable for this relatively simple purpose.
    Timer::__override_clock(1);
}



void Weapon::___rewind___ability_used()
{
    // NOTE: the weapon just fired, i.e. before it fired, its reload timer must
    // have been either zero or near zero. We could technically store the exact
    // value of the timer, but doing so would use more memory, and,
    // realistically, the timer value would only have been negative by some
    // small number of bytes; at most, one frame.
    //
    // Generally, for weapons, the projectile itself is responsible for calling
    // this function, when it's clock rewinds past zero.
    //
    Timer::__override_clock(0);
}



void show_target_queue(Island& isle, const TargetQueue& target_queue)
{
    static const int reticule_spr_idx = 95;

    Sprite::Alpha alpha = Sprite::Alpha::opaque;

    for (int i = target_queue.size() - 1; i > -1; --i) {
        auto target = target_queue[i].coord();

        auto pos = isle.visual_origin();
        pos.x += Fixnum::from_integer(target.x * 16);
        pos.y += Fixnum::from_integer(target.y * 16);

        Sprite spr;
        spr.set_position(pos);
        spr.set_tidx_16x16(reticule_spr_idx, 1);
        spr.set_size(Sprite::Size::w16_h16);
        spr.set_alpha(alpha);

        PLATFORM.screen().draw(spr);

        alpha = Sprite::Alpha::translucent;
    }

    minimap::draw_weapon_targets(target_queue);
}



void Weapon::display_on_hover(Platform::Screen& screen, const RoomCoord& cursor)
{
    if (not get_target()) {
        return;
    }

    auto target_island = other_island();

    if (target_island) {
        show_target_queue(*target_island, target_queue());
    }
}



Time Weapon::reload() const
{
    Time base_time = reload_impl();

    if (amplify_) {
        return (base_time * 5) / 8;
    } else {
        return base_time;
    }
}



void Weapon::amplify(bool enabled)
{
    amplify_ = enabled;
}



bool Weapon::target_pinned() const
{
    return target_pinned_;
}



void Weapon::set_target(const RoomCoord& target, bool pinned)
{
    if (is_powered_down()) {
        return;
    }

    if (get_target() and *get_target() == target) {
        // No need to waste space in rewind memory if the target does not
        // change.
        return;
    }

    clear_target_queue();

    target_queue_.push_back(PackedTarget::pack(target));
    target_pinned_ = pinned;
}



Room::TargetCount Weapon::target_count() const
{
    return target_queue_.size();
}



void Weapon::set_target(const TargetQueue& tq, bool pinned)
{
    if (is_powered_down()) {
        return;
    }

    if (tq.empty()) {
        return;
    }
    if (tq.size() == 1) {
        if (target_queue_.size() > 1) {
            target_queue_.clear();
        }
        set_target(tq[0].coord(), pinned);
        return;
    }

    clear_target_queue();

    for (int i = tq.size() - 1; i > -1; --i) {
        target_queue_.push_back(tq[i]);
    }

    target_pinned_ = pinned;
}



void Weapon::clear_target_queue()
{
    if (target_queue_.size() < 2) {
        time_stream::event::WeaponSetTarget e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;

        e.near_ = is_player_island(parent());
        e.previous_target_pinned_ = target_pinned_;

        if (get_target()) {
            e.previous_target_x_ = get_target()->x;
            e.previous_target_y_ = get_target()->y;
            e.has_previous_target_ = true;
        } else {
            e.previous_target_x_ = 0;
            e.previous_target_y_ = 0;
            e.has_previous_target_ = false;
        }

        APP.time_stream().push(APP.level_timer(), e);

    } else {

        if (APP.is_developer_mode() and not is_player_island(parent())) {
            // TargetQueuePop events assume that the player's weapons generated
            // them.
            PLATFORM.fatal("opponent has a target queue!?");
        }

        for (int i = target_queue_.size() - 1; i > -1; --i) {
            time_stream::event::TargetQueuePop e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;

            auto elem = target_queue_[i];
            e.queue_elem_x_ = elem.x_;
            e.queue_elem_y_ = elem.y_;

            APP.time_stream().push(APP.level_timer(), e);
        }

        time_stream::event::TargetQueueClear e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;
        APP.time_stream().push(APP.level_timer(), e);
    }

    target_queue_.clear();
}



void Weapon::unset_target()
{
    if (not get_target()) {
        // Already uninitialized.
        return;
    }

    clear_target_queue();

    target_pinned_ = false;
}



ScenePtr Weapon::select_impl(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (is_powered_down()) {
        return null_scene();
    }

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (auto scn = reject_if_friendly()) {
        return scn;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }


    if (is_player_island(parent())) {

        using Next = WeaponSetTargetScene;

        auto next = make_deferred_scene<Next>(position(), true, get_target());

        if (APP.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(next);
        } else {
            return next();
        }
    }
    return null_scene();
}



const TargetQueue& Weapon::target_queue() const
{
    return target_queue_;
}



} // namespace skyland
