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


#include "weapon.hpp"
#include "globals.hpp"
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
               Microseconds reload_time)
    : Room(parent, name, position)
{
    parent->bulk_timer().schedule(this, reload_time);
}



Weapon::~Weapon()
{
    parent()->bulk_timer().deschedule(this);
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

    if (target_) {

        if (parent()->power_supply() < parent()->power_drain()) {
            parent()->bulk_timer().schedule(this, reload() - 1);
            Timer::__override_clock(0);
            return;
        }

        auto island = other_island();

        if (island and not island->is_destroyed()) {
            fire();

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



void Weapon::update(Microseconds delta)
{
    Room::update(delta);
}



void Weapon::rewind(Microseconds delta)
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



void Weapon::display_on_hover(Platform::Screen& screen,

                              const RoomCoord& cursor)
{
    if (not target_) {
        return;
    }

    auto target_island = other_island();

    if (target_island) {
        auto pos = target_island->visual_origin();
        pos.x += Fixnum::from_integer(target_->x * 16);
        pos.y += Fixnum::from_integer(target_->y * 16);

        Sprite spr;
        spr.set_position(pos);
        spr.set_texture_index(45);
        spr.set_size(Sprite::Size::w16_h32);

        screen.draw(spr);
    }
}



bool Weapon::target_pinned() const
{
    return target_pinned_;
}



void Weapon::set_target(const RoomCoord& target, bool pinned)
{
    if (target_ and *target_ == target) {
        // No need to waste space in rewind memory if the target does not
        // change.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = is_player_island(parent());
    e.previous_target_pinned_ = target_pinned_;

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    APP.time_stream().push(APP.level_timer(), e);

    target_ = target;
    target_pinned_ = pinned;
}



void Weapon::unset_target()
{
    if (not target_) {
        // Already uninitialized.
        return;
    }


    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = is_player_island(parent());
    e.previous_target_pinned_ = target_pinned_;

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    APP.time_stream().push(APP.level_timer(), e);

    target_.reset();
    target_pinned_ = false;
}



ScenePtr<Scene> Weapon::select(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (auto scn = reject_if_friendly()) {
        return scn;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return scene_pool::alloc<NotificationScene>(str->c_str(), future_scene);
    }


    if (is_player_island(parent())) {

        using Next = WeaponSetTargetScene;

        auto next =
            scene_pool::make_deferred_scene<Next>(position(), true, target_);

        if (APP.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(next);
        } else {
            return next();
        }
    }
    return null_scene();
}



} // namespace skyland
