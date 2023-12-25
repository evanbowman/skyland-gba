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
               Time reload_time)
    : Room(parent, name, position)
{
    parent->bulk_timer().schedule(this, reload_time);
}



void Weapon::on_powerchange()
{
    if (is_powered_down()) {
        parent()->bulk_timer().deschedule(this);
    } else {
        auto rem = Timer::remaining();
        Timer::__override_clock(0);
        parent()->bulk_timer().schedule(this, rem);
    }
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
    if (is_powered_down()) {
        return;
    }

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



ScenePtr<Scene> Weapon::select_impl(const RoomCoord& cursor)
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
