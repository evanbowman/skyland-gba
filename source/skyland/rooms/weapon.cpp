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
    reload_timer_ = reload_time;
}



void Weapon::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    if (reload_timer_ > 0) {
        reload_timer_ -= delta;
        if (reload_timer_ < 0) {
            // We need to store the point at which we finished reloading. A
            // weapon does not fire if it does not have a target, or if the
            // parent island's power drain exceeds its power supply. So,
            // basically, we have no way of knowing how to roll back a reload
            // timer in certain cases. We could perhaps save some space by not
            // pushing this event in cases where we know that it's most likely
            // unnecessary.
            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            }
        }
    } else if (target_) {

        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island(app);

        if (island and not island->is_destroyed()) {
            fire(pfrm, app);
            reload_timer_ += reload();
        }
    }
}



void Weapon::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

    if (reload_timer_ <= 0) {
        // Nothing to do, fully reloaded weapon.
    } else if (reload_timer_ < reload()) {
        // Ok, so a weapon was in the process of reloading, but the reload timer
        // is not yet zero, so we want to increment the timer until it reaches
        // the reload interval. Remember, we're going back in time.
        reload_timer_ += delta;
    }
}



void Weapon::___rewind___finished_reload(Platform&, App&)
{
    // NOTE: the reload logic, above, increments the reload counter if the
    // reload timer is greater than zero. A bit hacky, but better than wasting
    // memory with a state variable for this relatively simple purpose.
    reload_timer_ = 1;
}



void Weapon::___rewind___ability_used(Platform&, App&)
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
    reload_timer_ = 0;
}



void Weapon::display_on_hover(Platform::Screen& screen,
                              App& app,
                              const RoomCoord& cursor)
{
    if (not target_) {
        return;
    }

    auto target_island = other_island(app);

    if (target_island) {
        auto pos = target_island->visual_origin();
        pos.x += target_->x * 16;
        pos.y += target_->y * 16;

        Sprite spr;
        spr.set_position(pos);
        spr.set_texture_index(45);
        spr.set_size(Sprite::Size::w16_h32);

        screen.draw(spr);
    }
}



void Weapon::set_target(Platform& pfrm, App& app, const RoomCoord& target)
{
    if (target_ and *target_ == target) {
        // No need to waste space in rewind memory if the target does not
        // change.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = parent() == &app.player_island();

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    app.time_stream().push(app.level_timer(), e);

    target_ = target;
}



void Weapon::unset_target(Platform& pfrm, App& app)
{
    if (not target_) {
        // Already uninitialized.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = parent() == &app.player_island();

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    app.time_stream().push(app.level_timer(), e);

    target_.reset();
}



ScenePtr<Scene>
Weapon::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (app.opponent_island() and
        // NOTE: cast should be safe, as a derived instance of Opponent should
        // always be bound to the opponent island.
        (static_cast<Opponent&>(app.opponent_island()->owner()))
            .is_friendly()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        pfrm.speaker().play_sound("beep_error", 3);
        auto str = SYSTR(error_friendly);
        return scene_pool::alloc<NotificationScene>(str->c_str(), future_scene);
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        pfrm.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_power_out);
        return scene_pool::alloc<NotificationScene>(str->c_str(), future_scene);
    }


    if (parent() == &app.player_island()) {

        using Next = WeaponSetTargetScene;

        auto next =
            scene_pool::make_deferred_scene<Next>(position(), true, target_);

        if (app.game_mode() == App::GameMode::co_op) {
            return co_op_acquire_lock(pfrm, next);
        } else {
            return next();
        }
    }
    return null_scene();
}



} // namespace skyland
