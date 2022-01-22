#include "weapon.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "globals.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



Weapon::Weapon(Island* parent,
               const char* name,
               const Vec2<u8>& size,
               const Vec2<u8>& position,
               Microseconds reload_time) :
    Room(parent, name, size, position)
{
    reload_timer_ = reload_time;
}



void Weapon::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

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
                app.time_stream().push(pfrm, app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(pfrm, app.level_timer(), e);
            }
        }
    } else if (target_) {

        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island(app);

        if (island and not island->is_destroyed()) {
            fire(pfrm, app);

            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomAbilityUsed e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(pfrm, app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomAbilityUsed e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(pfrm, app.level_timer(), e);
            }

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



void Weapon::___rewind___finished_reload()
{
    // NOTE: the reload logic, above, increments the reload counter if the
    // reload timer is greater than zero. A bit hacky, but better than wasting
    // memory with a state variable for this relatively simple purpose.
    reload_timer_ = 1;
}



void Weapon::___rewind___ability_used()
{
    // NOTE: the weapon just fired, i.e. before it fired, its reload timer must
    // have been either zero or near zero. We could technically store the exact
    // value of the timer, but doing so would use more memory, and,
    // realistically, the timer value would only have been negative by some
    // small number of bytes; at most, one frame.
    reload_timer_ = 0;
}



void Weapon::set_target(Platform& pfrm,
                        App& app,
                        const Vec2<u8>& target,
                        bool sequenced)
{
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

    if (sequenced) {
        app.time_stream().push(pfrm, app.level_timer(), e);
    }

    target_ = target;
}



void Weapon::unset_target(Platform& pfrm,
                          App& app,
                          bool sequenced)
{
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

    if (sequenced) {
        app.time_stream().push(pfrm, app.level_timer(), e);
    }

    target_.reset();
}



ScenePtr<Scene> Weapon::select(Platform& pfrm, App& app)
{
    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (parent() == &app.player_island()) {
        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), true, target_);
    }
    return null_scene();
}




}
