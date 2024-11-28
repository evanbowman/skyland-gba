///////////////////////////////////////////////////////////////////////////////
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


#pragma once

#include "memory/tinyBuffer.hpp"
#include "skyland/bulkTimer.hpp"
#include "skyland/room.hpp"



namespace skyland
{



class Weapon : public Room, public Timer
{
public:
    Weapon(Island* parent,
           const char* name,
           const RoomCoord& position,
           Time reload_time);


    ~Weapon();


    void timer_expired() override;


    Time reload_interval() const override
    {
        return reload();
    }


    virtual Time reload_impl() const = 0;


    Time reload() const;


    virtual void fire() = 0;


    void override_reload_timer(Time new_time) override
    {
        Timer::__override_clock(new_time);
    }


    Time reload_time_remaining() const override
    {
        return Timer::remaining();
    }


    static Category category()
    {
        return Category::weapon;
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::none;
    }


    void update(Time delta) override;


    void rewind(Time delta) override;


    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;


    bool target_pinned() const override;


    void set_target(const RoomCoord& target, bool pinned) override;


    void set_target(const TargetQueue& q, bool pinned) override;


    TargetCount target_count() const override;


    void unset_target() override;


    void finalize() override final;


    virtual void finalize_weapon();


    Optional<RoomCoord> get_target() const override
    {
        if (not target_queue_.empty()) {
            return target_queue_.back().coord();
        }
        return nullopt();
    }


    void __rewind_push_target_queue(const RoomCoord& target);


    int debris_tile() override
    {
        return 1;
    }


    Weapon* cast_weapon() override;


    void display_on_hover(Platform::Screen& screen,
                          const RoomCoord& cursor) override;


    ScenePtr select_impl(const RoomCoord& cursor) override;


    bool allows_powerdown() override;


    const TargetQueue& target_queue() const;


    void update_targets();


    void clear_target_queue();


    void amplify(bool enabled) override;


protected:
    void on_powerchange() override;

    TargetQueue target_queue_;
    bool target_pinned_ : 1 = false;
    bool amplify_ : 1 = false;
};



} // namespace skyland
