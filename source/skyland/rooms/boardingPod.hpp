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


#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class BoardingPod final : public Room
{
public:
    BoardingPod(Island* parent, const RoomCoord& position);


    void update(Platform&, App&, Microseconds delta) override;
    void rewind(Platform&, App&, Microseconds delta) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);

    void apply_damage(Platform& pfrm,
                      App& app,
                      Health damage,
                      Island* source) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(App& app,
                             bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static Float atp_value()
    {
        return 800.f;
    }


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static const char* name()
    {
        return "boarding-pod";
    }


    void render_scaffolding(App& app, TileId buffer[16][16]) override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable | RoomProperties::roof_hidden |
               RoomProperties::disallow_chimney |
               RoomProperties::destroy_quietly |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::locked_by_default;
    }


    static SystemString ui_name()
    {
        return SystemString::block_boarding_pod;
    }


    static Icon icon()
    {
        return 3960;
    }


    static Icon unsel_icon()
    {
        return 3944;
    }


    bool description_visible() override
    {
        return true;
    }


    void set_target(Platform& pfrm,
                    App& app,
                    const RoomCoord& target,
                    bool pinned) override;


    void unset_target(Platform& pfrm, App& app) override;


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const RoomCoord& cursor) override
    {
    }


    std::optional<RoomCoord> get_target() const override
    {
        return target_;
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    Island* owner_ = nullptr;


    Island* owner() const
    {
        return owner_;
    }


private:
    std::optional<RoomCoord> target_;
    Microseconds launch_timer_ = 0;
    Microseconds heal_timer_ = 0;
};



} // namespace skyland
