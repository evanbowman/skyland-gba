////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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


    void update(Time delta) override;
    void rewind(Time delta) override;


    static void format_description(StringBuffer<512>& buffer);

    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override;


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static const char* name()
    {
        return "boarding-pod";
    }


    void render_scaffolding(TileId buffer[16][16]) override;


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


    void set_target(const RoomCoord& target, bool pinned) override;


    void unset_target() override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override
    {
    }


    Optional<RoomCoord> get_target() const override
    {
        return target_;
    }


    ScenePtr select(const RoomCoord& cursor) override;


    Island* owner_ = nullptr;


    Island* owner() const
    {
        return owner_;
    }


private:
    Optional<RoomCoord> target_;
    Time launch_timer_ = 0;
    Time heal_timer_ = 0;
};



} // namespace skyland
