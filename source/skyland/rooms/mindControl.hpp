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



class MindControl final : public Room
{
public:
    MindControl(Island* parent,
                const RoomCoord& position,
                const char* n = name());


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden |
               RoomProperties::manufactory_required |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::locked_by_default;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 900.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "mind-control";
    }


    static SystemString ui_name()
    {
        return SystemString::block_mind_control;
    }


    static Icon icon()
    {
        return 4040;
    }


    static Icon unsel_icon()
    {
        return 4024;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    void bind_character(CharacterId id)
    {
        controlled_character_ = id;
    }


    CharacterId bound_character() const
    {
        return controlled_character_;
    }


    void finalize() override;


    void unset_target() override
    {
        controlled_character_ = 0;
    }


private:
    Time next_action_timer_ = seconds(1);

    CharacterId controlled_character_ = 0;
};



} // namespace skyland
