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
