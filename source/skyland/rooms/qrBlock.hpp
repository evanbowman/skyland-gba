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


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class QrBlock final : public Decoration
{
public:
    QrBlock(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position),
          data_(allocate_dynamic<StringBuffer<400>>("qr-data"))
    {
    }


    void update(Time delta) override;


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_qr)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::not_constructible;
    }


    static const char* name()
    {
        return "code";
    }


    static SystemString ui_name()
    {
        return SystemString::block_qr;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 3800;
    }


    static Icon unsel_icon()
    {
        return 3784;
    }


    bool non_owner_selectable() const override
    {
        return true;
    }


    static int default_power()
    {
        return 0;
    }

    static int default_cost()
    {
        return 1;
    }


    ScenePtr<Scene> select_impl(const RoomCoord& cursor) override;


    void set_message(const char* msg)
    {
        *data_ = msg;
    }


    bool opponent_display_on_hover() const override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


private:
    DynamicMemory<StringBuffer<400>> data_;
    Optional<Platform::DynamicTexturePtr> hint_img_;
    Time hint_img_release_timer_ = 0;
};



} // namespace skyland
