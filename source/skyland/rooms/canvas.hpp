////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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


#include "decoration.hpp"
#include "skyland/tileId.hpp"
#include "skyland/tile.hpp"
#include "skyland/systemString.hpp"
#include "skyland/img.hpp"



namespace skyland
{



class Canvas : public Decoration
{
public:

    Canvas(Island* parent, const RoomCoord& position);


    ~Canvas();


    static int default_health()
    {
        return 1;
    }


    static int default_cost()
    {
        return 1;
    }


    static int default_power()
    {
        return 0;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::only_constructible_in_sandbox | RoomProperties::fragile;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static SystemString ui_name()
    {
        return SystemString::block_generic;
    }


    static const char* name()
    {
        return "canvas";
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    ScenePtr select_impl(const RoomCoord& cursor) override;


    lisp::Value* serialize() override;


    void deserialize(lisp::Value* v) override;


    void bind_graphics(const img::Image& img);


    static Icon icon()
    {
        return 2664;
    }


    static Icon unsel_icon()
    {
        return 2680;
    }


    static void format_description(StringBuffer<512>& buffer);


    void publish_tiles();


private:
    TileId tile_;
    int canvas_texture_slot_ = -1;

    Optional<DynamicMemory<img::Image>> img_data_;
};



}
