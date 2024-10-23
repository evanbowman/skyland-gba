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


#include "ionFizzler.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"
#include "skyland/island.hpp"



namespace skyland
{



void IonFizzler::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_ion_fizzler)->c_str();
}



IonFizzler::IonFizzler(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void IonFizzler::update(Time delta)
{
    Room::update(delta);
}



void IonFizzler::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ion_fizzler;
    buffer[position().x][position().y + 1] = InteriorTile::ion_fizzler_interior;
}



void IonFizzler::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::ion_fizzler;
    buffer[position().x][position().y + 1] = Tile::ion_fizzler_exterior;

    // auto clamp = [](auto v, auto f, auto c) { return v; };

    int left = clamp(position().x - 2, 0, 15);
    int right = clamp(position().x + 2, 0, (int)parent()->terrain().size());
    int top = clamp(position().y - 2, 0, 15);
    int bot = clamp(position().y + 2, 0, 15);

    auto set_if_empty = [&](int x, int y, TileId t) {
        if (not buffer[x][y]) {
            buffer[x][y] = t;
        }
    };
    set_if_empty(left, top, Tile::fizzle_boundary_tl);
    set_if_empty(right, top, Tile::fizzle_boundary_tr);
    set_if_empty(left, bot, Tile::fizzle_boundary_bl);
    set_if_empty(right, bot, Tile::fizzle_boundary_br);

    for (int y = top + 1; y < bot; ++y) {
        set_if_empty(left, y, Tile::fizzle_boundary_l);
        set_if_empty(right, y, Tile::fizzle_boundary_r);
    }

    for (int x = right + 1; x < left; ++x) {
        set_if_empty(x, top, Tile::fizzle_boundary_t);
        set_if_empty(x, bot, Tile::fizzle_boundary_b);
    }
}



} // namespace skyland
