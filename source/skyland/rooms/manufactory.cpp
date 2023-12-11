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


#include "manufactory.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Manufactory::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_manufactory)->c_str();
}



Manufactory::Manufactory(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Manufactory::update(Time delta)
{
    Room::update(delta);
}



void Manufactory::render_interior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::manufactory_1;
    buffer[x + 1][y] = InteriorTile::manufactory_2;
    buffer[x + 2][y] = InteriorTile::manufactory_3;

    buffer[x][y + 1] = InteriorTile::manufactory_4;
    buffer[x + 1][y + 1] = InteriorTile::manufactory_5;
    buffer[x + 2][y + 1] = InteriorTile::plain_floor;
}



void Manufactory::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_2;
    buffer[x + 1][y] = Tile::wall_plain_1;
    buffer[x + 1][y + 1] = Tile::wall_plain_2;
    buffer[x + 2][y] = Tile::wall_window_1;
    buffer[x + 2][y + 1] = Tile::wall_window_2;
}



void Manufactory::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



} // namespace skyland
