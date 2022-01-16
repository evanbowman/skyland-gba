#include "cargoBay.hpp"
#include <string.h>
#include "skyland/tile.hpp"



namespace skyland {



CargoBay::CargoBay(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
    set_cargo("");
}



void CargoBay::format_description(StringBuffer<512>& buffer)
{
    buffer += "Stores one unit of cargo. Building a cargo bay enables quests "
        "in adventure mode!";
}



bool CargoBay::set_cargo(const char* cargo)
{
    if (str_len(cargo) + 1 > sizeof cargo_) {
        return false;
    }

    auto dest = cargo_;
    auto src = cargo;

    while (*src not_eq '\0') {
        *dest++ = *src++;
    }
    *dest = '\0';

    return true;
}



void CargoBay::update(Platform& pfrm, App& app, Microseconds delta)
{
    // ...
}



void CargoBay::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::workshop_1;
    buffer[position().x][position().y + 1] = InteriorTile::workshop_2;
    buffer[position().x + 1][position().y] = InteriorTile::workshop_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::workshop_4;
}



void CargoBay::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



}
