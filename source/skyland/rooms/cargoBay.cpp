#include "cargoBay.hpp"
#include "skyland/tile.hpp"
#include <string.h>



namespace skyland {



CargoBay::CargoBay(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
    set_cargo("");
}



void CargoBay::format_description(StringBuffer<512>& buffer)
{
    buffer += "Stores one unit of cargo. Required for trading and quests in "
              "adventure mode!";
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
    Room::update(pfrm, app, delta);
}



void CargoBay::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cargo_bay;
    buffer[position().x][position().y + 1] = InteriorTile::plain_floor;
}



void CargoBay::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



} // namespace skyland
