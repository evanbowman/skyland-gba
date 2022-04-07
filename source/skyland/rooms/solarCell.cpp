#include "solarCell.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland
{



Power SolarCell::power_usage() const
{
    const auto base_power = (*metaclass())->consumes_power();
    auto power = base_power;

    if (parent()->get_room({position().x, u8(position().y - 1)})) {
        power -= base_power / 4;
    }

    if (parent()->get_room({u8(position().x + 1), u8(position().y - 1)})) {
        power -= base_power / 4;
    }

    return power;
}



void SolarCell::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_solar_cell)->c_str();
}



void SolarCell::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::solar_cell;
    buffer[position().x + 1][position().y] = InteriorTile::solar_cell;
}



void SolarCell::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::solar_cell;
    buffer[position().x + 1][position().y] = Tile::solar_cell;
}



} // namespace skyland
