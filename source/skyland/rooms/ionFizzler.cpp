#include "ionFizzler.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void IonFizzler::format_description(StringBuffer<512>& buffer)
{
    buffer += "Absorbs all ion damage on contact. Useful for protecting "
        "vulnerable structures, such as reactors or forcefields,"
        " from ion cannons. Requires a foundry.";
}



IonFizzler::IonFizzler(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void IonFizzler::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void IonFizzler::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ion_fizzler;
    buffer[position().x][position().y + 1] = InteriorTile::ion_fizzler_interior;
}



void IonFizzler::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::ion_fizzler;
    buffer[position().x][position().y + 1] = Tile::ion_fizzler_exterior;
}



} // namespace skyland
