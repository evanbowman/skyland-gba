#include "infestor.hpp"



namespace skyland {



Infestor::Infestor(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Infestor::update(Platform&, App&, Microseconds delta)
{
}



void Infestor::render_interior(App& app, u8 buffer[16][16])
{
}



void Infestor::render_exterior(App& app, u8 buffer[16][16])
{
}



} // namespace skyland
