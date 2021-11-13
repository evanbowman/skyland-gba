#include "infestor.hpp"



namespace skyland {



Infestor::Infestor(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(full_health()))
{
}



void Infestor::update(Platform&, App&, Microseconds delta)
{
}



void Infestor::render_interior(Platform& pfrm, Layer layer)
{
}



void Infestor::render_exterior(Platform& pfrm, Layer layer)
{
}



} // namespace skyland
