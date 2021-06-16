#include "missileSilo.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



MissileSilo::MissileSilo(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(200))
{
}



void MissileSilo::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void MissileSilo::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::missile_silo_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::missile_silo_2);
}



void MissileSilo::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::missile_silo_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::missile_silo_2);
}



} // namespace skyland
