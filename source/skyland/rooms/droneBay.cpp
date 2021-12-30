#include "droneBay.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/moveDroneScene.hpp"
#include "skyland/tile.hpp"



namespace skyland {



DroneBay::DroneBay(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void DroneBay::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (drone_ and not(*drone_)->alive()) {
        drone_.reset();
    }
}



void DroneBay::display(Platform::Screen& screen)
{
    if (drone_) {
        screen.draw((*drone_)->sprite());
    }
}



void DroneBay::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::drone_bay_1);
    pfrm.set_tile(layer, position().x + 1, position().y, Tile::drone_bay_2);
}



void DroneBay::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::drone_bay_1);
    pfrm.set_tile(
        layer, position().x + 1, position().y, InteriorTile::drone_bay_2);
}



ScenePtr<Scene> DroneBay::select(Platform& pfrm, App& app)
{
    if (not drone_) {
        return scene_pool::alloc<MoveDroneScene>(pfrm, position());
    }
    return null_scene();
}



} // namespace skyland
