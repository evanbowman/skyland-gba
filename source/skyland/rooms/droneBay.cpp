#include "droneBay.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/constructDroneScene.hpp"
#include "skyland/scene/placeDroneScene.hpp"
#include "skyland/tile.hpp"



namespace skyland {



DroneBay::DroneBay(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void DroneBay::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

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



void DroneBay::render_interior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::drone_bay_1;
    buffer[position().x + 1][position().y] = Tile::drone_bay_2;
}



void DroneBay::render_exterior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::drone_bay_1;
    buffer[position().x + 1][position().y] = InteriorTile::drone_bay_2;
}



ScenePtr<Scene> DroneBay::select(Platform& pfrm, App& app)
{
    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (not drone_) {
        return scene_pool::alloc<ConstructDroneScene>(position());
    }
    return null_scene();
}



} // namespace skyland
