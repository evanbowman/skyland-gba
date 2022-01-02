#include "droneBay.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/constructDroneScene.hpp"
#include "skyland/scene/placeDroneScene.hpp"
#include "skyland/tile.hpp"
#include "skyland/entity/explosion/explosion.hpp"



namespace skyland {



DroneBay::DroneBay(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void DroneBay::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (drone_ and not(*drone_)->alive()) {
        detach_drone(pfrm, app);
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        if (drone_) {
            (*drone_)->kill();
        }
    }

    if (reload_ > 0) {
        reload_ -= delta;
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
    if (reload_ > 0) {
        return null_scene();
    }

    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (not drone_) {
        auto pos = position();
        bool free[2] = {true, true};
        pos.y -= 1;
        if (auto room = parent()->get_room(pos)) {
            if (room->metaclass() not_eq forcefield_mt) {
                free[0] = false;
            }
        }
        pos.x += 1;
        if (auto room = parent()->get_room(pos)) {
            if (room->metaclass() not_eq forcefield_mt) {
                free[1] = false;
            }
        }
        if (not free[0] or not free[1]) {
            // TODO: push a message indicating that the drone bay is covered and
            // cannot launch anything.
            return null_scene();
        }
        return scene_pool::alloc<ConstructDroneScene>(position());
    }
    return null_scene();
}



void DroneBay::attach_drone(Platform& pfrm,
                            App& app,
                            SharedEntityRef<Drone> drone)
{
    if (drone_) {
        detach_drone(pfrm, app);
    }
    drone_ = drone;
}



void DroneBay::detach_drone(Platform& pfrm, App& app, bool quiet)
{
    if (drone_ and not quiet) {
        medium_explosion(pfrm, app, (*drone_)->sprite().get_position());
    }

    drone_.reset();
}



} // namespace skyland
