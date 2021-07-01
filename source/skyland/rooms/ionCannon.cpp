#include "ionCannon.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



IonCannon::IonCannon(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(150))
{
}



void IonCannon::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::update(pfrm, app, delta);


    if (reload_ > 0) {
        reload_ -= delta;
    } else if (target_) {

        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island(app);

        if (island and not island->is_destroyed()) {
            if (auto room = island->get_room(*target_)) {
                app.camera().shake(4);

                auto start = center();

                // This just makes it a bit less likely for cannonballs to
                // run into the player's own buildings, especially around
                // corners.
                if (island == &app.player_island()) {
                    start.x -= 6;
                } else {
                    start.x += 6;
                }

                auto target = room->center();

                if (not pfrm.network_peer().is_connected()) {
                    target = rng::sample<6>(target, rng::critical_state);
                }

                auto c = alloc_entity<IonBurst>(start, target, parent());
                parent()->projectiles().push(std::move(c));

                reload_ = reload_time;
            } else {
                target_.reset();
            }
        }
    }
}



void IonCannon::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(
        layer, position().x, position().y, InteriorTile::particle_gun);
}



void IonCannon::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::particle_gun);
}



ScenePtr<Scene> IonCannon::select(Platform& pfrm, App& app)
{
    return scene_pool::alloc<WeaponSetTargetScene>(position());
}



} // namespace skyland
