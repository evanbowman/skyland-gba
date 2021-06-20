#include "cannon.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Cannon::Cannon(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(200))
{
}



void Cannon::update(Platform& pfrm, App& app, Microseconds delta)
{
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

                auto c = alloc_entity<Cannonball>(
                    start,
                    rng::sample<6>(room->center(), rng::utility_state),
                    parent());
                parent()->projectiles().push(std::move(c));

                reload_ = reload_time;
            } else {
                target_.reset();
            }
        }
    }
}



void Cannon::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::cannon_1);
}



void Cannon::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::cannon_1);
}



ScenePtr<Scene> Cannon::select(Platform& pfrm)
{
    return scene_pool::alloc<WeaponSetTargetScene>(position());
}



} // namespace skyland
