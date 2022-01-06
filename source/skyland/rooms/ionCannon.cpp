#include "ionCannon.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/ionBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



SHARED_VARIABLE(ion_cannon_reload_ms);



IonCannon::IonCannon(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void IonCannon::update(Platform& pfrm, App& app, Microseconds delta)
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

                auto target = room->center();

                if (not pfrm.network_peer().is_connected() and
                    not app.tutorial_mode()) {
                    target = rng::sample<6>(target, rng::critical_state);
                }

                auto c =
                    app.alloc_entity<IonBurst>(pfrm, start, target, parent());
                if (c) {
                    parent()->projectiles().push(std::move(c));
                }

                reload_ = 1000 * ion_cannon_reload_ms;
            } else {
                target_.reset();
            }
        }
    }
}



void IonCannon::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::particle_gun;
}



void IonCannon::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::particle_gun;
}



ScenePtr<Scene> IonCannon::select(Platform& pfrm, App& app)
{
    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    return scene_pool::alloc<WeaponSetTargetScene>(position());
}



} // namespace skyland
