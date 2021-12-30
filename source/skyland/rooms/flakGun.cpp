#include "flakGun.hpp"
#include "globals.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



FlakGun::FlakGun(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void FlakGun::update(Platform& pfrm, App& app, Microseconds delta)
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
                    start.x -= 18;
                } else {
                    start.x += 18;
                }

                auto target = room->center();

                if (not pfrm.network_peer().is_connected() and
                    not app.tutorial_mode()) {
                    target = rng::sample<6>(target, rng::critical_state);
                }

                auto c =
                    alloc_entity<Flak>(start, target, parent(), position());
                if (c) {
                    parent()->projectiles().push(std::move(c));
                }


                reload_ = reload_time;
            } else {
                target_.reset();
            }
        }
    }
}



ScenePtr<Scene> FlakGun::select(Platform& pfrm, App& app)
{
    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    return scene_pool::alloc<WeaponSetTargetScene>(position());
}



void FlakGun::render_exterior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::flak_gun_1;
    buffer[position().x + 1][position().y] = InteriorTile::flak_gun_2;
}



void FlakGun::render_interior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::flak_gun_1;
    buffer[position().x + 1][position().y] = Tile::flak_gun_2;
}



} // namespace skyland
