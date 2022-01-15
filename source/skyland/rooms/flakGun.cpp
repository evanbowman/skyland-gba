#include "flakGun.hpp"
#include "globals.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland {



SHARED_VARIABLE(flak_gun_reload_ms);



extern Sound cannon_sound;



void FlakGun::format_description(StringBuffer<512>& buffer)
{
    buffer += "Deals low damage, but spread over large areas. "
        "Highly destructive when used in groups against castles "
        "with thin hull. Requires a workshop.";
}



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

            Vec2<Float> target;

            auto origin = island->origin();
            origin.x += target_->x * 16 + 8;
            origin.y += target_->y * 16 + 8;
            target = origin;


            app.camera().shake(4);

            auto start = center();

            // This just makes it a bit less likely for cannonballs to
            // run into the player's own buildings, especially around
            // corners.
            if (island == &app.player_island()) {
                start.x -= 22;
            } else {
                start.x += 22;
            }

            if (not pfrm.network_peer().is_connected() and
                app.game_mode() not_eq App::GameMode::tutorial) {
                target = rng::sample<6>(target, rng::critical_state);
            }

            cannon_sound.play(pfrm, 3);

            auto c = alloc_entity<Flak>(start, target, parent(), position());
            if (c) {
                parent()->projectiles().push(std::move(c));
            }


            reload_ += 1000 * flak_gun_reload_ms;
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

    if (parent() == &app.player_island()) {
        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), true, target_);
    }
    return null_scene();
}



void FlakGun::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::flak_gun_1;
    buffer[position().x + 1][position().y] = InteriorTile::flak_gun_2;
}



void FlakGun::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::flak_gun_1;
    buffer[position().x + 1][position().y] = Tile::flak_gun_2;
}



} // namespace skyland
