#include "missileSilo.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



SHARED_VARIABLE(missile_silo_reload_ms);



MissileSilo::MissileSilo(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void MissileSilo::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (load_ > 0) {
        load_ -= delta;
    } else {
        if (target_) {
            auto island = other_island(app);

            if (parent()->power_supply() < parent()->power_drain()) {
                return;
            }

            if (island) {

                if (target_) {

                    Vec2<Float> target;

                    auto origin = island->origin();
                    origin.x += target_->x * 16 + 8;
                    origin.y += target_->y * 16 + 8;
                    target = origin;

                    auto start = center();
                    start.y -= 24;

                    app.camera().shake(6);
                    load_ = 1000 * missile_silo_reload_ms;
                    auto m = app.alloc_entity<Missile>(pfrm, start, target, parent());

                    if (m) {
                        parent()->projectiles().push(std::move(m));
                    }
                }
            }
        }
    }
}



void MissileSilo::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::missile_silo_1;
    buffer[position().x][position().y + 1] = Tile::missile_silo_2;
}



void MissileSilo::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::missile_silo_1;
    buffer[position().x][position().y + 1] = Tile::missile_silo_2;
}


ScenePtr<Scene> MissileSilo::select(Platform& pfrm, App& app)
{
    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (parent() == &app.player_island()) {
        return scene_pool::alloc<WeaponSetTargetScene>(position());
    }

    return null_scene();
}


} // namespace skyland
