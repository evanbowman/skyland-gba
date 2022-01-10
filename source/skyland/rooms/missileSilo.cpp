#include "missileSilo.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/sound.hpp"



namespace skyland {



SHARED_VARIABLE(missile_silo_reload_ms);



static Sound missile_sound("missile");



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

                    if (auto room = island->get_room(*target_)) {
                        target = room->center();
                    } else {
                        auto origin = island->origin();
                        origin.x += target_->x * 16 + 8;
                        origin.y += target_->y * 16 + 8;
                        target = origin;
                    }

                    if (not pfrm.network_peer().is_connected() and
                        app.game_mode() not_eq App::GameMode::tutorial) {
                        target = rng::sample<10>(target, rng::critical_state);
                    }

                    auto start = center();
                    start.y -= 24;

                    app.camera().shake(6);
                    load_ += 1000 * missile_silo_reload_ms;
                    auto m = app.alloc_entity<Missile>(pfrm,
                                                       start,
                                                       target,
                                                       parent());

                    missile_sound.play(pfrm, 3, milliseconds(400));

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
        return scene_pool::alloc<WeaponSetTargetScene>(position(),
                                                       true,
                                                       target_);
    }

    return null_scene();
}


} // namespace skyland
