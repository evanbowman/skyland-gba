#include "tnt.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



Explosive::Explosive(Island* parent,
                     const Vec2<u8>& position,
                     const char* class_name)
    : Room(parent, class_name, position)
{
}



void Explosive::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(pfrm, app, 5);
            damage_timer_ = 0;
        }
    }
}



ScenePtr<Scene> Explosive::select(Platform& pfrm, App& app)
{
    if (parent() not_eq &app.player_island()) {
        return null_scene();
    }

    Room::apply_damage(pfrm, app, 1);

    network::packet::DynamiteActivated packet;
    packet.x_ = position().x;
    packet.y_ = position().y;
    network::transmit(pfrm, packet);

    ignition_ = true;

    return null_scene();
}



void Explosive::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::dynamite;
}



void Explosive::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::dynamite;
}



static SharedVariable tnt_damage("dynamite_damage", 30);
static SharedVariable tnt_range("dynamite_range", 1);



void Explosive::apply_damage(Platform& pfrm, App& app, Health damage)
{
    Room::apply_damage(pfrm, app, damage);

    ignition_ = true;
}



void Explosive::ignite(Platform& pfrm, App& app, int range, Health damage)
{
    auto flak_smoke = [](Platform& pfrm, App& app, const Vec2<Float>& pos) {
        auto e = app.alloc_entity<SmokePuff>(
            pfrm, rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            app.effects().push(std::move(e));
        }
    };

    flak_smoke(pfrm, app, center());
    flak_smoke(pfrm, app, center());

    app.on_timeout(pfrm,
                   milliseconds(190),
                   [pos = center(), flak_smoke](Platform& pf, App& app) {
                       flak_smoke(pf, app, pos);
                   });

    auto targets = allocate_dynamic<Buffer<Room*, 300>>(pfrm);

    for (int x = -range; x < range + 1; ++x) {
        for (int y = -range; y < range + 1; ++y) {
            if (position().x + x < 0 or position().y + y < 0 or
                position().x + x > 15 or position().y + y > 15) {
                continue;
            }

            auto pos = Vec2<u8>{u8(position().x + x), u8(position().y + y)};

            if (auto room = parent()->get_room(pos)) {
                bool found = false;
                for (auto& target : *targets) {
                    if (target == room) {
                        found = true;
                        break;
                    }
                }
                if (not found) {
                    targets->push_back(room);
                }
            } else if (auto drone = parent()->get_drone(pos)) {
                (*drone)->apply_damage(pfrm, app, damage);
            }
        }
    }

    if (app.game_mode() == App::GameMode::adventure or
        app.game_mode() == App::GameMode::skyland_forever) {

        for (auto& room : *targets) {
            room->apply_damage(pfrm, app, damage);

            // Hack added for an achievement where you unlock dynamite-ii when
            // destroying invading goblins with dynamite.
            if (room->parent() == &app.player_island() and room->health() == 0) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &app.opponent()) {
                        set_enabled(::skyland::metaclass_index("dynamite-ii"),
                                    true);
                        break;
                    }
                }
            }
        }
    }
}



void Explosive::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (not ignition_) {
        return;
    } else {
        ignite(pfrm, app, tnt_range, tnt_damage);
    }
}



void TNT::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::tnt;
}



void TNT::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::tnt;
}



} // namespace skyland
