////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "tnt.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/entity/projectile/fireBolt.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Explosive::Explosive(Island* parent,
                     const RoomCoord& position,
                     const char* class_name)
    : Room(parent, class_name, position)
{
}



void Explosive::update(Time delta)
{
    Room::update(delta);

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(5);
            damage_timer_ = 0;
        }
    }
}



ScenePtr<Scene> Explosive::select(const RoomCoord& cursor)
{
    if (parent() not_eq &APP.player_island()) {
        return null_scene();
    }

    Room::apply_damage(1);

    network::packet::DynamiteActivated packet;
    packet.x_ = position().x;
    packet.y_ = position().y;
    network::transmit(packet);

    ignition_ = true;

    return null_scene();
}



void Explosive::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::dynamite;
}



void Explosive::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::dynamite;
}



static SharedVariable tnt_damage("dynamite_damage", 50);
static SharedVariable tnt_range("dynamite_range", 1);



void Explosive::apply_damage(Health damage)
{
    Room::apply_damage(damage);

    ignition_ = true;
}



void Explosive::ignite(int range, Health damage, bool spread_fire)
{
    auto flak_smoke = [](const Vec2<Fixnum>& pos) {
        auto e = APP.alloc_entity<SmokePuff>(
            rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            APP.effects().push(std::move(e));
        }
    };

    flak_smoke(center());
    flak_smoke(center());

    Vec2<s32> pos;
    pos.x = center().x.as_integer();
    pos.y = center().y.as_integer();

    APP.on_timeout(milliseconds(190), [pos, flak_smoke]() {
        Vec2<Fixnum> p;
        p.x = Fixnum::from_integer(pos.x);
        p.y = Fixnum::from_integer(pos.y);
        flak_smoke(p);
    });

    auto targets =
        allocate_dynamic<Buffer<Room*, 300>>("dynamite-target-bufer");

    for (int x = -range; x < range + 1; ++x) {
        for (int y = -range; y < range + 1; ++y) {
            if (position().x + x < 0 or position().y + y < 0 or
                position().x + x > 15 or position().y + y > 15) {
                continue;
            }

            auto pos = RoomCoord{u8(position().x + x), u8(position().y + y)};

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
                (*drone)->apply_damage(damage);
            }
        }
    }

    for (auto& room : *targets) {

        room->apply_damage(damage);


        if (spread_fire and not((*room->metaclass())->properties() &
                                RoomProperties::fireproof)) {
            if (room->health() > 0) {
                room->parent()->fire_create(room->position());
            }
        }


        if (APP.game_mode() == App::GameMode::adventure or
            APP.game_mode() == App::GameMode::challenge or
            APP.game_mode() == App::GameMode::skyland_forever) {

            // Hack added for an achievement where you unlock dynamite-ii when
            // destroying invading goblins with dynamite.
            if (is_player_island(room->parent()) and room->health() == 0) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &APP.opponent()) {
                        achievements::raise(achievements::Achievement::triage);
                        break;
                    }
                }
            }
        }
    }
}



void Explosive::finalize()
{
    Room::finalize();

    if (not ignition_) {
        return;
    } else {
        ignite(tnt_range, tnt_damage, false);
        ExploSpawner::create(center());
    }
}



void TNT::finalize()
{
    Room::finalize();

    if (not ignition_) {
        return;
    } else {
        ignite(2, 180, true);
        ExploSpawner::create(center());

        if (not PLATFORM.network_peer().is_connected()) {
            for (int i = 0; i < 10; ++i) {

                auto c = APP.alloc_entity<FireBolt>(
                    center(), center(), parent(), position());
                if (c) {
                    c->set_direction(rng::choice<359>(rng::critical_state));
                    parent()->projectiles().push(std::move(c));
                }
            }
        }
    }
}



static void display_damage_range_dist1(Platform::Screen& screen, Room& room)
{
    auto pos = room.position();

    auto origin = room.parent()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 1; x < pos.x + 2; ++x) {

        if (x == pos.x) {

            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);
            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y - 1) * 16)});
        } else {
            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y - 1) * 16)});
        }

        screen.draw(sprite);

        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(13, 1);
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});
        screen.draw(sprite);
        sprite.set_texture_index(13);
    }
}



static void display_damage_range_dist2(Platform::Screen& screen, Room& room)
{
    auto pos = room.position();

    auto origin = room.parent()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y - 2) * 16)});
        screen.draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);

            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y) * 16)});

            screen.draw(sprite);

            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
        }


        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});

        screen.draw(sprite);
    }
}



void Explosive::display_on_hover(Platform::Screen& screen,

                                 const RoomCoord& cursor)
{
    display_damage_range_dist1(screen, *this);
}



void TNT::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::tnt;
}



void TNT::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::tnt;
}



void TNT::display_on_hover(Platform::Screen& screen,

                           const RoomCoord& cursor)
{
    display_damage_range_dist2(screen, *this);
}



void Cesium::update(Time delta)
{
    Room::update(delta);
    Room::ready();

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(10);
            damage_timer_ = 0;
        }

    } else {

        u8 x = position().x;
        u8 y = position().y;

        if (auto room = parent()->get_room({x, u8(y - 1)})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1);
                return;
            }
        }

        if (auto room = parent()->get_room({x, u8(y + 1)})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1);
                return;
            }
        }

        if (auto room = parent()->get_room({u8(x + 1), y})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1);
                return;
            }
        }

        if (auto room = parent()->get_room({u8(x - 1), y})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(1);
                return;
            }
        }
    }
}



void Cesium::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cesium;
}



void Cesium::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cesium;
}



void Cesium::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_cesium)->c_str();
}



void Cesium::display_on_hover(Platform::Screen& screen,

                              const RoomCoord& cursor)
{
    display_damage_range_dist2(screen, *this);
}



} // namespace skyland
