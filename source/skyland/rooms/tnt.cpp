////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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



void Explosive::update(App& app, Microseconds delta)
{
    Room::update(app, delta);

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(app, 5);
            damage_timer_ = 0;
        }
    }
}



ScenePtr<Scene> Explosive::select(App& app, const RoomCoord& cursor)
{
    if (parent() not_eq &app.player_island()) {
        return null_scene();
    }

    Room::apply_damage(app, 1);

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



void Explosive::apply_damage(App& app, Health damage)
{
    Room::apply_damage(app, damage);

    ignition_ = true;
}



void Explosive::ignite(App& app, int range, Health damage, bool spread_fire)
{
    auto flak_smoke = [](App& app, const Vec2<Fixnum>& pos) {
        auto e = app.alloc_entity<SmokePuff>(
            rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            app.effects().push(std::move(e));
        }
    };

    flak_smoke(app, center());
    flak_smoke(app, center());

    Vec2<s32> pos;
    pos.x = center().x.as_integer();
    pos.y = center().y.as_integer();

    app.on_timeout(milliseconds(190), [pos, flak_smoke](App& app) {
        Vec2<Fixnum> p;
        p.x = Fixnum::from_integer(pos.x);
        p.y = Fixnum::from_integer(pos.y);
        flak_smoke(app, p);
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
                (*drone)->apply_damage(app, damage);
            }
        }
    }

    for (auto& room : *targets) {

        room->apply_damage(app, damage);


        if (spread_fire and not((*room->metaclass())->properties() &
                                RoomProperties::fireproof)) {
            if (room->health() > 0) {
                room->parent()->fire_create(app, room->position());
            }
        }


        if (app.game_mode() == App::GameMode::adventure or
            app.game_mode() == App::GameMode::challenge or
            app.game_mode() == App::GameMode::skyland_forever) {

            // Hack added for an achievement where you unlock dynamite-ii when
            // destroying invading goblins with dynamite.
            if (room->parent() == &app.player_island() and
                room->health() == 0) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &app.opponent()) {
                        achievements::raise(app,
                                            achievements::Achievement::triage);
                        break;
                    }
                }
            }
        }
    }
}



void Explosive::finalize(App& app)
{
    Room::finalize(app);

    if (not ignition_) {
        return;
    } else {
        ignite(app, tnt_range, tnt_damage, false);
        ExploSpawner::create(app, center());
    }
}



void TNT::finalize(App& app)
{
    Room::finalize(app);

    if (not ignition_) {
        return;
    } else {
        ignite(app, 2, 180, true);
        ExploSpawner::create(app, center());

        if (not PLATFORM.network_peer().is_connected()) {
            for (int i = 0; i < 10; ++i) {

                auto c = app.alloc_entity<FireBolt>(
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
                                 App& app,
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
                           App& app,
                           const RoomCoord& cursor)
{
    display_damage_range_dist2(screen, *this);
}



void Cesium::update(App& app, Microseconds delta)
{
    Room::update(app, delta);
    Room::ready();

    if (health() not_eq max_health()) {
        Room::ready();
        ignition_ = true;

        damage_timer_ += delta;
        if (damage_timer_ > milliseconds(200)) {
            apply_damage(app, 10);
            damage_timer_ = 0;
        }

    } else {

        u8 x = position().x;
        u8 y = position().y;

        if (auto room = parent()->get_room({x, u8(y - 1)})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(app, 1);
                return;
            }
        }

        if (auto room = parent()->get_room({x, u8(y + 1)})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(app, 1);
                return;
            }
        }

        if (auto room = parent()->get_room({u8(x + 1), y})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(app, 1);
                return;
            }
        }

        if (auto room = parent()->get_room({u8(x - 1), y})) {
            if ((*room->metaclass())->properties() & RoomProperties::fluid) {
                ignition_ = true;
                apply_damage(app, 1);
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
                              App& app,
                              const RoomCoord& cursor)
{
    display_damage_range_dist2(screen, *this);
}



} // namespace skyland
