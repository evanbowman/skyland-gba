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


#include "sparkCannon.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/arcBolt.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



extern Sound cannon_sound;



void SparkCannon::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_spark_cannon)->c_str();
}



SparkCannon::SparkCannon(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void SparkCannon::on_lightning(Platform& pfrm, App& app)
{
    if (level_ < 2) {
        ++level_;
        schedule_repaint();
    } else {
        if (parent() == &app.player_island()) {
            time_stream::event::PlayerRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            app.time_stream().push(app.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            app.time_stream().push(app.level_timer(), e);
        }
    }
}



void SparkCannon::on_lightning_rewind(Platform& pfrm, App& app)
{
    if (level_ > 0) {
        --level_;
        schedule_repaint();
    }
}



void SparkCannon::render_interior(App* app, TileId buffer[16][16])
{
    int x1 = 0;
    int x2 = 1;

    bool right = false;
    if (app) {
        right = parent() == &app->player_island();
    }

    if (not right) {
        std::swap(x1, x2);
    }
    switch (level_) {
    case 0:
        buffer[position().x + x1][position().y] = InteriorTile::spark_cannon_l1;
        break;
    case 1:
        buffer[position().x + x1][position().y] = InteriorTile::spark_cannon_l2;
        break;
    default:
        buffer[position().x + x1][position().y] = InteriorTile::spark_cannon_l3;
        break;
    }

    buffer[position().x + x2][position().y] = InteriorTile::spark_cannon_front;
}



void SparkCannon::render_exterior(App* app, TileId buffer[16][16])
{
    int x1 = 0;
    int x2 = 1;

    bool right = false;
    if (app) {
        right = parent() == &app->player_island();
    }

    if (not right) {
        std::swap(x1, x2);
    }
    switch (level_) {
    case 0:
        buffer[position().x + x1][position().y] = Tile::spark_cannon_l1;
        break;
    case 1:
        buffer[position().x + x1][position().y] = Tile::spark_cannon_l2;
        break;
    default:
        buffer[position().x + x1][position().y] = Tile::spark_cannon_l3;
        break;
    }

    buffer[position().x + x2][position().y] = Tile::spark_cannon_front;
}



ScenePtr<Scene>
SparkCannon::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (level_ == 0) {
        pfrm.speaker().play_sound("beep_error", 3);
        return null_scene();
    }

    auto start = center();

    auto island = other_island(app);

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    bool right = true;
    if (island == &app.player_island()) {
        right = false;
        start.x -= 24.0_fixed;
    } else {
        start.x += 24.0_fixed;
    }

    cannon_sound.play(pfrm, 3);

    switch (level_) {
    case 1: {
        auto ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 0 : 180, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 20 : 160, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 340 : 200, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }
        break;
    }

    default: {
        auto ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 0 : 180, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 10 : 190, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 350 : 170, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 20 : 160, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = app.alloc_entity<ArcBolt>(
            pfrm, start, right ? 340 : 200, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        start.y += 4.0_fixed;

        auto target = center();
        if (parent() == &app.player_island()) {
            target.x += 100.0_fixed;
        } else {
            target.x -= 100.0_fixed;
        }

        auto c = app.alloc_entity<DecimatorBurst>(
            pfrm, start, target, parent(), position());

        if (c) {
            parent()->projectiles().push(std::move(c));
        }

        break;
    }
    }

    auto record_lv = [&] {
        if (parent() == &app.player_island()) {
            time_stream::event::PlayerRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            app.time_stream().push(app.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            app.time_stream().push(app.level_timer(), e);
        }
    };

    while (level_) {
        record_lv();
        --level_;
    }

    schedule_repaint();

    return null_scene();
}



void SparkCannon::___rewind___finished_reload(Platform&, App&)
{
    ++level_;
    schedule_repaint();
}



} // namespace skyland
