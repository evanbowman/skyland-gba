////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



void SparkCannon::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_spark_cannon)->c_str();
}



SparkCannon::SparkCannon(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void SparkCannon::on_lightning()
{
    if (level_ < 2) {
        ++level_;
        schedule_repaint();
    } else {
        if (is_player_island(parent())) {
            time_stream::event::PlayerRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            APP.time_stream().push(APP.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            APP.time_stream().push(APP.level_timer(), e);
        }
    }
}



void SparkCannon::on_lightning_rewind()
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



ScenePtr SparkCannon::select_impl(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (level_ == 0) {
        PLATFORM.speaker().play_sound("beep_error", 3);
        return null_scene();
    }

    auto start = center();

    auto island = other_island();

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    bool right = true;
    if (is_player_island(island)) {
        right = false;
        start.x -= 24.0_fixed;
    } else {
        start.x += 24.0_fixed;
    }

    cannon_sound.play(3);

    switch (level_) {
    case 1: {
        auto ab = APP.alloc_entity<ArcBolt>(
            start, right ? 0 : 180, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = APP.alloc_entity<ArcBolt>(
            start, right ? 20 : 160, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = APP.alloc_entity<ArcBolt>(
            start, right ? 340 : 200, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }
        break;
    }

    default: {
        auto ab = APP.alloc_entity<ArcBolt>(
            start, right ? 0 : 180, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = APP.alloc_entity<ArcBolt>(
            start, right ? 10 : 190, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = APP.alloc_entity<ArcBolt>(
            start, right ? 350 : 170, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = APP.alloc_entity<ArcBolt>(
            start, right ? 20 : 160, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        ab = APP.alloc_entity<ArcBolt>(
            start, right ? 340 : 200, parent(), position());
        if (ab) {
            parent()->projectiles().push(std::move(ab));
        }

        start.y += 4.0_fixed;

        auto target = center();
        if (is_player_island(parent())) {
            target.x += 100.0_fixed;
        } else {
            target.x -= 100.0_fixed;
        }

        auto c = APP.alloc_entity<DecimatorBurst>(
            start, target, parent(), position());

        if (c) {
            parent()->projectiles().push(std::move(c));
        }

        break;
    }
    }

    auto record_lv = [&] {
        if (is_player_island(parent())) {
            time_stream::event::PlayerRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            APP.time_stream().push(APP.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            APP.time_stream().push(APP.level_timer(), e);
        }
    };

    while (level_) {
        record_lv();
        --level_;
    }

    schedule_repaint();

    return null_scene();
}



void SparkCannon::___rewind___finished_reload()
{
    ++level_;
    schedule_repaint();
}



} // namespace skyland
