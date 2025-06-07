////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "chaosCore.hpp"
#include "containers/vector.hpp"
#include "number/random.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/explosion/exploTrail.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(chaos_core_yield_rate);



Power ChaosCore::power_usage() const
{
    if (health() == 0) {
        return 0;
    }

    auto mt = room_metatable();
    Vector<bool> seen_rooms;
    for (int i = 0; i < mt.second; ++i) {
        seen_rooms.push_back(false);
    }

    for (auto& room : parent()->rooms()) {
        seen_rooms[room->metaclass_index()] = true;
    }

    int sum = 0;
    for (bool seen : seen_rooms) {
        if (seen) {
            sum += chaos_core_yield_rate;
        }
    }

    return -1 * sum;
}



void ChaosCore::format_description(StringBuffer<512>& buffer)
{
    make_format(
        buffer, SYSTR(description_chaos_core)->c_str(), chaos_core_yield_rate);
}



void ChaosCore::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::chaos_core_1;
    buffer[position().x + 1][position().y] = InteriorTile::chaos_core_2;

    buffer[position().x][position().y + 1] = InteriorTile::chaos_core_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::chaos_core_4;

    buffer[position().x][position().y + 2] = InteriorTile::core_2;
    buffer[position().x + 1][position().y + 2] = InteriorTile::plain_floor;
}



void ChaosCore::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_plain_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_plain_2;
}



extern Sound core_destroyed;



void ChaosCore::finalize()
{
    Room::finalize();

    if (health() <= 0) {

        core_destroyed.play(4, milliseconds(600));
        core_explosion(parent(),
                       center(),
                       CoreExplosionConfig{
                           .arms_ = 3,
                           .rot_ = rng::choice<45>(rng::utility_state),
                       });

        auto pos = center();
        pos.y += 16.0_fixed;
        ExploSpawner::create(pos);
        pos.y -= 32.0_fixed;
        ExploSpawner::create(pos);

        for (int i = 0; i < 2; ++i) {
            if (auto e = alloc_entity<ExploTrail>(
                    center(),
                    rng::choice<360>(rng::utility_state),
                    1.25_fixed,
                    seconds(2))) {
                APP.effects().push(std::move(e));
            }
        }

        APP.on_timeout(milliseconds(500), [p = center()] {
            auto p2 = rng::sample<16>(p, rng::utility_state);
            APP.alloc_entity<ExpFlash>(p2);
        });

        APP.on_timeout(milliseconds(750), [p = center()] {
            auto p2 = rng::sample<32>(p, rng::utility_state);
            APP.alloc_entity<ExpFlash>(p2);
        });

        APP.on_timeout(milliseconds(900), [p = center()] {
            auto p2 = rng::sample<32>(p, rng::utility_state);
            APP.alloc_entity<ExpFlash>(p2);
        });

        const int xstart = clamp(position().x - 2, 0, 15);
        const int ystart = clamp(position().y - 2, 0, 15);
        const int xend = clamp(position().x + size().x + 2, 0, 15);
        const int yend = clamp(position().y + size().y + 2, 0, 15);

        for (u8 x = xstart; x < xend; ++x) {
            for (u8 y = ystart; y < yend; ++y) {
                if (auto room = parent()->get_room({x, y})) {
                    room->apply_damage(10);
                    parent()->fire_create({x, y});
                }
            }
        }
    }
}



} // namespace skyland
