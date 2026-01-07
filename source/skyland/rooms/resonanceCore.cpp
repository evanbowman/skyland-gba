////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "resonanceCore.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(resonance_core_ability_power);



void ResonanceCore::format_description(StringBuffer<512>& buffer)
{
    buffer += format(SYSTR(description_resonance_core)->c_str(),
                     resonance_core_ability_power);
}



ResonanceCore::ResonanceCore(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



Power ResonanceCore::power_usage() const
{
    const auto base_power = (*metaclass())->consumes_power();
    Power result = base_power;
    for (auto& room : parent()->rooms()) {
        if (room.get() not_eq this and room->metaclass_index() == metaclass_index()) {
            result -= resonance_core_ability_power;
        }
    }
    return result;
}



extern Sound core_destroyed;



void ResonanceCore::finalize()
{
    Room::finalize();

    if (health() == 0) {
        core_destroyed.play(4, milliseconds(600));
        core_explosion(parent(),
                       center(),
                       CoreExplosionConfig{
                           .arms_ = 5,
                           .rot_ = rng::choice<45>(rng::utility_state),
                       });
    }
}



void ResonanceCore::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::resonator_1;
    buffer[position().x + 1][position().y] = InteriorTile::resonator_2;

    buffer[position().x][position().y + 1] = InteriorTile::resonator_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::resonator_4;

    buffer[position().x][position().y + 2] = InteriorTile::core_2;
    buffer[position().x + 1][position().y + 2] = InteriorTile::plain_floor;
}



void ResonanceCore::render_exterior(App* app, TileId buffer[16][16])
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



void ResonanceCore::apply_damage(Health damage, const DamageConfiguration& conf)
{
    Room::apply_damage(damage, conf);
}



} // namespace skyland
