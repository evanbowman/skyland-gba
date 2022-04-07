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


#include "infirmary.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Infirmary::Infirmary(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}


void Infirmary::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    int characters_healing = 0;

    for (auto& character : characters()) {
        if (character->owner() == &parent()->owner() and
            character->state() not_eq BasicCharacter::State::fighting) {
            ++characters_healing;
        }
    }

    if (characters_healing) {
        heal_timer_ += delta;
        if (heal_timer_ > milliseconds(1000)) {
            heal_timer_ -= milliseconds(1000);
            int distribute_health = 20;
            distribute_health /= characters_healing;
            for (auto& character : characters()) {
                if (character->owner() == &parent()->owner() and
                    character->state() not_eq BasicCharacter::State::fighting) {
                    character->heal(pfrm, app, distribute_health);
                }
            }
        }
    }
}


void Infirmary::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::infirmary_1;
    buffer[position().x][position().y + 1] = InteriorTile::infirmary_2;
    buffer[position().x + 1][position().y] = InteriorTile::infirmary_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::plain_floor;
}


void Infirmary::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



} // namespace skyland
