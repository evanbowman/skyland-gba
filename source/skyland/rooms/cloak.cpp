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


#include "cloak.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Cloak::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_cloak)->c_str();
}



Cloak::Cloak(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Cloak::update(Microseconds delta)
{
    Room::update(delta);

    const int x = position().x;
    const int y = position().y;

    if (++timer_ == 10) {
        for (int xx = x - 2; xx < x + 3; ++xx) {
            for (int yy = y - 2; yy < y + 3; ++yy) {
                if (xx > -1 and yy > -1 and xx < 15 and yy < 15) {
                    if (auto room = parent()->get_room({(u8)x, (u8)y})) {
                        room->set_visually_cloaked(true);
                    }
                }
            }
        }
        timer_ = 0;
    }

    Room::ready();
}



void Cloak::rewind(Microseconds delta)
{
    Room::rewind(delta);
}



void Cloak::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cloak;
}



void Cloak::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::cloak;
}



bool Cloak::cloaks_coordinate(const RoomCoord& c)
{
    return abs(c.x - position().x) < 3 and abs(c.y - position().y) < 3;
}



void Cloak::render_cloak(TileId buffer[16][16])
{
    if (parent() == &player_island() and parent()->interior_visible()) {
        return;
    }

    auto x = position().x;
    auto y = position().y;

    for (int xx = x - 2; xx < x + 3; ++xx) {
        for (int yy = y - 2; yy < y + 3; ++yy) {
            if (xx > -1 and yy > -1 and xx < 15 and yy < 15) {
                auto t = buffer[xx][yy];
                switch (t) {
                case Tile::roof_plain:
                case Tile::roof_chimney:
                case Tile::roof_plain_intersect_armored_wall:
                case Tile::roof_flag:
                case Tile::roof_strut:
                case Tile::flag_start:
                case Tile::tin_chimney:
                case Tile::roof_strut_joined:
                case Tile::flag_mount:
                    if (t == Tile::roof_flag and
                        buffer[xx][yy - 1] == Tile::flag_start) {
                        buffer[xx][yy - 1] = Tile::null;
                    }
                    t = Tile::null;
                    break;

                case Tile::strut:
                case Tile::strut_top:
                case Tile::grass:
                case Tile::null:
                case Tile::cloak:
                    break;

                default:
                    t = Tile::cloaked;
                    break;
                }

                buffer[xx][yy] = t;
            }
        }
    }
}



} // namespace skyland
