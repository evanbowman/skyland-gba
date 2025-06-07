////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



void Cloak::update(Time delta)
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



void Cloak::rewind(Time delta)
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
