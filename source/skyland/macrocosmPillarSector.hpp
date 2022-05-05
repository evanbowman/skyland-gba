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


#pragma once


#include "macrocosmSector.hpp"
#include "macrocosmSectorImpl.hpp"



namespace skyland::macro::terrain
{



class PillarSector : public MacrocosmSectorImpl<PillarSector, 6, 6, 16, 14>
{
public:
    PillarSector(Vec2<s8> position) : MacrocosmSectorImpl(position, Shape::pillar)
    {
        erase();
    }


    // Projects isometric geometry into indices in the tilemap.
    static constexpr const u16 screen_mapping_lut[6][6] = {
        {14, 45, 76, 107, 138, 169},
        {43, 74, 105, 136, 167, 198},
        {72, 103, 134, 165, 196, 227},
        {101, 132, 163, 194, 225, 256},
        {130, 161, 192, 223, 254, 285},
        {159, 190, 221, 252, 283, 314}};


    static constexpr const Vec2<u8> winding_path[] = {
        {0, 0}, {1, 0}, {0, 1}, {2, 0}, {1, 1}, {0, 2}, {3, 0}, {2, 1},
        {1, 2}, {0, 3}, {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4}, {5, 0},
        {4, 1}, {3, 2}, {2, 3}, {1, 4}, {0, 5},
        {5, 1}, {4, 2}, {3, 3}, {2, 4}, {1, 5},
        {5, 2}, {4, 3}, {3, 4}, {2, 5},
        {5, 3}, {4, 4}, {3, 5},
        {5, 4}, {4, 5},
        {5, 5},
    };


    void restore(const Persistent& p, u8 blocks[16][6][6]) override;

    void update() override;
};



} // namespace skyland::macro::terrain
