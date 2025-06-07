////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "macrocosmSector.hpp"
#include "macrocosmSectorImpl.hpp"



namespace skyland::macro::terrain
{



class PillarSector : public MacrocosmSectorImplFull<PillarSector, 6, 6, 16, 16>
{
public:
    PillarSector(Vec2<s8> position)
        : MacrocosmSectorImplFull(position, Shape::pillar)
    {
        erase();

        set_z_view(16);
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
        {0, 0}, {1, 0}, {0, 1}, {2, 0}, {1, 1}, {0, 2}, {3, 0}, {2, 1}, {1, 2},
        {0, 3}, {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4}, {5, 0}, {4, 1}, {3, 2},
        {2, 3}, {1, 4}, {0, 5}, {5, 1}, {4, 2}, {3, 3}, {2, 4}, {1, 5}, {5, 2},
        {4, 3}, {3, 4}, {2, 5}, {5, 3}, {4, 4}, {3, 5}, {5, 4}, {4, 5}, {5, 5},
    };


    void restore(const Persistent& p, u8 blocks[16][6][6]) override;

    void update() override;
};



} // namespace skyland::macro::terrain
