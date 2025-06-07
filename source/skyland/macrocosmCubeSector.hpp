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



class CubeSector : public MacrocosmSectorImplFull<CubeSector, 8, 8, 9, 8>
{
public:
    CubeSector(Vec2<s8> position)
        : MacrocosmSectorImplFull(position, Shape::cube)
    {
        erase();
    }


    // Projects isometric geometry into indices in the tilemap.
    static constexpr const u16 screen_mapping_lut[8][8] = {
        {14, 45, 76, 107, 138, 169, 200, 231},
        {43, 74, 105, 136, 167, 198, 229, 260},
        {72, 103, 134, 165, 196, 227, 258, 289},
        {101, 132, 163, 194, 225, 256, 287, 318},
        {130, 161, 192, 223, 254, 285, 316, 347},
        {159, 190, 221, 252, 283, 314, 345, 376},
        {188, 219, 250, 281, 312, 343, 374, 405},
        {217, 248, 279, 310, 341, 372, 403, 434}};


    // I've removed autoformatting for the winding array for this smaller-sized
    // island, so that you can see what this variable does. Other arrays are too
    // large and look bad in my editor, so they'll be formatted normally.
    // i.e. this array represents the draw order for blocks in one z-plane of an
    // island.
    static constexpr const Vec2<u8> winding_path[] = {
        // clang-format off
                                    {0, 0},
                                {1, 0}, {0, 1},
                            {2, 0}, {1, 1}, {0, 2},
                        {3, 0}, {2, 1}, {1, 2}, {0, 3},
                    {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4},
                {5, 0}, {4, 1}, {3, 2}, {2, 3}, {1, 4}, {0, 5},
            {6, 0}, {5, 1}, {4, 2}, {3, 3}, {2, 4}, {1, 5}, {0, 6},
        {7, 0}, {6, 1}, {5, 2}, {4, 3}, {3, 4}, {2, 5}, {1, 6}, {0, 7},
            {7, 1}, {6, 2}, {5, 3}, {4, 4}, {3, 5}, {2, 6}, {1, 7},
                {7, 2}, {6, 3}, {5, 4}, {4, 5}, {3, 6}, {2, 7},
                    {7, 3}, {6, 4}, {5, 5}, {4, 6}, {3, 7},
                        {7, 4}, {6, 5}, {5, 6}, {4, 7},
                            {7, 5}, {6, 6}, {5, 7},
                                {7, 6}, {6, 7},
                                    {7, 7},
        // clang-format on
    };


    void restore(const Persistent& p, u8 blocks[z_limit][8][8]) override;

    void update() override;
};



} // namespace skyland::macro::terrain
