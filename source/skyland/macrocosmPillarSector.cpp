////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "macrocosmPillarSector.hpp"
#include "macrocosmEngine.hpp"



namespace skyland::macro
{



void terrain::PillarSector::restore(const Persistent& p, u8 blocks[16][6][6])
{
    erase();

    memcpy(&p_, &p, sizeof p);

    for (u8 z = 0; z < 16; ++z) {
        for (u8 x = 0; x < 6; ++x) {
            for (u8 y = 0; y < 6; ++y) {
                blocks_[z][x][y].type_ = blocks[z][x][y];
                blocks_[z][x][y].data_ = 0;
            }
        }
    }
}



} // namespace skyland::macro
