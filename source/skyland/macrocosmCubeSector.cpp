////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "macrocosmCubeSector.hpp"
#include "macrocosmEngine.hpp"



// NOTE: I do have a branch where I refactored some of this copy-pasted code
// into a class template. TODO: fix issues on branch and merge.



namespace skyland::macro
{



void terrain::CubeSector::restore(const Persistent& p, u8 blocks[z_limit][8][8])
{
    erase();

    memcpy(&p_, &p, sizeof p);

    for (u8 z = 0; z < macro::terrain::Sector::z_limit; ++z) {
        for (u8 x = 0; x < 8; ++x) {
            for (u8 y = 0; y < 8; ++y) {
                blocks_[z][x][y].type_ = blocks[z][x][y];
                blocks_[z][x][y].data_ = 0;
            }
        }
    }
}



} // namespace skyland::macro
