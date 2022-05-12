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


#include "macrocosmOutpostSector.hpp"
#include "macrocosmEngine.hpp"



namespace skyland::macro
{



void terrain::OutpostSector::restore(const Persistent& p, u8 blocks[4][5][5])
{
    erase();

#ifdef __GBA__
    static_assert(sizeof *this <= 232);
#endif

    memcpy(&p_, &p, sizeof p);

    for (u8 z = 0; z < 4; ++z) {
        for (u8 x = 0; x < 5; ++x) {
            for (u8 y = 0; y < 5; ++y) {
                blocks_[z][x][y].type_ = blocks[z][x][y];
                blocks_[z][x][y].data_ = 0;
            }
        }
    }
}



} // namespace skyland::macro
