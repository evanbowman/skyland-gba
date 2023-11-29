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


#include "macroFreebuildTeam.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/skyland.hpp"


namespace skyland::macro
{



void FreebuildTeam::update(Microseconds delta)
{
    PlayerP1::update(delta);

    if (PLATFORM.network_peer().is_connected()) {
        network::poll_messages(*this);
    } else {
        info("lost connection to freebuild friend!");
        APP.swap_player<PlayerP1>();
        return;
    }
}



void FreebuildTeam::receive(const network::packet::MacroSetBlock& p)
{
    auto orientation = (int)macrocosm().sector().persistent().orientation_;

    Vec3<u8> coord = {p.x_, p.y_, p.z_};

    while (orientation not_eq p.rot_) {
        auto rotate_coord = [](Vec3<u8> input) -> Vec3<u8> {
            return {(u8)((10 - 1) - input.y), input.x, input.z};
        };

        coord = rotate_coord(coord);
        coord = rotate_coord(coord);
        coord = rotate_coord(coord);
        orientation += 1;
        orientation %= 4;
    }

    auto& existing = macrocosm().sector().get_block(coord);
    if (existing.type() == terrain::Type::selector) {
        macrocosm().sector().set_cursor({p.x_, p.y_, u8(p.z_ + 1)});
    }

    macrocosm().sector().set_block(coord, (terrain::Type)p.type_);
}



} // namespace skyland::macro
