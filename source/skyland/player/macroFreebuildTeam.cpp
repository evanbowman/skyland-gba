////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "macroFreebuildTeam.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/skyland.hpp"


namespace skyland::macro
{



void FreebuildTeam::update(Time delta)
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
