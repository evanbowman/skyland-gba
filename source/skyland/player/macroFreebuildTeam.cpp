////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
