////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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


#include "deflector.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(deflector_shield_strength);



void Deflector::format_description(StringBuffer<512>& buffer)
{
    buffer += format<256>(SYSTR(description_deflector)->c_str(),
                          deflector_shield_strength);
}



Deflector::Deflector(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



bool Deflector::allows_powerdown()
{
    return true;
}



void Deflector::on_powerchange()
{
    parent()->schedule_recompute_deflector_shields();
}



void Deflector::project_deflector_shield()
{
    if (is_powered_down()) {
        return;
    }

    int left = clamp(position().x - 3, 0, 15);
    int right =
        clamp(position().x + 3, 0, (int)(parent()->terrain().size() - 1));
    int top = clamp(position().y - 3, 4, 15);
    int bot = clamp(position().y + 3, 0, 15);

    for (u8 x = left; x < right + 1; ++x) {
        for (u8 y = top; y < bot + 1; ++y) {
            if (auto room = parent()->get_room({x, y})) {
                room->set_shielded(true);
            }
            // FIXME: this doesn't work...
            // if (auto drone = parent()->get_drone({x, y})) {
            //     (*drone)->set_shielded(true);
            // }
        }
    }
}



void Deflector::render_interior(App* app, TileId buffer[16][16])
{
    render_exterior(app, buffer);
    buffer[position().x][position().y] = Tile::deflector_source;
}



void Deflector::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::deflector_source;

    if (is_powered_down()) {
        return;
    }

    int left = clamp(position().x - 3, 0, 15);
    int right =
        clamp(position().x + 3, 0, (int)(parent()->terrain().size() - 1));
    int top = clamp(position().y - 3, 4, 15);
    int bot = clamp(position().y + 3, 0, 15);

    auto set_if_empty = [&](int x, int y, TileId t) {
        auto prev = buffer[x][y];
        if (prev >= Tile::deflector_boundary_tl and
            prev <= Tile::deflector_boundary_br and prev not_eq t) {

            auto is_corner = [](auto t) {
                return t == Tile::deflector_boundary_tl or
                       t == Tile::deflector_boundary_bl or
                       t == Tile::deflector_boundary_tr or
                       t == Tile::deflector_boundary_br;
            };

            if (is_corner(prev)) {
                if (is_corner(t)) {
                    buffer[x][y] = Tile::deflector_boundary_center;
                    return;
                }
                buffer[x][y] = t;
                return;
            }

            if ((prev == Tile::deflector_boundary_t or
                 prev == Tile::deflector_boundary_l or
                 prev == Tile::deflector_boundary_r or
                 prev == Tile::deflector_boundary_b) and
                not is_corner(t)) {
                buffer[x][y] = Tile::deflector_boundary_center;
                return;
            }

            // if (prev == Tile::deflector_boundary_tl or
            //     prev == Tile::deflector_boundary_tr or
            //     prev == Tile::deflector_boundary_bl or
            //     prev == Tile::deflector_boundary_br) {
            //     buffer[x][y] = t;
            //     return;
            // }
            // buffer[x][y] = Tile::null;
            // return;
        }
        if (not buffer[x][y]) {
            buffer[x][y] = t;
        }
    };
    set_if_empty(left, top, Tile::deflector_boundary_tl);
    set_if_empty(right, top, Tile::deflector_boundary_tr);
    set_if_empty(left, bot, Tile::deflector_boundary_bl);
    set_if_empty(right, bot, Tile::deflector_boundary_br);

    for (int y = top + 1; y < bot; ++y) {
        set_if_empty(left, y, Tile::deflector_boundary_l);
        set_if_empty(right, y, Tile::deflector_boundary_r);
    }

    for (int x = left + 1; x < right; ++x) {
        set_if_empty(x, top, Tile::deflector_boundary_t);
        set_if_empty(x, bot, Tile::deflector_boundary_b);
    }

    for (int x = left + 1; x < right; ++x) {
        for (int y = top + 1; y < bot; ++y) {
            set_if_empty(x, y, Tile::deflector_boundary_center);
        }
    }
}



} // namespace skyland
