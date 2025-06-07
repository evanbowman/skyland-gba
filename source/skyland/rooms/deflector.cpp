////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    make_format(buffer,
                SYSTR(description_deflector)->c_str(),
                deflector_shield_strength);
}



Deflector::Deflector(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



static const auto activate_time = milliseconds(750);



void phase_ripple(const Vec2<Fixnum>& pos);



void Deflector::update(Time delta)
{
    Room::update(delta);

    if (active_timer_ < activate_time) {
        Room::ready();
        if (is_powered_down()) {
            return;
        }
        static const auto ripple_offset = milliseconds(280);
        if (active_timer_ < activate_time - ripple_offset) {
            if (active_timer_ + delta >= activate_time - ripple_offset) {
                phase_ripple(visual_center());
                PLATFORM.speaker().play_sound("deflector.raw", 0);
            }
        }
        active_timer_ += delta;
        if (active_timer_ >= activate_time) {
            parent()->schedule_repaint();
        }
    }
}



void Deflector::rewind(Time delta)
{
    active_timer_ = activate_time;
}



bool Deflector::allows_powerdown()
{
    return true;
}



void Deflector::on_powerchange()
{
    parent()->schedule_recompute_deflector_shields();
    active_timer_ = 0;
    Room::ready();
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

    if (active_timer_ < activate_time) {
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
