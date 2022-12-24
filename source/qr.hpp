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


#pragma once

#include "platform/scratch_buffer.hpp"



namespace skyland
{



class QRCode
{
public:
    static std::optional<QRCode> create(const char* text);


    bool get_module(const Vec2<int>& position) const;


    using Sidelength = int;


    Sidelength size() const;


    void copy_to_vram(Platform& pfrm, u16 tile_start_offset, int format);


    // NOTE: calls copy_to_vram(), and overwrites tiles starting at overlay tile
    // index 181. The caller may need to reload the overlay tile texture to
    // recover any overwritten tiles.
    void draw(Platform& pfrm,
              const Vec2<u8>& screen_coord,
              int format = 0);


    QRCode& data_color_index(u8 color)
    {
        data_color_ = color;
        return *this;
    }


    QRCode& position_marker_inner_color_index(u8 color)
    {
        position_marker_inner_color_ = color;
        return *this;
    }


    QRCode& position_marker_outer_color_index(u8 color)
    {
        position_marker_outer_color_ = color;
        return *this;
    }


    int drawsize(int format) const;


private:
    QRCode(ScratchBufferPtr qr_data_);


    u8 data_color_ = 3;
    u8 position_marker_inner_color_ = 3;
    u8 position_marker_outer_color_ = 3;

    ScratchBufferPtr qr_data_;
};



} // namespace skyland
