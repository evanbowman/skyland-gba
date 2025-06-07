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

#include "platform/scratch_buffer.hpp"



namespace skyland
{



class QRCode
{
public:
    static Optional<QRCode> create(const char* text);


    bool get_module(const Vec2<int>& position) const;


    using Sidelength = int;


    Sidelength size() const;


    void copy_to_vram(u16 tile_start_offset, int format);


    // NOTE: calls copy_to_vram(), and overwrites tiles starting at overlay tile
    // index 181. The caller may need to reload the overlay tile texture to
    // recover any overwritten tiles.
    void draw(const Vec2<u8>& screen_coord, int format = 0);


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
