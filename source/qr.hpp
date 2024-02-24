////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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
