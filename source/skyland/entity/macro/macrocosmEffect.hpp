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


#include "macrocosmEntity.hpp"



namespace skyland::macro
{



class MacrocosmEffect : public MacrocosmEntity
{
public:
    MacrocosmEffect(const Vec2<Fixnum>& position,
                    int begin_tile,
                    int end_tile,
                    Time rate)
        : rate_(rate), begin_tile_(begin_tile), end_tile_(end_tile)
    {
        sprite_.set_size(Sprite::Size::w32_h32);
        sprite_.set_texture_index(begin_tile);
        sprite_.set_position(position);
        sprite_.set_origin({8, 8});
    }


    void update(macro::Engine& s, Time delta) override
    {
        timer_ += delta;
        if (timer_ >= rate_) {
            timer_ -= rate_;

            auto t = sprite_.get_texture_index();
            if (t == end_tile_) {
                kill();
                return;
            }

            ++t;
            sprite_.set_texture_index(t);
        }
    }


private:
    Time timer_ = 0;
    Time rate_;
    u16 begin_tile_;
    u16 end_tile_;
};



} // namespace skyland::macro
