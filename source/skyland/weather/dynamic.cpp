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


#include "dynamic.hpp"
#include "platform/color.hpp"



namespace skyland::weather
{



Dynamic::Dynamic(App& app)
{
    compute_palettes(app, 70);
}


static const ColorConstant t0_clear_palette[15] = {
    custom_color(0x163061),
    custom_color(0x666691),
    custom_color(0x9fb7c5),
    custom_color(0x000000),
    custom_color(0xe24920),
    custom_color(0x6e2d4a),
    custom_color(0x277b6e),
    custom_color(0xb8ea80),
    custom_color(0xf2f5eb),
    custom_color(0xa2dfe8),
    custom_color(0x66fff7),
    custom_color(0x165fce),
    custom_color(0xd9e2a3),
    custom_color(0xa9b07f),
    custom_color(0x6b6b39),
};


static const ColorConstant t0_storm_palette[15] = {
    custom_color(0x10405c),
    custom_color(0x5e728c),
    custom_color(0x95bbbd),
    custom_color(0x000000),
    custom_color(0xc7612e),
    custom_color(0x6e2d4a),
    custom_color(0x277b6e),
    custom_color(0xb8ea80),
    custom_color(0xe8ebe6),
    custom_color(0xa2dfe8),
    custom_color(0x66fff7),
    custom_color(0x1477b5),
    custom_color(0xdee7a5),
    custom_color(0xaab87d),
    custom_color(0x5f6e3b),
};


static const ColorConstant background_storm_palette[4] = {
    custom_color(0x6fbdb9),
    custom_color(0xe8ebe6),
    custom_color(0x9adbd6),
    custom_color(0x49a7b8),
};



static const ColorConstant background_clear_palette[4] = {
    custom_color(0x63b2e0),
    custom_color(0xf2f7e9),
    custom_color(0xa2dfe8),
    custom_color(0x5fa8ea),
};



void Dynamic::compute_palettes(App& app, u8 scale)
{
    auto mix = [&](auto c1, auto c2, int var) {
        auto col1 = Color(c1);
        auto col2 = Color(c2);

        return Color(fast_interpolate(col1.r_, col2.r_, var),
                     fast_interpolate(col1.g_, col2.g_, var),
                     fast_interpolate(col1.b_, col2.b_, var));
    };


    auto& s = *state_;

    for (int i = 0; i < 15; ++i) {
        s.t0_palette_.values_[i + 1] =
            mix(t0_clear_palette[i], t0_storm_palette[i], scale).hex();
    }

    for (int i = 0; i < 4; ++i) {
        s.background_palette_.values_[i + 1] =
            mix(background_clear_palette[i], background_storm_palette[i], scale)
                .hex();
    }
}



void Dynamic::update(Platform& pfrm, App& app, Microseconds delta)
{
}



void Dynamic::rewind(Platform& pfrm, App& app, Microseconds delta)
{
}



Platform::Screen::Shader Dynamic::shader(App& app) const
{
    return [state = ScratchMemory<State>(state_)](
               ShaderPalette palette, ColorConstant k, int arg, int index) {
        auto& s = *state;

        switch (palette) {
        case ShaderPalette::tile0: {
            auto v = s.t0_palette_.values_[index];
            if ((int)v) {
                return v;
            }
            return k;
        }

        case ShaderPalette::tile1: {
            auto v = s.t1_palette_.values_[index];
            if ((int)v) {
                return v;
            }
            return k;
        }

        case ShaderPalette::background: {
            auto v = s.background_palette_.values_[index];
            if ((int)v) {
                return v;
            }
            return k;
        }

        case ShaderPalette::spritesheet: {
            auto v = s.sprite_palette_.values_[index];
            if ((int)v) {
                return v;
            }
            return k;
        }

        default:
            return k;
        }

        return k;
    };
}



} // namespace skyland::weather