////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "night.hpp"



namespace skyland::weather
{



void Night::display()
{
}



EnvironmentId Night::id() const
{
    return id_;
}



Platform::Screen::Shader Night::shader() const
{
    return get_shader();
}



Platform::Screen::Shader Night::get_shader()
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x163061);
            case 2:
                return custom_color(0x47679e);
            case 3:
                return custom_color(0x9bc4c1);
            case 4:
                if (player_island().interior_visible()) {
                    return custom_color(0x919160);
                } else {
                    return custom_color(0xf7ec8b);
                }
                break;
            case 5:
                return custom_color(0xc95175);
            case 6:
                return custom_color(0x163061);
            case 7:
                return custom_color(0x27788f);
            case 8:
                return custom_color(0x69cfcb);
            case 9:
                return custom_color(0xe6f7ed);
            case 10:
                if (not player_island().interior_visible()) {
                    return custom_color(0xa1d7e6);
                }
                break;
            case 11:
                return custom_color(0x63f2ff);
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xd4ebbe);
            case 14:
                return custom_color(0x98b39d);
            case 15:
                return custom_color(0x526b59);
            }
            break;

        case ShaderPalette::tile1:
            switch (index) {
            case 1:
                return custom_color(0x163061);
            case 2:
                return custom_color(0x47679e);
            case 3:
                return custom_color(0x9bc4c1);
            case 4:
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    // return custom_color(0x272654);
                    return custom_color(0xf7ec8b);
                } else {
                    return custom_color(0x919160);
                }
                break;
            case 5:
                return custom_color(0xc95175);
            case 6:
                return custom_color(0x163061);
            case 7:
                return custom_color(0x27788f);
            case 8:
                return custom_color(0x69cfcb);
            case 9:
                return custom_color(0xe6f7ed);
            case 10: // FIXME
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x809174);
                }
                break;
            case 11:
                return custom_color(0x63f2ff);
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xd4ebbe);
            case 14:
                return custom_color(0x98b39d);
            case 15:
                return custom_color(0x526b59);
            }
            break;

        case ShaderPalette::background:
            switch (index) {
            case 1:
                return custom_color(0x21426b);
            case 2:
                return custom_color(0xaed1fc);
            case 3:
                return custom_color(0x5481b8);
            case 4:
                return custom_color(0x15365e);
            case 5:
                return custom_color(0x81c7c3);
            }
            break;

        case ShaderPalette::spritesheet:
            switch (index) {
            case 1:
                return custom_color(0xc95175);
            case 8:
                return custom_color(0x47679e);

            case 10:
                return custom_color(0x0872E9);

            case 11:
                return custom_color(0x9bc4c1);

            case 12:
                return custom_color(0x163061);

            case 2:
                return custom_color(0x1e1121);

            default:
                break;
            }
            break;

        default:
            return k;
        }

        return k;
    };
}



} // namespace skyland::weather
