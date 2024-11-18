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


#include "solarStorm.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland::weather
{



EnvironmentId SolarStorm::id() const
{
    return id_;
}



Platform::Screen::Shader SolarStorm::shader() const
{
    return get_shader();
}



void SolarStorm::update(Time delta)
{
    if (delta > 0 and ++update_cyc_ == 30) {
        update_cyc_ = 0;

        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (auto r = APP.player_island().get_room({x, y})) {
                    auto p = (*r->metaclass())->properties();
                    if (p & RoomProperties::highly_flammable) {
                        APP.player_island().fire_create({x, y});
                    }
                }

                APP.with_opponent_island([x, y](auto& isle) {
                    if (auto r = isle.get_room({x, y})) {
                        auto p = (*r->metaclass())->properties();
                        if (p & RoomProperties::highly_flammable) {
                            isle.fire_create({x, y});
                        }
                    }
                });
            }
        }
    }
}



Platform::Screen::Shader SolarStorm::get_shader()
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x3b3854);
            case 2:
                return custom_color(0x726375);
            case 3:
                return custom_color(0xb8b1a9);
            case 4:
                if (player_island().interior_visible()) {
                    return custom_color(0x919160);
                } else {
                    return custom_color(0xd9d2d8);
                }
                break;
            case 5:
                return custom_color(0xbf6d02);
            case 6:
                return custom_color(0x7a3830);
            case 7:
                return custom_color(0x6a8228);
            case 8:
                return custom_color(0xd5e882);
            case 9:
                return custom_color(0xfffff0);
            case 10:
                if (not player_island().interior_visible()) {
                    return custom_color(0xd9d2d8);
                }
                break;
            case 11:
                return custom_color(0x63f2ff);
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xe8e8a7);
            case 14:
                return custom_color(0xbfb882);
            case 15:
                return custom_color(0x806f4a);
            }
            break;

        case ShaderPalette::tile1:
            switch (index) {
            case 1:
                return custom_color(0x3b3854);
            case 2:
                return custom_color(0x726375);
            case 3:
                return custom_color(0xb8b1a9);
            case 4:
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    // return custom_color(0x272654);
                    return custom_color(0x54314f);
                } else {
                    return custom_color(0x919160);
                }
                break;
            case 5:
                return custom_color(0xbf6d02);
            case 6:
                return custom_color(0x7a3830);
            case 7:
                return custom_color(0x6a8228);
            case 8:
                return custom_color(0xd5e882);
            case 9:
                return custom_color(0xfffff0);
            case 10: // FIXME
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x9c946b);
                }
                break;
            case 11:
                return custom_color(0x63f2ff);
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xe8e8a7);
            case 14:
                return custom_color(0xbfb882);
            case 15:
                return custom_color(0x806f4a);
            }
            break;

        case ShaderPalette::background:
            switch (index) {
            case 1:
                return custom_color(0xc4b98f);
            case 2:
                return custom_color(0xfffff0);
            case 3:
                return custom_color(0xd9d2d8);
            case 4:
                return custom_color(0xd2d483);
            case 5:
                return custom_color(0x81c7c3);
            }
            break;

        case ShaderPalette::spritesheet:
            // TODO...
            // switch (index) {
            // case 1:
            //     return custom_color(0xc95175);
            // case 8:
            //     return custom_color(0x47679e);

            // case 10:
            //     return custom_color(0x0872E9);

            // case 11:
            //     return custom_color(0x9bc4c1);

            // case 12:
            //     return custom_color(0x163061);

            // case 2:
            //     return custom_color(0x1e1121);

            // default:
            //     break;
            // }
            break;

        default:
            return k;
        }

        return k;
    };
}



} // namespace skyland::weather
