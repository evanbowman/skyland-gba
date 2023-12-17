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


#include "slightlyOvercast.hpp"
#include "skyland/island.hpp"



namespace skyland::weather
{



SlightlyOvercast::SlightlyOvercast()
{
    auto& s = *state_;
    s.particle_count_ = 2;

    const auto scale = 128;

    for (int i = 0; i < s.particle_count_; ++i) {
        auto& rd = s.raindrops_[i];
        rd.x = rng::choice(240 * scale, rng::utility_state);
        rd.y = rng::choice(160 * scale, rng::utility_state);
    }

    s.spr_ += 3;
}



int SlightlyOvercast::id() const
{
    return 2;
}



void SlightlyOvercast::display()
{
    if (not PLATFORM.screen().fade_active()) {
        (*state_).display();
    }

    ClearSkies::display();
}



void SlightlyOvercast::update(Time delta)
{
    ClearSkies::update(delta);
    (*state_).update(delta);
}



void SlightlyOvercast::rewind(Time delta)
{
    (*state_).rewind(delta);
}



Platform::Screen::Shader SlightlyOvercast::shader() const
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x14355F);
            case 2:
                return custom_color(0x636A8F);
            case 3:
                return custom_color(0x9CB8C2);
            case 5:
                return custom_color(0xD95125);
            case 8:
                return custom_color(0xb8ea80);
            case 9:
                return custom_color(0xEFF2E9);
            case 10:
                if (not player_island().interior_visible()) {
                    return custom_color(0x9FDEE2);
                }
                break;
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xDBE4A4);
            case 14:
                return custom_color(0xA9B37E);
            case 15:
                return custom_color(0x676C3A);
            }
            break;

        case ShaderPalette::tile1:
            switch (index) {
            case 1:
                return custom_color(0x14355F);
            case 2:
                return custom_color(0x636A8F);
            case 3:
                return custom_color(0x9CB8C2);
            case 5:
                return custom_color(0xD95125);
            case 8:
                return custom_color(0xb8ea80);
            case 9:
                return custom_color(0xEFF2E9);
            case 10: // FIXME
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x96976A);
                }
                break;
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xDBE4A4);
            case 14:
                return custom_color(0xA9B37E);
            case 15:
                return custom_color(0x676C3A);
            }
            break;

        case ShaderPalette::background:
            switch (index) {
            case 1:
                return custom_color(0x67B6D3);
            case 2:
                return custom_color(0xEFF3E8);
            case 3:
                return custom_color(0x9FDEE2);
            case 4:
                return custom_color(0x58A8D9);
            case 5:
                return custom_color(0x81c7c3);
            }
            break;

        case ShaderPalette::spritesheet:
            switch (index) {

            case 8:
                return custom_color(0x616891);

            case 10:
                return custom_color(0x0872E9);

            case 11:
                return custom_color(0x87ACC8);

            case 12:
                return custom_color(0x14355F);

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
