////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



EnvironmentId SlightlyOvercast::id() const
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
