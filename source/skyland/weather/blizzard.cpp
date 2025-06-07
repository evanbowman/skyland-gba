////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "blizzard.hpp"
#include "number/random.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland::weather
{


Platform::Screen::Shader Blizzard::get_shader()
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x270d42);
            case 2:
                return custom_color(0x535c78);
            case 3:
                return custom_color(0x9bbfc4);
            case 5:
                return custom_color(0xde2c4a);
            case 7:
                return custom_color(0x106469);
            case 8:
                return custom_color(0x90de81);
            case 9:
                return custom_color(0xdedede);
            case 10:
                if (not player_island().interior_visible()) {
                    return custom_color(0x9db3c9);
                }
                break;
            case 12:
                return custom_color(0x115ba6);
            case 13:
                return custom_color(0xdee7a5);
            case 14:
                return custom_color(0xaab87d);
            case 15:
                return custom_color(0x475c32);
            }
            break;

        case ShaderPalette::tile1:
            switch (index) {
            case 1:
                return custom_color(0x270d42);
            case 2:
                return custom_color(0x535c78);
            case 3:
                return custom_color(0x9bbfc4);
            case 4:
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x322f59);
                }
                break;
            case 5:
                return custom_color(0xde2c4a);
            case 7:
                return custom_color(0x106469);
            case 8:
                return custom_color(0x90de81);
            case 9:
                return custom_color(0xdedede);
            case 10: // FIXME
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x838c6b);
                }
                break;
            case 12:
                return custom_color(0x115ba6);
            case 13:
                return custom_color(0xdee7a5);
            case 14:
                return custom_color(0xaab87d);
            case 15:
                return custom_color(0x475c32);
            }
            break;

        case ShaderPalette::background:
            switch (index) {
            case 1:
                return custom_color(0x7d8dab);
            case 2:
                return custom_color(0xced6d6);
            case 3:
                return custom_color(0x9db3c9);
            case 4:
                return custom_color(0x5b6987);
            case 5:
                return custom_color(0x8a9dbd);
            }
            break;

        case ShaderPalette::spritesheet:
            switch (index) {
            case 2:
                return custom_color(0x492f5e);

            case 8:
                return custom_color(0x444c63);

            case 11:
                return custom_color(0x95bbbd);

            case 10:
                return custom_color(0x115ba6);

            case 12:
                return custom_color(0x270d42);

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



EnvironmentId Blizzard::id() const
{
    return 4;
}



Platform::Screen::Shader Blizzard::shader() const
{
    return get_shader();
}



void Blizzard::update(Time delta)
{
    Storm::update(delta);
}



void Blizzard::display()
{
    if (PLATFORM.screen().fade_active()) {
        return;
    }

    using Buf = Buffer<Vec2<s32>, 64>;
    auto batch =
        allocate_dynamic_fast<Buf>("rain-spr-buffer", Buf::SkipZeroFill{});

    static const int rain_pos_scale = 128;
    const auto scale = rain_pos_scale;

    auto& s = *state_;

    for (int i = 0; i < s.particle_count_; ++i) {
        auto& rd = s.raindrops_[i];
        batch->push_back({rd.x / scale, rd.y / scale});
    }

    Platform::Screen::SpriteBatchOptions opts;
    opts.position_absolute_ = true;
    opts.sz_ = Sprite::Size::w8_h8;

    PLATFORM.screen().draw_batch(89 * 8 + 1, *batch, opts);
}



} // namespace skyland::weather
