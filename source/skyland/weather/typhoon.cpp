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


#include "typhoon.hpp"
#include "number/random.hpp"



namespace skyland::weather
{



void Typhoon::update(Platform& pfrm, App& app, Microseconds delta)
{
    Storm::update(pfrm, app, delta);

    switch (ls_) {
    case LightningState::none:
        break;

    case LightningState::begin1:
        lightning_timer_ -= delta;
        if (lightning_timer_ <= 0) {
            lightning_timer_ = milliseconds(48);
            ls_ = LightningState::begin2;
            pfrm.screen().schedule_fade(0.25f,
                                ColorConstant::rich_black,
                                true,
                                false,
                                false,
                                true,
                                true);

            pfrm.screen().schedule_fade(0.25f,
                                        ColorConstant::silver_white,
                                        false,
                                        false,
                                        true,
                                        false,
                                        true);
        }
        break;

    case LightningState::begin2:
        lightning_timer_ -= delta;
        if (lightning_timer_ <= 0) {
            lightning_timer_ = milliseconds(100);
            ls_ = LightningState::hold;

            pfrm.speaker().play_sound("thunder_close_1", 0);

            pfrm.screen().schedule_fade(1.f,
                                        ColorConstant::rich_black,
                                        true,
                                        false,
                                        false,
                                        true,
                                        true);

            pfrm.screen().schedule_fade(1.f,
                                        ColorConstant::silver_white,
                                        false,
                                        false,
                                        true,
                                        false,
                                        true);
        }
        break;

    case LightningState::hold:
        lightning_timer_ -= delta;
        if (lightning_timer_ <= 0) {
            lightning_timer_ = milliseconds(430);
            ls_ = LightningState::fade;
        }
        break;

    case LightningState::fade:
        lightning_timer_ -= delta;
        if (lightning_timer_ <= 0) {
            lightning_timer_ = 0;
            ls_ = LightningState::none;

            pfrm.screen().schedule_fade(0.f);
        } else {
            const auto amount = smoothstep(0.f, milliseconds(430), lightning_timer_);
            pfrm.screen().schedule_fade(amount,
                                        ColorConstant::rich_black,
                                        true,
                                        false,
                                        false,
                                        true,
                                        true);

            pfrm.screen().schedule_fade(amount,
                                        ColorConstant::silver_white,
                                        false,
                                        false,
                                        true,
                                        false,
                                        true);
        }
        break;
    }
}



Platform::Screen::Shader Typhoon::shader(App& app) const
{
    return [&app](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x270d42);
            case 2:
                return custom_color(0x444c63);
            case 3:
                return custom_color(0x95bbbd);
            case 5:
                return custom_color(0xde2c4a);
            case 7:
                return custom_color(0x106469);
            case 8:
                return custom_color(0x90de81);
            case 9:
                return custom_color(0xd9dbdb);
            case 10:
                if (not player_island(app).interior_visible()) {
                    return custom_color(0x80acb0);
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
                return custom_color(0x444c63);
            case 3:
                return custom_color(0x95bbbd);
            case 4:
                if (opponent_island(app) and
                    not opponent_island(app)->interior_visible()) {
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
                return custom_color(0xd9dbdb);
            case 10: // FIXME
                if (opponent_island(app) and
                    not opponent_island(app)->interior_visible()) {
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
                return custom_color(0x5d818f);
            case 2:
                return custom_color(0xcdd1d0);
            case 3:
                return custom_color(0x80acb0);
            case 4:
                return custom_color(0x395e70);
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



void Typhoon::on_lightning(Platform& pfrm)
{
    ls_ = LightningState::begin1;
    lightning_timer_ = milliseconds(65);

    pfrm.screen().schedule_fade(0.6f,
                                ColorConstant::rich_black,
                                true,
                                false,
                                false,
                                true,
                                true);

    pfrm.screen().schedule_fade(0.6f,
                                ColorConstant::silver_white,
                                false,
                                false,
                                true,
                                false,
                                true);
}



} // namespace skyland::weather
