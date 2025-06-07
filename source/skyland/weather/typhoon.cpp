////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "typhoon.hpp"
#include "number/random.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland::weather
{



ColorConstant Typhoon::fadein_colorize_tone() const
{
    return ColorConstant::rich_black;
}



void Typhoon::rewind(Time delta)
{
    Storm::rewind(delta);

    switch (ls_) {
    case LightningState::none:
        break;

    case LightningState::begin1:
        lightning_timer_ += delta;
        if (lightning_timer_ >= milliseconds(65)) {
            lightning_timer_ = 0;
            ls_ = LightningState::none;
            PLATFORM.screen().schedule_fade(0);
        }
        break;

    case LightningState::begin2:
        lightning_timer_ += delta;
        if (lightning_timer_ >= milliseconds(48)) {
            lightning_timer_ = 0;
            ls_ = LightningState::begin1;

            PLATFORM.screen().schedule_fade(0.6f,
                                            ColorConstant::rich_black,
                                            true,
                                            false,
                                            false,
                                            true,
                                            true);

            PLATFORM.screen().schedule_fade(0.6f,
                                            ColorConstant::silver_white,
                                            false,
                                            false,
                                            true,
                                            false,
                                            true);
        }
        break;

    case LightningState::hold:
        lightning_timer_ += delta;
        if (lightning_timer_ >= milliseconds(100)) {
            lightning_timer_ = 0;

            PLATFORM.screen().schedule_fade(0.25f,
                                            ColorConstant::rich_black,
                                            true,
                                            false,
                                            false,
                                            true,
                                            true);

            PLATFORM.screen().schedule_fade(0.25f,
                                            ColorConstant::silver_white,
                                            false,
                                            false,
                                            true,
                                            false,
                                            true);

            ls_ = LightningState::begin2;
        }
        break;

    case LightningState::fade:
        lightning_timer_ += delta;
        if (lightning_timer_ >= milliseconds(430)) {
            lightning_timer_ = 0;
            ls_ = LightningState::hold;

            PLATFORM.screen().schedule_fade(
                1.f, ColorConstant::rich_black, true, false, false, true, true);

            PLATFORM.screen().schedule_fade(1.f,
                                            ColorConstant::silver_white,
                                            false,
                                            false,
                                            true,
                                            false,
                                            true);

        } else {
            const auto amount =
                smoothstep(0.f, milliseconds(430), lightning_timer_);
            PLATFORM.screen().schedule_fade(amount,
                                            ColorConstant::rich_black,
                                            true,
                                            false,
                                            false,
                                            true,
                                            true);

            PLATFORM.screen().schedule_fade(amount,
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



void Typhoon::update(Time delta)
{
    Storm::update(delta);

    switch (ls_) {
    case LightningState::none:
        break;

    case LightningState::begin1:
        lightning_timer_ -= delta;
        if (lightning_timer_ <= 0) {
            lightning_timer_ = milliseconds(48);
            ls_ = LightningState::begin2;
            PLATFORM.screen().schedule_fade(0.25f,
                                            ColorConstant::rich_black,
                                            true,
                                            false,
                                            false,
                                            true,
                                            true);

            PLATFORM.screen().schedule_fade(0.25f,
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

            PLATFORM.speaker().play_sound("thunder_close_1", 0);

            time_stream::event::Lightning e;
            APP.time_stream().push(APP.level_timer(), e);

            for (auto& room : player_island().rooms()) {
                room->on_lightning();
            }

            if (opponent_island()) {
                for (auto& room : opponent_island()->rooms()) {
                    room->on_lightning();
                }
            }

            PLATFORM.screen().schedule_fade(
                1.f, ColorConstant::rich_black, true, false, false, true, true);

            PLATFORM.screen().schedule_fade(1.f,
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

            PLATFORM.screen().schedule_fade(0.f);

            time_stream::event::LightningDone e;
            APP.time_stream().push(APP.level_timer(), e);

        } else {
            const auto amount =
                smoothstep(0.f, milliseconds(430), lightning_timer_);
            PLATFORM.screen().schedule_fade(amount,
                                            ColorConstant::rich_black,
                                            true,
                                            false,
                                            false,
                                            true,
                                            true);

            PLATFORM.screen().schedule_fade(amount,
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



EnvironmentId Typhoon::id() const
{
    return 5;
}



Platform::Screen::Shader Typhoon::shader() const
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
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
                if (not player_island().interior_visible()) {
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
                return custom_color(0xd9dbdb);
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
                return custom_color(0x5d818f);
            case 2:
                return custom_color(0xcdd1d0);
            case 3:
                return custom_color(0x80acb0);
            case 4:
                return custom_color(0x395e70);
            case 5:
                return custom_color(0x6a92a1);
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



void Typhoon::on_lightning()
{
    ls_ = LightningState::begin1;
    lightning_timer_ = milliseconds(65);

    PLATFORM.screen().schedule_fade(
        0.6f, ColorConstant::rich_black, true, false, false, true, true);

    PLATFORM.screen().schedule_fade(
        0.6f, ColorConstant::silver_white, false, false, true, false, true);
}



} // namespace skyland::weather
