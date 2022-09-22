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


#include "storm.hpp"
#include "number/random.hpp"
#include "skyland/skyland.hpp"



namespace skyland::weather
{



static const int rain_pos_scale = 128;
static_assert(rain_pos_scale % 2 == 0);



Storm::Storm()
{
    auto& s = *state_;

    auto& gen = rng::utility_state;
    s.thunder_timer_ = seconds(6) + rng::choice(seconds(5), gen);
    s.lightning_timer_ = seconds(7) + rng::choice(14, gen) * seconds(1);

    const auto scale = rain_pos_scale;

    for (auto& rd : s.raindrops_) {
        // FIXME: pass in Platform::screen() and use screen size!
        rd.x = rng::choice(240 * scale, rng::utility_state);
        rd.y = rng::choice(160 * scale, rng::utility_state);
    }
}



void Environment::on_pause()
{
    // ...
}



void Storm::update(Platform& pfrm, App& app, Microseconds delta)
{
    static const auto scale = rain_pos_scale;
    static_assert(scale % 2 == 0);

    auto& gen = rng::utility_state;

    auto camera = app.camera()->center().cast<s16>();
    auto camera_diff_x = camera.x - last_camera_.x;
    auto camera_diff_y = camera.y - last_camera_.y;

    auto& s = *state_;

    s.thunder_timer_ -= delta;
    if (s.thunder_timer_ <= 0) {
        s.thunder_timer_ = seconds(8) + rng::choice(seconds(25), gen);
        if (rng::choice<2>(gen)) {
            pfrm.speaker().play_sound("thunder_1", 0);
        } else {
            pfrm.speaker().play_sound("thunder_2", 0);
        }
    }

    s.lightning_timer_ -= delta;
    if (s.lightning_timer_ <= 0) {
        s.lightning_timer_ = seconds(9) + rng::choice(4, gen) * seconds(1);

        if (app.opponent_island() and not app.opponent_island()->is_destroyed() and
            not app.player_island().is_destroyed()) {
            on_lightning(pfrm);
        }
    }

    const s16 sd = delta;
    const s16 sx = pfrm.screen().size().x + 24;
    const s16 sy = pfrm.screen().size().y;

    for (auto& rd : s.raindrops_) {
        if ((rd.x / scale) < 0 or (rd.y / scale) > sy or (rd.x / scale) > sx or
            (rd.y / scale) < -24) {

            if (delta == 0) {
                rd.x = rng::choice(pfrm.screen().size().x * scale, gen);
                rd.y = rng::choice(pfrm.screen().size().y * scale, gen);
            } else {
                if (rng::choice<2>(rng::utility_state)) {
                    rd.x = pfrm.screen().size().x * scale;
                    rd.y = rng::choice(pfrm.screen().size().y * scale, gen);
                } else {
                    rd.x = rng::choice(pfrm.screen().size().x * scale, gen);
                    rd.y = 0;
                }
            }
        } else {
            rd.x -= (sd >> 6) + (sd >> 8);
            rd.y += (sd >> 6) + (sd >> 8);

            rd.x -= camera_diff_x * (scale + 48);
            rd.y -= camera_diff_y * (scale + 48);
        }
    }

    if (camera_diff_x != 0 or camera_diff_y != 0) {
        last_camera_ = camera;
    }
}



void Storm::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    const auto scale = rain_pos_scale;

    auto& gen = rng::utility_state;

    auto& s = *state_;

    for (auto& rd : s.raindrops_) {
        if ((rd.x / scale) > (s16)pfrm.screen().size().x or
            (rd.y / scale) < 0) {
            if (rng::choice<2>(rng::utility_state)) {
                rd.x = 0;
                rd.y = rng::choice(pfrm.screen().size().y * scale, gen);
            } else {
                rd.x = rng::choice(pfrm.screen().size().x * scale, gen);
                rd.y = pfrm.screen().size().y * scale;
            }
        } else {
            rd.x += (delta >> 6) + (delta >> 8);
            rd.y -= (delta >> 6) + (delta >> 8);
        }
    }
}



void Storm::display(Platform& pfrm, App& app)
{
    auto batch = allocate_dynamic<Buffer<Vec2<s32>, 64>>("rain-spr-buffer");

    const auto scale = rain_pos_scale;

    auto& s = *state_;

    for (auto& rd : s.raindrops_) {
        batch->push_back({rd.x / scale, rd.y / scale});
    }

    Platform::Screen::SpriteBatchOptions opts;
    opts.position_absolute_ = true;

    pfrm.screen().draw_batch(89, *batch, opts);
}



const char* Storm::music() const
{
    return "solecism";
}



Platform::Screen::Shader Storm::shader(App& app) const
{
    return [&app](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x10405c);
            case 2:
                return custom_color(0x5e728c);
            case 3:
                return custom_color(0x95bbbd);
            case 5:
                return custom_color(0xc7612e);
            case 8:
                return custom_color(0xb8ea80);
            case 9:
                return custom_color(0xe8ebe6);
            case 10:
                if (not player_island(app).interior_visible()) {
                    return custom_color(0x9adbd6);
                }
                break;
            case 12:
                return custom_color(0x1477b5);
            case 13:
                return custom_color(0xdee7a5);
            case 14:
                return custom_color(0xaab87d);
            case 15:
                return custom_color(0x5f6e3b);
            }
            break;

        case ShaderPalette::tile1:
            switch (index) {
            case 1:
                return custom_color(0x10405c);
            case 2:
                return custom_color(0x5e728c);
            case 3:
                return custom_color(0x95bbbd);
            case 5:
                return custom_color(0xc7612e);
            case 8:
                return custom_color(0xb8ea80);
            case 9:
                return custom_color(0xe8ebe6);
            case 10: // FIXME
                if (opponent_island(app) and
                    not opponent_island(app)->interior_visible()) {
                    return custom_color(0x899668);
                }
                break;
            case 12:
                return custom_color(0x1477b5);
            case 13:
                return custom_color(0xdee7a5);
            case 14:
                return custom_color(0xaab87d);
            case 15:
                return custom_color(0x5f6e3b);
            }
            break;

        case ShaderPalette::background:
            switch (index) {
            case 1:
                return custom_color(0x6fbdb9);
            case 2:
                return custom_color(0xe8ebe6);
            case 3:
                return custom_color(0x9adbd6);
            case 4:
                return custom_color(0x49a7b8);
            }
            break;

        case ShaderPalette::spritesheet:
            switch (index) {

            case 8:
                return custom_color(0x5e728c);

            case 10:
                return custom_color(0x1780bd);

            case 11:
                return custom_color(0x95bbbd);

            case 12:
                return custom_color(0x10405c);

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
