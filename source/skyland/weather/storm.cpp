////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "storm.hpp"
#include "number/random.hpp"
#include "skyland/latency.hpp"
#include "skyland/skyland.hpp"



namespace skyland::weather
{



static const int rain_pos_scale = 128;
static_assert(rain_pos_scale % 2 == 0);



Storm::Storm(int particle_count)
{
    auto& s = *state_;
    s.particle_count_ = particle_count;

    if (particle_count > State::particle_max) {
        Platform::fatal("logic error: particle_count");
    }

    auto& gen = rng::utility_state;
    s.thunder_timer_ = seconds(6) + rng::choice(seconds(5), gen);
    s.lightning_timer_ = seconds(7) + rng::choice(14, gen) * seconds(1);

    const auto scale = rain_pos_scale;

    for (int i = 0; i < s.particle_count_; ++i) {
        auto& rd = s.raindrops_[i];
        rd.x = rng::choice(240 * scale, rng::utility_state);
        rd.y = rng::choice(160 * scale, rng::utility_state);
    }
}



void Environment::on_pause()
{
    // ...
}



#define STATE_BYTES 7
#define MULT 0x13B /* for STATE_BYTES==6 only */
#define MULT_LO (MULT & 255)
#define MULT_HI (MULT & 256)

static uint8_t rand8(void)
{
    static uint8_t state[STATE_BYTES] = {
        0x87, 0xdd, 0xdc, 0x10, 0x35, 0xbc, 0x5c};
    static uint16_t c = 0x42;
    static int i = 0;
    uint16_t t;
    uint8_t x;

    x = state[i];
    t = (uint16_t)x * MULT_LO + c;
    c = t >> 8;
#if MULT_HI
    c += x;
#endif
    x = t & 255;
    state[i] = x;
    if (++i >= (int)sizeof(state))
        i = 0;
    return x;
}



State::State()
{
    for (auto& v : sine_x_input_) {
        v = rng::choice<std::numeric_limits<s16>::max()>(rng::utility_state);
    }
    for (auto& v : sine_y_input_) {
        v = rng::choice<std::numeric_limits<s16>::max()>(rng::utility_state);
    }
}



void State::display()
{
    using Buf = Buffer<Vec2<s32>, 64>;
    auto batch =
        allocate_dynamic_fast<Buf>("rain-spr-buffer", Buf::SkipZeroFill{});

    constexpr auto scale = rain_pos_scale;

    for (int i = 0; i < particle_count_; ++i) {
        auto& rd = raindrops_[i];
        batch->push_back({rd.x / scale, rd.y / scale});
    }

    Platform::Screen::SpriteBatchOptions opts;
    opts.position_absolute_ = true;
    opts.sz_ = Sprite::Size::w8_h8;

    PLATFORM.screen().draw_batch(spr_, *batch, opts);
}



void State::rewind(Time delta)
{
    const auto scale = rain_pos_scale;

    auto& gen = rng::utility_state;

    u8 sine_sub = 1;
    switch (APP.game_speed()) {
    default:
        break;

    case GameSpeed::rewind:
        sine_sub = 2;
        break;
    }
    for (auto& v : sine_x_input_) {
        v -= sine_sub;
    }
    for (auto& v : sine_y_input_) {
        v -= sine_sub;
    }

    for (int i = 0; i < particle_count_; ++i) {
        auto& rd = raindrops_[i];
        if ((rd.x / scale) > (s16)PLATFORM.screen().size().x or
            (rd.y / scale) < 0) {
            if (rng::choice<2>(rng::utility_state)) {
                rd.x = 0;
                rd.y = rng::choice(PLATFORM.screen().size().y * scale, gen);
            } else {
                rd.x = rng::choice(PLATFORM.screen().size().x * scale, gen);
                rd.y = PLATFORM.screen().size().y * scale;
            }
        } else {
            rd.x += (delta >> 6) + (delta >> 8);
            rd.y -= (delta >> 6) + (delta >> 8);

            if (delta) {
                rd.x -= sine8_signed(sine_x_input_[i] >> sine_damping_);
                rd.y -= sine8_signed(sine_y_input_[i] >> (sine_damping_ + 1));
            }
        }
    }
}



void State::update(Time delta)
{
    if (PLATFORM.screen().fade_active()) {
        return;
    }

    static constexpr const auto scale = rain_pos_scale;
    static_assert(scale % 2 == 0);

    auto camera = APP.camera()->center().cast<s16>();
    auto camera_diff_x = camera.x - last_camera_.x;
    auto camera_diff_y = camera.y - last_camera_.y;

    const u16 sd = delta;
#ifdef __GBA__
    const s16 sx = PLATFORM.screen().size().x + 24;
    const s16 sy = PLATFORM.screen().size().y;
#else
    const s16 sx = 240;
    const s16 sy = 160;
#endif

    int fixup = 0;

    u8 sine_add = 1;
    switch (APP.game_speed()) {
    default:
        break;

    case GameSpeed::stopped:
        sine_add = 0;
        break;

    case GameSpeed::fast:
        sine_add = 2;
        break;

    case GameSpeed::slow:
        if (sine_alt_) {
            sine_add = 0;
            sine_alt_ = false;
        } else {
            sine_alt_ = true;
        }
        break;
    }
    for (auto& v : sine_x_input_) {
        v += sine_add;
    }
    for (auto& v : sine_y_input_) {
        v += sine_add;
    }

    for (int i = 0; i < particle_count_; ++i) {
        auto& rd = raindrops_[i];
        if ((rd.x / scale) < 0 or (rd.y / scale) > sy or (rd.x / scale) > sx or
            (rd.y / scale) < -24) {

            if (fixup) {
                continue;
            }

            if (delta == 0) {
                rd.x = rand8() * scale;
                rd.y = rand8() * scale;
            } else {
                if (rand8() % 2) {
                    rd.x = PLATFORM.screen().size().x * scale;
                    rd.y = rand8() * scale;
                } else {
                    rd.x = rand8() * scale;
                    rd.y = 0;
                }
            }
            ++fixup;
        } else {
            rd.x -= (sd >> 6) + (sd >> 8);
            rd.y += (sd >> 6) + (sd >> 8);

            if (delta) {
                rd.x += sine8_signed(sine_x_input_[i] >> sine_damping_);
                rd.y += sine8_signed(sine_y_input_[i] >> (sine_damping_ + 1));
            }

            rd.x -= camera_diff_x * (scale + 48);
            rd.y -= camera_diff_y * (scale + 48);
        }
    }

    if (camera_diff_x != 0 or camera_diff_y != 0) {
        last_camera_ = camera;
    }
}



void Storm::update(Time delta)
{
    if (PLATFORM.screen().fade_active()) {
        return;
    }

    auto& gen = rng::utility_state;

    auto& s = *state_;

    s.thunder_timer_ -= delta;
    if (s.thunder_timer_ <= 0) {
        s.thunder_timer_ = seconds(8) + rng::choice(seconds(25), gen);
        if (rng::choice<2>(gen)) {
            PLATFORM.speaker().play_sound("thunder_1", 0);
        } else {
            PLATFORM.speaker().play_sound("thunder_2", 0);
        }
    }

    s.lightning_timer_ -= delta;
    if (s.lightning_timer_ <= 0) {
        s.lightning_timer_ = seconds(4) + rng::choice(13, gen) * seconds(1);

        if (APP.opponent_island() and
            not APP.opponent_island()->is_destroyed() and
            not APP.player_island().is_destroyed()) {
            on_lightning();
        }
    }

    s.update(delta);
}



void Storm::rewind(Time delta)
{
    auto& s = *state_;

    s.rewind(delta);
}



void Storm::display()
{
    if (PLATFORM.screen().fade_active()) {
        return;
    }

    auto& s = *state_;
    s.display();
}



Platform::Screen::Shader Storm::shader() const
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
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
                if (not player_island().interior_visible()) {
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
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
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
            case 5:
                return custom_color(0x81c7c3);
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



ColorConstant Storm::fadein_colorize_tone() const
{
    return custom_color(0x0a2742);
}



EnvironmentId Storm::id() const
{
    return 3;
}



} // namespace skyland::weather
