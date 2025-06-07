////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "solarStorm.hpp"
#include "skyland/entity/misc/lightningStrike.hpp"
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
    auto last_ignite_tm = ignite_timer_;
    ignite_timer_ += delta;

    const auto ignite_warning_time = seconds(3);
    const auto ignite_time = seconds(26);
    const auto warn_at = ignite_time - ignite_warning_time;

    if (ignite_timer_ > warn_at and last_ignite_tm <= warn_at) {
        PLATFORM.speaker().play_sound("warning_alarm.raw", 7);
    }

    if (ignite_timer_ > ignite_time) {
        ignite_timer_ -= ignite_time;

        if (auto l = APP.alloc_entity<LightningStrike>()) {
            PLATFORM.speaker().play_sound("thunder_close_1", 5);
            APP.effects().push(std::move(l));

            for (auto& room : player_island().rooms()) {
                room->on_lightning();
            }

            if (opponent_island()) {
                for (auto& room : opponent_island()->rooms()) {
                    room->on_lightning();
                }
            }

            auto start_random_fire = [&](auto& isle) {
                if (isle.phase()) {
                    return;
                }
                Vector<Room*> hr;
                for (auto& r : isle.rooms()) {
                    auto prop = (*r->metaclass())->properties();
                    if (prop & RoomProperties::habitable) {
                        hr.push_back(r.get());
                    }
                }
                if (hr.size() == 0) {
                    return;
                }
                int idx = rng::choice(hr.size(), rng::critical_state);
                auto r = hr[idx];
                auto p = r->position();
                p.y += r->size().y - 1;
                isle.fire_create(p);
            };

            start_random_fire(APP.player_island());
            start_random_fire(APP.player_island());

            APP.with_opponent_island([&](auto& isle) {
                start_random_fire(isle);
                start_random_fire(isle);
            });
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
                return custom_color(0x5254c4);
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
                return custom_color(0x5254c4);
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
            switch (index) {
            case 1:
                return custom_color(0xe33314);
            case 8:
                return custom_color(0x736373);

            case 10:
                return custom_color(0x5252c6);

            case 11:
                return custom_color(0xbdb5ad);

            case 12:
                return custom_color(0x393952);

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
