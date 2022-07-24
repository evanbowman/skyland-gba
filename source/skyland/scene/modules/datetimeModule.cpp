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


#include "datetimeModule.hpp"
#include "skyland/scene/titleScreenScene.hpp"


namespace skyland
{



void DatetimeModule::repaint(Platform& pfrm)
{
    auto draw_couplet =
        [&](int x, int val) {
            if (val < 10) {
                draw_image(pfrm, 118 + 0, x, 1, 4, 6, Layer::overlay);
                draw_image(pfrm, 118 + val * 24, x + 4, 1, 4, 6, Layer::overlay);
            } else {
                int tens = val / 10;
                int ones = val % 10;
                draw_image(pfrm, 118 + tens * 24, x, 1, 4, 6, Layer::overlay);
                draw_image(pfrm, 118 + ones * 24, x + 4, 1, 4, 6, Layer::overlay);
            }
        };

    draw_image(pfrm, 94, 8, 1, 4, 6, Layer::overlay);
    draw_image(pfrm, 94, 18, 1, 4, 6, Layer::overlay);

    draw_couplet(1, dt_.hour_);
    draw_couplet(11, dt_.minute_);
    draw_couplet(21, dt_.second_);
}



void DatetimeModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (auto tm = pfrm.system_clock().now()) {
        dt_ = *tm;
    }

    pfrm.screen().schedule_fade(0.95f);
    pfrm.screen().schedule_fade(1.f);

    repaint(pfrm);

    pfrm.load_overlay_texture("overlay_large_numeral");
}



void DatetimeModule::exit(Platform& pfrm, App& app, Scene& next)
{

}



ScenePtr<Scene>
DatetimeModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (key_down<Key::action_2>(pfrm)) {
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    return null_scene();
}



// DatetimeModule::Factory DatetimeModule::factory_;



}
