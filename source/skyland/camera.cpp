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


#include "camera.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const int view_y_min = -80;



void Camera::update(Platform& pfrm,
                    App& app,
                    Island& target,
                    const Vec2<u8>& cursor_loc,
                    Microseconds delta,
                    bool near)
{
    auto view = pfrm.screen().get_view();



    int base_offset;
    if (pfrm.screen().size().y == 160) {
        base_offset = 15;
    } else {
        base_offset = pfrm.screen().size().y / 12;
    }

    target_.y = (-((base_offset - (cursor_loc.y + 1)) * 16) / 2);
    target_.y = clamp(target_.y, view_y_min, 0);

    // Ok, so, about the x-anchoring for the camera. If we're the near camera,
    // we want the camera to be weighted slightly to the right, so we can get a
    // glimpse of enemy castles. But when the camera is locked on an enemy
    // castle, we want to shift the camera slightly to the left, otherwise,
    // you're just looking at a bunch of empty space, especially early on, while
    // enemy castles are small.

    auto tpos = fvec(target.get_position());

    if (near) {
        target_.x = tpos.x + ((cursor_loc.x - 3) * 16) / 2;
        target_.x = clamp(target_.x, (int)tpos.x - 40, (int)tpos.x + 48);
        target_.x -= 16;
    } else {
        target_.x = tpos.x + ((cursor_loc.x + 3) * 16) / 2;
        target_.x = clamp(target_.x, (int)tpos.x - 48, (int)tpos.x + 256);
        target_.x -= 100;
    }


    current_ = interpolate(target_.cast<Float>(), current_, delta * 0.0000081f);


    if (shake_magnitude_ not_eq 0) {
        shake_timer_ += delta;

        const auto shake_duration = milliseconds(250);

        if (shake_timer_ > shake_duration) {
            shake_timer_ = 0;
            shake_magnitude_ = 0;
        }

        // Exponents are too expensive, so we aren't doing true damping...
        const auto damping =
            ease_out(shake_timer_, 0, shake_magnitude_ / 2, shake_duration);

        auto offset = shake_magnitude_ * (float(cosine(shake_timer_ / 4)) /
                                          std::numeric_limits<s16>::max());

        if (offset > 0) {
            offset -= damping;
            if (offset < 0) {
                offset = 0;
            }
        } else {
            offset += damping;
            if (offset > 0) {
                offset = 0;
            }
        }

        view.set_center(
            {current_.x, clamp(current_.y + offset, (Float)view_y_min, 0.f)});
    } else {
        view.set_center(current_);
    }

    pfrm.screen().set_view(view);
}



void Camera::shake(int magnitude)
{
    if (shake_magnitude_ == 0 or shake_magnitude_ <= magnitude) {
        shake_magnitude_ = magnitude;
        shake_timer_ = 0;
    }
}



} // namespace skyland
