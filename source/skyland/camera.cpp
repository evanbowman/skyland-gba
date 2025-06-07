////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "camera.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static const int view_y_min = -80;



void Camera::update(Island& target,
                    const RoomCoord& cursor_loc,
                    Time delta,
                    bool near)
{
    auto view = PLATFORM.screen().get_view();



    int base_offset;
    if (PLATFORM.screen().size().y == 160) {
        base_offset = 15;
    } else {
        base_offset = PLATFORM.screen().size().y / 12;
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

    int x = cursor_loc.x;
    if (x == 255) {
        // FIXME: Late in development, I decided I wanted players to be able to
        // build terrain on both sides of an island, requiring the player to be
        // able to move cursor location to x index -1. The ConstructionScene now
        // uses an s8 cursor index, and the rest of the rest of the codebase
        // uses a u8 index.
        x = -1;
    }

    if (near) {
        target_.x = tpos.x + ((x - 3) * 16) / 2;
        target_.x = clamp(target_.x, (int)tpos.x - 40, (int)tpos.x + 48);
        target_.x -= 16;
    } else {
        target_.x = tpos.x + ((x + 3) * 16) / 2;
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

    PLATFORM.screen().set_view(view);
}



void Camera::shake(int magnitude)
{
    if (shake_magnitude_ == 0 or shake_magnitude_ <= magnitude) {
        shake_magnitude_ = magnitude;
        shake_timer_ = 0;
    }
}



} // namespace skyland
