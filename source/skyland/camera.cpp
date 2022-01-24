#include "camera.hpp"
#include "island.hpp"
#include "platform/platform.hpp"



namespace skyland {



void Camera::update(Platform& pfrm,
                    Island& target,
                    const Vec2<u8>& cursor_loc,
                    Microseconds delta,
                    bool near)
{
    auto view = pfrm.screen().get_view();
    // auto center = view.get_center();

    target_.y = (-((15 - (cursor_loc.y + 1)) * 16) / 2);
    target_.y = clamp(target_.y, -44, 0);

    // Ok, so, about the x-anchoring for the camera. If we're the near camera,
    // we want the camera to be weighted slightly to the right, so we can get a
    // glimpse of enemy castles. But when the camera is locked on an enemy
    // castle, we want to shift the camera slightly to the left, otherwise,
    // you're just looking at a bunch of empty space, especially early on, while
    // enemy castles are small.

    if (near) {
        target_.x = target.get_position().x + ((cursor_loc.x - 3) * 16) / 2;
        target_.x = clamp(target_.x,
                          (int)target.get_position().x - 40,
                          (int)target.get_position().x + 48);
        target_.x -= 16;
    } else {
        target_.x = target.get_position().x + ((cursor_loc.x + 3) * 16) / 2;
        target_.x = clamp(target_.x,
                          (int)target.get_position().x - 48,
                          (int)target.get_position().x + 256);
        target_.x -= 125;
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

        view.set_center({current_.x, clamp(current_.y + offset, -44.f, 0.f)});
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
