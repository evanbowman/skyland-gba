#include "camera.hpp"
#include "platform/platform.hpp"
#include "island.hpp"



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
    target_.y = clamp(target_.y, -40.f, 0.f);

    // Ok, so, about the x-anchoring for the camera. If we're the near camera,
    // we want the camera to be weighted slightly to the right, so we can get a
    // glimpse of enemy castles. But when the camera is locked on an enemy
    // castle, we want to shift the camera slightly to the left, otherwise,
    // you're just looking at a bunch of empty space, especially early on, while
    // enemy castles are small.

    if (near) {
        target_.x = target.get_position().x + ((cursor_loc.x - 3) * 16) / 2;
        target_.x = clamp(target_.x, target.get_position().x -40, target.get_position().x + 48);
        target_.x -= 16;
    } else {
        target_.x = target.get_position().x + ((cursor_loc.x + 3) * 16) / 2;
        target_.x = clamp(target_.x, target.get_position().x -48, target.get_position().x + 256);
        target_.x -= 100;
    }


    current_ = interpolate(
            target_,
            current_,
            delta * 0.0000081f);



    view.set_center(current_);
    pfrm.screen().set_view(view);

}



}
