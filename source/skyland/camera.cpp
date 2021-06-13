#include "camera.hpp"
#include "platform/platform.hpp"



namespace skyland {



void Camera::update(Platform& pfrm,
                    Island& target,
                    const Vec2<u8>& cursor_loc,
                    Microseconds delta)
{
    auto view = pfrm.screen().get_view();
    auto center = view.get_center();

    target_.y = (-((15 - (cursor_loc.y + 1)) * 16) / 2);
    target_.y = clamp(target_.y, -40.f, 0.f);

    target_.x = center.x;


    current_ = interpolate(
            target_,
            current_,
            delta * 0.0000081f);



    view.set_center(current_);
    pfrm.screen().set_view(view);

}



}
