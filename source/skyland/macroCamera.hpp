#pragma once

#include "camera.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



class Camera : public ::skyland::Camera
{
public:
    Camera(Platform& pfrm)
    {
        current_ = {0, 40.f};
        pfrm.set_scroll(Layer::map_0, current_.x, current_.y);
        pfrm.set_scroll(Layer::map_1, current_.x, current_.y + 8);
    }


    void update(Platform& pfrm,
                App& app,
                Island&,
                const RoomCoord&,
                Microseconds delta,
                bool)
    {
        if (not app.macrocosm()) {
            return;
        }

        auto& m = macrocosm(app);

        if (m.sector().size().x < 6) {
            // For outposts, which are tiny:
            pfrm.set_scroll(Layer::map_0, current_.x, 48);
            pfrm.set_scroll(Layer::map_1, current_.x, 48 + 8);
            return;
        }

        auto p = m.sector().cursor_raster_pos();

        int y = p / 30;
        int real_y = y * 8;

        //target_.x = -x * 4;
        target_.y = -24 + y * 4;
        if (macrocosm(app).sector().size().x == 8) {
            target_.y = clamp(target_.y, 0, 40);
        } else if (macrocosm(app).sector().size().x == 6) {
            target_.y = clamp(target_.y, 0, 80);
        } else {
            target_.y = clamp(target_.y, 0, 80);
        }


        if (real_y < (current_.y + 10) or real_y > current_.y + 120) {
            current_ = interpolate(
                target_.cast<Float>(), current_, delta * 0.0000021f);

            pfrm.set_scroll(Layer::map_0, current_.x, current_.y);
            pfrm.set_scroll(Layer::map_1, current_.x, current_.y + 8);
        }
    }
};



} // namespace skyland::macro
