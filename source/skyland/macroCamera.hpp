#pragma once

#include "camera.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



class Camera : public ::skyland::Camera
{
private:
    u8 ambient_movement_ = 0;
    Time timer_ = 0;

public:
    Camera()
    {
        current_ = {0, 40.f};
        PLATFORM.set_scroll(Layer::map_0, current_.x, current_.y);
        PLATFORM.set_scroll(Layer::map_1, current_.x, current_.y + 8);
    }


    void update(Island&, const RoomCoord&, Time delta, bool checkers_mode)
    {
        if (not APP.macrocosm()) {
            return;
        }

        auto& m = macrocosm();

        timer_ += milliseconds(16);
        u8 ambient_offset = 2 *
                            float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
                            std::numeric_limits<s16>::max();

        ambient_movement_ = ambient_offset;
        if (checkers_mode) { // Fixme
            ambient_movement_ = 0;
        }

        if (m.sector().size().x < 6) {
            // For outposts, which are tiny:
            PLATFORM.set_scroll(Layer::map_0, current_.x, 48);
            PLATFORM.set_scroll(Layer::map_1, current_.x, 48 + 8);
            return;
        }

        auto p = m.sector().cursor_raster_pos();

        int y = p / 30;
        int real_y = y * 8;

        //target_.x = -x * 4;
        target_.y = -24 + y * 4;
        if (macrocosm().sector().size().x == 14) {
            target_.y = clamp(target_.y, 0, 100);
        } else if (macrocosm().sector().size().x == 8) {
            target_.y = clamp(target_.y, 0, 40);
        } else if (macrocosm().sector().size().x == 6) {
            target_.y = clamp(target_.y, 0, 80);
        } else {
            target_.y = clamp(target_.y, 0, 80);
        }


        if (real_y < (current_.y + 10) or real_y > current_.y + 120) {
            current_ = interpolate(
                target_.cast<Float>(), current_, delta * 0.0000021f);
        }


        int shake_offset = 0;

        if (shake_magnitude_ not_eq 0) {
            shake_timer_ += delta;

            const auto shake_duration = milliseconds(250);

            if (shake_timer_ > shake_duration) {
                shake_timer_ = 0;
                shake_magnitude_ = 0;
            }

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

            shake_offset = offset;
        }

        PLATFORM.set_scroll(Layer::map_0,
                            current_.x,
                            current_.y + ambient_movement_ + shake_offset);

        PLATFORM.set_scroll(Layer::map_1,
                            current_.x,
                            current_.y + 8 + ambient_movement_ + shake_offset);
    }
};



} // namespace skyland::macro
