#pragma once

#include "number/numeric.hpp"



class Platform;



namespace skyland {



class Island;



class Camera {
public:
    void update(Platform& pfrm,
                Island& target,
                const Vec2<u8>& cursor_loc,
                Microseconds delta,
                bool near);



    void shake(int magnitude = 12);


    bool is_shaking() const
    {
        return shake_magnitude_;
    }


    void reset()
    {
        target_ = {};
        current_ = {};
    }


private:
    Vec2<int> target_;
    Vec2<Float> current_;

    Microseconds shake_timer_ = 0;
    int shake_magnitude_ = 0;
};



} // namespace skyland
