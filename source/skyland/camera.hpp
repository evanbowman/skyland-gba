#pragma once

#include "number/numeric.hpp"



class Platform;



namespace skyland {



class App;
class Island;



class Camera {
public:

    virtual ~Camera() {}


    virtual void update(Platform& pfrm,
                        App& app,
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


    virtual bool always_update(Platform& pfrm)
    {
        return false;
    }


    virtual void reset_default(App& app)
    {
    }


protected:
    Vec2<int> target_;
    Vec2<Float> current_;

    Microseconds shake_timer_ = 0;
    int shake_magnitude_ = 0;
};



} // namespace skyland
