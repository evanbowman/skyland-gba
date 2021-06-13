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
                Microseconds delta);


private:
    Vec2<Float> target_;
    Vec2<Float> current_;
};




}
