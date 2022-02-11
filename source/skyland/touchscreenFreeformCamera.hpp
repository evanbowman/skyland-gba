#pragma once

#include "camera.hpp"



class Platform;



namespace skyland {



class TouchscreenFreeformCamera : public Camera {
public:


    void update(Platform& pfrm,
                App& app,
                Island& target,
                const Vec2<u8>& cursor_loc,
                Microseconds delta,
                bool near) override;



    bool always_update(Platform& pfrm) override
    {
        return true;
    }


    void reset_default(App& app) override;


private:
    std::optional<Vec2<u32>> scroll_locus_;
    Vec2<Float> view_center_;
};




}
