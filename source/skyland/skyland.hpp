#pragma once

#include "island.hpp"
#include "platform/platform.hpp"


namespace skyland {


class App {
public:
    App(Platform& pfrm);

    void update(Platform& pfrm, Microseconds delta);
    void render(Platform& pfrm);

private:
    Island player_;
    Float cloud_scroll_1_;
    Float cloud_scroll_2_;
};


}
