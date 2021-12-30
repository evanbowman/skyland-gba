#include "drone.hpp"
#include "platform/platform.hpp"
#include "skyland/room.hpp"
#include "skyland/island.hpp"



namespace skyland {



void Drone::update(Platform& pfrm, App&, Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_pos_.x * 16;
    o.y += grid_pos_.y * 16;
    sprite_.set_position(o);
}



}
