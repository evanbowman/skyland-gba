#include "drone.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/room.hpp"



namespace skyland {



static Vec2<Float> calc_pos(Island* island, const Vec2<u8>& grid_coord)
{
    auto o = island->origin();
    o.x += grid_coord.x * 16;
    o.y += grid_coord.y * 16;
    return o;
}



Drone::Drone(Island* parent,
             Island* destination,
             const Vec2<u8>& grid_pos)
    : Entity({{10, 10}, {8, 8}}), parent_(parent),
      destination_(destination),
      grid_pos_({grid_pos.x, u8(grid_pos.y)})
{
    sprite_.set_texture_index(64);
    sprite_.set_size(Sprite::Size::w16_h32);

    health_ = 40;

    auto o = calc_pos(parent_, grid_pos);
    sprite_.set_position(o);
    anchor_ = o;
}



void Drone::update(Platform& pfrm, App&, Microseconds delta)
{
    switch (state_) {
    case State::launch: {
        static const auto launch_duration = seconds(1);
        timer_ += delta;
        if (timer_ >= launch_duration) {
            timer_ = 0;
            state_ = State::ready;
            grid_pos_ = movement_target_;
            break;
        }
        auto amount = smoothstep(0.f, launch_duration, timer_);
        auto dest = calc_pos(destination_, movement_target_);
        auto pos = interpolate(dest, anchor_, amount);
        sprite_.set_position(pos);
        break;
    }

    case State::ready:
        auto o = calc_pos(destination_, grid_pos_);
        sprite_.set_position(o);
        break;
    }
}



} // namespace skyland
