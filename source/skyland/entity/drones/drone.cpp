#include "drone.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/room.hpp"
#include "skyland/skyland.hpp"
#include "droneMeta.hpp"



namespace skyland {



static Vec2<Float> calc_pos(Island* island, const Vec2<u8>& grid_coord)
{
    auto o = island->origin();
    o.x += grid_coord.x * 16;
    o.y += grid_coord.y * 16;
    return o;
}



Drone::Drone(const char* name,
             Island* parent,
             Island* destination,
             const Vec2<u8>& grid_pos)
    : Entity({{16, 16}, {0, 0}}), parent_(parent), destination_(destination),
      grid_pos_({grid_pos.x, u8(grid_pos.y)})
{
    sprite_.set_texture_index(64);
    sprite_.set_size(Sprite::Size::w16_h32);

    health_ = 40;

    auto o = calc_pos(parent_, grid_pos);
    sprite_.set_position(o);
    anchor_ = o.cast<s16>();

    target_near_ = false;

    metaclass_index_ = DroneMeta::index(name);
}



void Drone::set_movement_target(const Vec2<u8>& position)
{
    auto old_pos = grid_pos_;
    grid_pos_ = position;

    auto v1 = calc_pos(parent_, old_pos);
    auto v2 = calc_pos(destination_, grid_pos_);

    auto dist = distance(v1, v2);

    auto speed = 0.0002f;

    duration_ = dist / speed;
}



void Drone::update_sprite(App& app)
{
    auto o = calc_pos(destination_, grid_pos_);

    const Float offset = 3 *
                         float(sine(4 * 3.14f * 0.0005f * duration_ + 180)) /
                         std::numeric_limits<s16>::max();

    o.y += offset;

    sprite_.set_position(o);

    if (target_) {
        if (destination() == &*app.opponent_island() and target_near_) {
            sprite_.set_flip({true, false});
        } else if (destination() == &app.player_island() and not target_near_) {
            sprite_.set_flip({false, false});
        } else {
            if (target_->x < grid_pos_.x) {
                sprite_.set_flip({true, false});
            } else {
                sprite_.set_flip({false, false});
            }
        }
    }
}



void Drone::update(Platform& pfrm, App& app, Microseconds delta)
{
    switch (state_) {
    case State::launch: {
        timer_ += delta;
        if (timer_ >= duration_) {
            timer_ = 0;
            state_ = State::ready;
            break;
        }
        auto amount = smoothstep(0.f, duration_, timer_);
        auto dest = calc_pos(destination_, grid_pos_);
        auto pos = interpolate(dest, anchor_.cast<Float>(), amount);
        sprite_.set_position(pos);

        if (parent() not_eq &app.player_island()) {
            sprite_.set_flip({true, false});
        }
        break;
    }

    case State::ready:
        update_sprite(app);
        break;
    }
}



} // namespace skyland
