#pragma once


#include "player.hpp"



namespace skyland {



// An implementation of Player, controlled directly by a player via the device's
// physical buttons.



class PlayerP1 : public Player {
public:
    void update(Platform&, App&, Microseconds delta) override;


    void on_room_destroyed(Platform& pfrm, App& app, Room& room) override;


    void on_room_plundered(Platform& pfrm, App& app, Room& room) override;


    bool key_down(Platform&, Key k) override;


    bool key_up(Platform&, Key k) override;


    bool key_pressed(Platform&, Key k) override;


    bool key_held(Key k, Microseconds duration) override;


    void key_held_reset(Key k, Microseconds decrement) override;


    void key_held_distribute(Platform& pfrm) override;


    std::optional<std::tuple<Vec2<u32>, Microseconds>>
    touch_released(Platform& pfrm) override;


    std::optional<Vec2<u32>> touch_current(Platform& pfrm) override;


    bool touch_held(Microseconds duration) override;


    void touch_consume() override;


private:
    Microseconds last_key_ = 0;

    Microseconds touch_held_time_ = 0;
    Microseconds last_touch_held_time_ = 0;

    bool touch_invalidate_ = false;
    Vec2<u32> last_touch_;
    Vec2<Float> touch_velocity_;

    Microseconds key_held_timers_[static_cast<int>(Key::count)];
};



} // namespace skyland
