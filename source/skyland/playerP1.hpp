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


private:
    Microseconds last_key_ = 0;


    Microseconds key_held_timers_[static_cast<int>(Key::count)];
};



} // namespace skyland
