#pragma once


#include "opponent.hpp"
#include "skyland/coins.hpp"



namespace skyland {



class Cannon;
class MissileSilo;



class EnemyAI : public Opponent {
public:


    void update(Platform&, App&, Microseconds delta) override;


    void on_room_damaged(Platform&, App& app, Room&) override;


    void add_coins(Coins value)
    {
        coins_ += value;
    }


private:

    void set_target(Platform&, App&, const u8 matrix[16][16], Cannon& cannon);
    void set_target(Platform&, App&, const u8 matrix[16][16], MissileSilo& silo);


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;


    // Give the player some time to attempt to take out the AI's missile
    // launchers, or to build defenses, before attempting to attack.
    Microseconds next_missile_launch_ = seconds(25);
    Microseconds last_missile_launch_ = 0;


    Coins coins_ = 0;
};



}
