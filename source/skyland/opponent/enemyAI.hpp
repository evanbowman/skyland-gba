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
    void
    set_target(Platform&, App&, const u8 matrix[16][16], MissileSilo& silo);


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;


    Coins coins_ = 0;
};



} // namespace skyland
