#pragma once


#include "opponent.hpp"



namespace skyland {



class Cannon;



class EnemyAI : public Opponent {
public:


    void update(Platform&, App&, Microseconds delta) override;


    void on_room_damaged(Platform&, Room&) override;


private:

    void set_cannon_target(Platform&, App&, Cannon& cannon);


    static const auto next_action_timeout = seconds(1);

    Microseconds next_action_timer_ = next_action_timeout;
};



}
