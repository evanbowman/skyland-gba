#pragma once


#include "player.hpp"



namespace skyland {



class PlayerP1 : public Player {
public:
    void update(Platform&, App&, Microseconds delta) override
    {

    }


    void on_room_destroyed(Platform& pfrm, App& app, Room& room) override;


};



}
