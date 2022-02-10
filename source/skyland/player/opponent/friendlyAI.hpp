#pragma once


#include "opponent.hpp"



namespace skyland {



class Cannon;
class MissileSilo;



class FriendlyAI : public Opponent {
public:
    void update(Platform&, App&, Microseconds delta) override
    {
        // TODO...
    }


    void on_room_damaged(Platform& pfrm, App& app, Room&) override;

};



} // namespace skyland
