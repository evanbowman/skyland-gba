#pragma once


#include "opponent.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class Cannon;
class MissileSilo;



class FriendlyAI : public Opponent {
public:
    void update(Platform&, App&, Microseconds delta) override
    {
        // TODO...
    }


    void on_room_damaged(Platform&, App& app, Room&) override
    {
        // What!? The player attacked us! We're no longer a friendly AI.
        app.swap_ai<EnemyAI>();
    }
};



} // namespace skyland
