#pragma once


#include "opponent.hpp"
#include "skyland/scene/scriptHookScene.hpp"
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


    void on_room_damaged(Platform& pfrm, App& app, Room&) override
    {
        // What!? The player attacked us! We're no longer a friendly AI.
        app.swap_ai<EnemyAI>();

        invoke_hook(pfrm, "hostile-transition-hook");
    }
};



} // namespace skyland
