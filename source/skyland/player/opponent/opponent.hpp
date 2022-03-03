#pragma once


#include "skyland/player/player.hpp"



class Platform;



namespace skyland {



class App;



class Opponent : public Player
{
public:
    virtual ~Opponent()
    {
    }

    void on_room_damaged(Platform& pfrm, App& app, Room&) override;
};



} // namespace skyland
