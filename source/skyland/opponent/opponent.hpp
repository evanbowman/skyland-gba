#pragma once


#include "number/numeric.hpp"
#include "skyland/player.hpp"



class Platform;



namespace skyland {



class App;



class Opponent : public Player {
public:
    virtual ~Opponent()
    {
    }
    virtual void update(Platform&, App&, Microseconds delta) = 0;
};



} // namespace skyland
