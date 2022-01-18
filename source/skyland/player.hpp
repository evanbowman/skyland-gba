#pragma once


#include "number/numeric.hpp"
#include "platform/key.hpp"



class Platform;



namespace skyland {



class Room;
class App;



using MissileAmmo = short int;



class Player {
public:
    virtual ~Player()
    {
    }


    virtual void update(Platform&, App&, Microseconds delta)
    {
    }


    virtual void on_room_damaged(Platform& pfrm, App& app, Room& room)
    {
    }


    virtual void on_room_destroyed(Platform& pfrm, App& app, Room& room)
    {
    }


    virtual void on_room_plundered(Platform& pfrm, App& app, Room& room)
    {
    }


    virtual bool key_up(Platform&, Key k)
    {
        return false;
    }


    virtual bool key_down(Platform&, Key k)
    {
        return false;
    }


    virtual bool key_pressed(Platform&, Key k)
    {
        return false;
    }


    // key_held and key_held_reset should implement some sort of timer-based key
    // states.
    virtual bool key_held(Key k, Microseconds duration)
    {
        return false;
    }


    virtual void key_held_reset(Key k, Microseconds decrement)
    {
    }


    bool test_key(Platform& pfrm,
                  Key k,
                  Microseconds held_time,
                  Microseconds held_decrement)
    {
        if (key_down(pfrm, k) or key_held(k, held_time)) {
            key_held_reset(k, held_decrement);
            return true;
        }
        return false;
    }


    int rooms_lost_ = 0;
    int rooms_built_ = 0;


    MissileAmmo& missile_ammo()
    {
        return missile_ammo_;
    }


private:
    MissileAmmo missile_ammo_;
};



} // namespace skyland
