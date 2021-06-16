#pragma once



class Platform;



namespace skyland {



class Room;
class App;



using MissileAmmo = short int;



class Player {
public:
    virtual ~Player() {}


    virtual void on_room_damaged(Platform& pfrm, App& app, Room& room) {}


    MissileAmmo& missile_ammo()
    {
        return missile_ammo_;
    }


private:
    MissileAmmo missile_ammo_;
};



}
