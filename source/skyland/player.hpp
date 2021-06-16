#pragma once



class Platform;



namespace skyland {



class Room;



class Player {
public:
    virtual ~Player() {}


    virtual void on_room_damaged(Platform& pfrm, Room& room) {}
};



}
