#pragma once

#include "timeStreamHeader.hpp"



namespace skyland::time_stream::event {



enum Type : u8 {
    player_room_created,
    opponent_room_created,
    player_room_destroyed,
    opponent_room_destroyed,
    player_cannonball_destroyed,
    opponent_cannonball_destroyed,
};



struct PlayerRoomCreated {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;

    static constexpr const auto t = Type::player_room_created;
};



struct OpponentRoomCreated {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;

    static constexpr const auto t = Type::opponent_room_created;
};



struct PlayerRoomDestroyed {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;
    u8 type_;

    static constexpr const auto t = Type::player_room_destroyed;
};



struct OpponentRoomDestroyed {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;
    u8 type_;

    static constexpr const auto t = Type::opponent_room_destroyed;
};



struct CannonballDestroyed {
    Header header_;
    u8 x_origin_ : 4;
    u8 y_origin_ : 4;
    host_s32 timer_;
    host_s16 x_pos_;
    host_s16 y_pos_;
    u8 x_speed_[sizeof(Float)];
    u8 y_speed_[sizeof(Float)];
};



struct PlayerCannonballDestroyed : CannonballDestroyed {
    static constexpr const auto t = Type::player_cannonball_destroyed;
};



struct OpponentCannonballDestroyed : CannonballDestroyed {
    static constexpr const auto t = Type::player_cannonball_destroyed;
};



}
