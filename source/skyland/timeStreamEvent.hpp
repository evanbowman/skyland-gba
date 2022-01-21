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

    player_arcbolt_destroyed,
    opponent_arcbolt_destroyed,

    player_flak_destroyed,
    opponent_flak_destroyed,

    player_room_damaged,
    opponent_room_damaged,

    player_room_repaired,
    opponent_room_repaired,
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



struct BasicProjectileDestroyed {
    Header header_;
    u8 x_origin_ : 4;
    u8 y_origin_ : 4;
    host_s32 timer_;
    host_s16 x_pos_;
    host_s16 y_pos_;
    u8 x_speed_[sizeof(Float)];
    u8 y_speed_[sizeof(Float)];
};



struct PlayerCannonballDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::player_cannonball_destroyed;
};



struct OpponentCannonballDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::player_cannonball_destroyed;
};



struct PlayerArcboltDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::player_arcbolt_destroyed;
};



struct OpponentArcboltDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::player_arcbolt_destroyed;
};



struct PlayerFlakDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::player_flak_destroyed;
};



struct OpponentFlakDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::opponent_flak_destroyed;
};



struct RoomHealthChanged {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;
    // The whole point of this timestream stuff is for implementing rewind, so
    // we only care about what the health was before it changed, to execute a
    // revert.
    host_u16 previous_health_;
};



struct PlayerRoomDamaged : RoomHealthChanged {
    static constexpr const auto t = Type::player_room_damaged;
};



struct OpponentRoomDamaged : RoomHealthChanged {
    static constexpr const auto t = Type::opponent_room_damaged;
};



struct PlayerRoomRepaired : RoomHealthChanged {
    static constexpr const auto t = Type::player_room_repaired;
};



struct OpponentRoomRepaired : RoomHealthChanged {
    static constexpr const auto t = Type::opponent_room_repaired;
};



}
