#pragma once

#include "timeStreamHeader.hpp"
#include "coins.hpp"



// At first, you might question why we have both time_stream::event and
// network::event, when there might seem to be a lot of overlap between the
// two. But with rewind functionality, you care about the state before the
// current state, and with network sync, you care about the current state, so
// stuff can in some cases be stored more compactly in some cases when you only
// need to worry about storing the previous state. Furthermore, some events
// necessary for rewind are unnecessary for multiplayer.



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

    player_ionburst_destroyed,
    opponent_ionburst_destroyed,

    player_room_damaged,
    opponent_room_damaged,

    player_room_repaired,
    opponent_room_repaired,

    coins_changed,

    character_moved,
    character_died,
    // character_health_changed,
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



struct PlayerIonBurstDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::player_ionburst_destroyed;
};



struct OpponentIonBurstDestroyed : BasicProjectileDestroyed {
    static constexpr const auto t = Type::opponent_ionburst_destroyed;
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



struct CoinsChanged {
    Header header_;
    HostInteger<u32> previous_value_;

    static constexpr const auto t = Type::coins_changed;
};



struct CharacterMoved {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;
    u8 previous_x_ : 4;
    u8 previous_y_ : 4;
    u8 owned_by_player_ : 1;
    u8 near_ : 1;
    u8 unused_ : 6;

    static constexpr const auto t = Type::character_moved;
};



struct CharacterDied {
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;
    u8 owned_by_player_ : 1;
    u8 near_ : 1;
    u8 is_replicant_ : 1;
    u8 unused_ : 5;

    static constexpr const auto t = Type::character_died;
};



// struct CharacterHealthChanged {
//     Header header_;
//     u8 x_ : 4;
//     u8 y_ : 4;
//     u8 owned_by_player_ : 1;
//     u8 near_ : 1;
//     u8 unused_ : 6;
//     host_u16 health_;
// };



// struct CharacterTransported {
//     Header header_;
//     // TODO...
// };



}
