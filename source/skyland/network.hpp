#pragma once


#include "number/endian.hpp"
#include "number/int.h"
#include "room_metatable.hpp"



namespace skyland {
namespace network {
namespace packet {



struct Header {
    enum MessageType : u8 {
        null,
        room_constructed,
        terrain_constructed,
        room_salvaged,
        weapon_set_target,
        character_set_target,
        room_destroyed,
        character_boarded,
        character_disembark,
        character_died,
        replicant_created,
        bulkhead_changed,
        program_version,
        block_transfer_start,
        block_transfer,
        drone_set_target,
        drone_spawn,
        drone_destroyed,
        game_match_parameter_update,
        game_match_settings_cursor,
        game_match_begin,
        heartbeat,
        dynamite_activated,
        game_mode_selected,
    } message_type_;
};
static_assert(sizeof(Header) == 1);



#define NET_EVENT_SIZE_CHECK(TYPE)                                             \
    static_assert(sizeof(TYPE) == Platform::NetworkPeer::max_message_size);



struct ProgramVersion {
    Header header_;

    host_u16 major_;
    u8 minor_;
    u8 subminor_;
    u8 revision_;

    static const auto mt = Header::MessageType::program_version;
};



struct Heartbeat {
    Header header_;

    // Just to make sure that players don't somehow end up in different
    // multiplayer game modes. Mostly a sanity check.
    u8 game_mode_;

    u8 unused_[4];

    static const auto mt = Header::MessageType::heartbeat;
};



struct RoomConstructed {
    Header header_;
    HostInteger<MetaclassIndex> metaclass_index_;
    u8 x_;
    u8 y_;
    u8 unused_[1];

    static const auto mt = Header::MessageType::room_constructed;
};



struct TerrainConstructed {
    Header header_;
    u8 new_terrain_size_;
    u8 unused_[4];

    static const auto mt = Header::MessageType::terrain_constructed;
};



struct RoomSalvaged {
    Header header_;
    u8 x_;
    u8 y_;

    u8 unused_[3];

    static const auto mt = Header::MessageType::room_salvaged;
};



struct DynamiteActivated {
    Header header_;
    u8 x_;
    u8 y_;

    u8 unused_[3];

    static const auto mt = Header::MessageType::dynamite_activated;
};



struct WeaponSetTarget {
    Header header_;
    u8 weapon_x_;
    u8 weapon_y_;
    u8 target_x_;
    u8 target_y_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::weapon_set_target;
};



struct DroneSetTarget {
    Header header_;

    u8 drone_x_ : 4;
    u8 drone_y_ : 4;
    u8 target_x_ : 4;
    u8 target_y_ : 4;

    u8 drone_near_ : 1;
    u8 target_near_ : 1;
    u8 reserved_ : 6;

    u8 unused_[2];

    static const auto mt = Header::MessageType::drone_set_target;
};



struct CharacterSetTarget {
    Header header_;
    u8 src_x_;
    u8 src_y_;
    u8 dst_x_;
    u8 dst_y_;

    bool near_island_;

    static const auto mt = Header::MessageType::character_set_target;
};



struct TimekeeperSync {
    Header header_;
    HostInteger<u16> microseconds_;
    HostInteger<u16> seconds_;

    // TODO:
    //
    // In case there's any delay on either console, we should periodically
    // transmit TimekeeperSync messages, to jump ahead one of the game clocks,
    // if necessary.
};



struct RoomDestroyed {
    Header header_;
    u8 room_x_;
    u8 room_y_;

    // For the receiver to determine upon which island the room was destroyed.
    // When reading the RoomDestroyed packet, near_island_ will be true if a
    // room on the receiving player's island was destroyed.
    bool near_island_;

    // We need this event for room plunder actions. If a character plunders a
    // room, PlunderedRoom sentinel structures will be immediately created in
    // place of the room, and we do not want these replacement structures to be
    // destroyed, so we need to check the type of the destroyed room.
    HostInteger<MetaclassIndex> metaclass_index_;

    static const auto mt = Header::MessageType::room_destroyed;
};



// Like the RoomDestroyed Event, the CharacterDied Event is intended for
// preventing scenarios where the games become out of sync. Usually, a character
// that dies in one game will already be equally dead on the connected console.
// This event generally only needs to be transmitted when a character dies in
// combat, as destruction of a room will also destroy the characters within the
// room, so the RoomDestroyed event already covers one of the major ways that a
// character can die in the game.
struct CharacterDied {
    Header header_;
    u8 chr_x_;
    u8 chr_y_;

    bool near_island_;
    bool chr_owned_by_player_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::character_died;
};



struct CharacterBoarded {
    Header header_;

    u8 src_x_;
    u8 src_y_;
    u8 dst_x_;
    u8 dst_y_;

    u8 unused_;

    static const auto mt = Header::MessageType::character_boarded;
};



struct CharacterDisembark {
    Header header_;

    u8 src_x_;
    u8 src_y_;
    u8 dst_x_;
    u8 dst_y_;

    u8 unused_;

    static const auto mt = Header::MessageType::character_disembark;
};



struct ReplicantCreated {
    Header header_;

    u8 src_x_;
    u8 src_y_;

    u8 health_;

    u8 unused_[2];

    static const auto mt = Header::MessageType::replicant_created;
};



struct DroneSpawn {
    Header header_;
    u8 origin_x_ : 4;
    u8 origin_y_ : 4;

    u8 deploy_x_ : 4;
    u8 deploy_y_ : 4;

    u8 destination_near_ : 1;
    u8 reserved_ : 7;

    u8 drone_class_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::drone_spawn;
};



struct DroneDestroyed {
    Header header_;
    u8 drone_x_ : 4;
    u8 drone_y_ : 4;

    u8 destination_near_ : 1;
    u8 reserved_ : 7;

    u8 unused_[3];

    static const auto mt = Header::MessageType::drone_destroyed;
};



struct OpponentBulkheadChanged {
    Header header_;

    u8 room_x_;
    u8 room_y_;

    bool open_;

    u8 unused_[2];

    static const auto mt = Header::MessageType::bulkhead_changed;
};



struct BlockTransferStart {
    Header header_;

    enum class Category : u8 { null } category_;

    host_u16 length_;

    u8 unused_[2];

    static const auto mt = Header::MessageType::block_transfer_start;
};



struct BlockTransfer {
    Header header_;
    u8 sequence_; // (a block transfer can span 256 * 4 bytes)
    u8 data_[4];

    static const auto mt = Header::MessageType::block_transfer;
};



struct GameMatchParameterUpdate {
    Header header_;
    u8 parameter_id_;
    host_s32 value_;

    static const auto mt = Header::MessageType::game_match_parameter_update;
};



struct GameMatchSettingsCursor {
    Header header_;
    u8 cursor_line_;

    u8 unused_[4];

    static const auto mt = Header::MessageType::game_match_settings_cursor;
};



struct GameMatchReady {
    Header header_;
    u8 unused_[5];

    static const auto mt = Header::MessageType::game_match_begin;
};



struct GameModeSelected {
    Header header_;

    // 0: none
    // 1: vs
    // 2: co-op
    u8 mode_;

    u8 unused_[4];

    static const auto mt = Header::MessageType::game_mode_selected;
};



} // namespace packet



class Listener {
public:
    virtual ~Listener()
    {
    }


    virtual void receive(Platform&, App&, const packet::RoomConstructed&)
    {
    }


    virtual void receive(Platform&, App&, const packet::RoomSalvaged&)
    {
    }


    virtual void receive(Platform&, App&, const packet::TerrainConstructed&)
    {
    }


    virtual void receive(Platform&, App&, const packet::WeaponSetTarget&)
    {
    }


    virtual void receive(Platform&, App&, const packet::DroneSetTarget&)
    {
    }


    virtual void receive(Platform&, App&, const packet::CharacterSetTarget&)
    {
    }


    virtual void receive(Platform&, App&, const packet::RoomDestroyed&)
    {
    }


    virtual void receive(Platform&, App&, const packet::CharacterBoarded&)
    {
    }


    virtual void receive(Platform&, App&, const packet::CharacterDisembark&)
    {
    }


    virtual void receive(Platform&, App&, const packet::CharacterDied&)
    {
    }


    virtual void receive(Platform&, App&, const packet::ReplicantCreated&)
    {
    }


    virtual void
    receive(Platform&, App&, const packet::OpponentBulkheadChanged&)
    {
    }


    virtual void receive(Platform&, App&, const packet::ProgramVersion&)
    {
    }


    virtual void receive(Platform&, App&, const packet::BlockTransferStart&)
    {
    }


    virtual void receive(Platform&, App&, const packet::BlockTransfer&)
    {
    }


    virtual void receive(Platform&, App&, const packet::DroneSpawn&)
    {
    }


    virtual void receive(Platform&, App&, const packet::DroneDestroyed&)
    {
    }


    virtual void
    receive(Platform&, App&, const packet::GameMatchParameterUpdate&)
    {
    }


    virtual void
    receive(Platform&, App&, const packet::GameMatchSettingsCursor&)
    {
    }


    virtual void receive(Platform&, App&, const packet::GameMatchReady&)
    {
    }


    virtual void receive(Platform&, App&, const packet::Heartbeat&)
    {
    }


    virtual void receive(Platform&, App&, const packet::DynamiteActivated&)
    {
    }


    virtual void receive(Platform&, App&, const packet::GameModeSelected&)
    {
    }
};



void poll_messages(Platform& pfrm, App& app, Listener& listener);



template <typename T> void transmit(Platform& pfrm, T& message)
{
    static_assert(sizeof(T) <= Platform::NetworkPeer::max_message_size);

    message.header_.message_type_ = T::mt;

    while (
        pfrm.network_peer().is_connected() and
        not pfrm.network_peer().send_message({(u8*)&message, sizeof message}))
        ;
}



} // namespace network



} // namespace skyland
