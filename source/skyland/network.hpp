////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "number/endian.hpp"
#include "number/int.h"
#include "room_metatable.hpp"



namespace skyland
{
namespace network
{
namespace packet
{



struct Header
{
    enum MessageType : u8 {
#include "network_header_enum"
    } message_type_;
};
static_assert(sizeof(Header) == 1);



#define NET_EVENT_SIZE_CHECK(TYPE)                                             \
    static_assert(sizeof(TYPE) == Platform::NetworkPeer::max_message_size);



struct ProgramVersion
{
    Header header_;

    host_u16 major_;
    u8 minor_;
    u8 subminor_;
    u8 revision_;

    static const auto mt = Header::MessageType::program_version;
};



struct Heartbeat
{
    Header header_;

    u8 unused_[5];

    static const auto mt = Header::MessageType::heartbeat;
};



struct RoomConstructed
{
    Header header_;
    HostInteger<MetaclassIndex> metaclass_index_;
    u8 x_;
    u8 y_;
    u8 unused_[1];

    static const auto mt = Header::MessageType::room_constructed;
};



struct OpponentRoomCreated
{
    Header header_;
    HostInteger<MetaclassIndex> metaclass_index_;
    u8 x_;
    u8 y_;
    u8 unused_[1];

    static const auto mt = Header::MessageType::opponent_room_created;
};



struct TerrainConstructed
{
    Header header_;
    u8 new_terrain_size_;
    u8 unused_[4];

    static const auto mt = Header::MessageType::terrain_constructed;
};



struct TerrainConstructedLeft
{
    Header header_;
    u8 new_terrain_size_;
    u8 unused_[4];

    static const auto mt = Header::MessageType::terrain_constructed_left;
};



struct RoomSalvaged
{
    Header header_;
    u8 x_;
    u8 y_;

    HostInteger<MetaclassIndex> metaclass_index_;
    u8 unused_[1];

    static const auto mt = Header::MessageType::room_salvaged;
};



struct DynamiteActivated
{
    Header header_;
    u8 x_;
    u8 y_;

    u8 unused_[3];

    static const auto mt = Header::MessageType::dynamite_activated;
};



struct WeaponSetTarget
{
    Header header_;
    u8 weapon_x_;
    u8 weapon_y_;
    u8 target_x_;
    u8 target_y_;
    bool weapon_near_;

    static const auto mt = Header::MessageType::weapon_set_target;
};



struct DroneSetTarget
{
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



struct TimekeeperSync
{
    Header header_;
    HostInteger<u16> microseconds_;
    HostInteger<u16> seconds_;

    // TODO:
    //
    // In case there's any delay on either console, we should periodically
    // transmit TimekeeperSync messages, to jump ahead one of the game clocks,
    // if necessary.
};



struct RoomDestroyed
{
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



struct ChrDiedV2
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    bool near_island_;
    u8 unused_[2];

    static const auto mt = Header::MessageType::chr_died_v2;
};



struct ChrBoardedV2
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    u8 dst_x_ : 4;
    u8 dst_y_ : 4;

    // NOTE: for invalidating a transporter after use.
    u8 transporter_x_ : 4;
    u8 transporter_y_ : 4;

    bool transporter_near_ : 1;

    static const auto mt = Header::MessageType::chr_boarded_v2;
};



struct ChrDisembarkV2
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    u8 dst_x_ : 4;
    u8 dst_y_ : 4;

    // NOTE: for invalidating a transporter after use.
    u8 transporter_x_ : 4;
    u8 transporter_y_ : 4;

    bool transporter_near_ : 1;

    static const auto mt = Header::MessageType::chr_disembark_v2;
};



struct ReplicantCreated
{
    Header header_;

    u8 src_x_;
    u8 src_y_;

    u8 health_;

    HostInteger<CharacterId> chr_id_;

    static const auto mt = Header::MessageType::replicant_created;
};



struct DroneSpawn
{
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



struct DroneDestroyed
{
    Header header_;
    u8 drone_x_ : 4;
    u8 drone_y_ : 4;

    u8 destination_near_ : 1;
    u8 reserved_ : 7;

    u8 unused_[3];

    static const auto mt = Header::MessageType::drone_destroyed;
};



struct OpponentBulkheadChanged
{
    Header header_;

    u8 room_x_;
    u8 room_y_;

    bool open_;

    u8 unused_[2];

    static const auto mt = Header::MessageType::bulkhead_changed;
};



struct GameMatchParameterUpdate
{
    Header header_;
    u8 parameter_id_;
    host_s32 value_;

    static const auto mt = Header::MessageType::game_match_parameter_update;
};



struct GameMatchSettingsCursor
{
    Header header_;
    u8 cursor_line_;

    u8 unused_[4];

    static const auto mt = Header::MessageType::game_match_settings_cursor;
};



struct GameMatchReady
{
    Header header_;
    u8 unused_[5];

    static const auto mt = Header::MessageType::game_match_begin;
};



struct CoOpCursor
{
    Header header_;
    u8 x_;
    u8 y_;
    bool near_;
    u8 icon_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::co_op_cursor;
};



struct SetWeaponGroup
{
    Header header_;
    u8 x_;
    u8 y_;
    u8 group_;

    u8 unused_[2];

    static const auto mt = Header::MessageType::set_weapon_group;
};



struct PlayMusic
{
    Header header_;
    host_s32 music_id_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::play_music;
};



struct CoOpRoomLockAcquire
{
    Header header_;
    u8 x_;
    u8 y_;
    u8 unused_[3];

    static const auto mt = Header::MessageType::co_op_room_lock_acquire;
};



struct CoOpRoomLockRelease
{
    Header header_;
    u8 x_;
    u8 y_;
    u8 unused_[3];

    static const auto mt = Header::MessageType::co_op_room_lock_release;
};



struct CoOpRoomLockResponse
{
    Header header_;
    u8 x_;
    u8 y_;
    enum Status : u8 { success, failure } status_;
    u8 unused_[2];

    static const auto mt = Header::MessageType::co_op_room_lock_response;
};



struct CoOpChrLockAcquire
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    u8 unused_[3];

    static const auto mt = Header::MessageType::co_op_chr_lock_acquire;
};



struct CoOpChrLockRelease
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    u8 unused_[3];

    static const auto mt = Header::MessageType::co_op_chr_lock_release;
};



struct CoOpChrLockResponse
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    enum Status : u8 { success, failure } status_;
    u8 unused_[2];

    static const auto mt = Header::MessageType::co_op_chr_lock_response;
};



// NOTE: USE THIS MESSAGE RATHER THAN CHARACTER SET TARGET, EXCEPT FOR VS
// MULTIPLAYER, WHERE WE FIRST NEED TO FIX CHARACTER IDS TO MAKE THEM DISTINCT!
struct ChrSetTargetV2
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    bool near_island_;
    u8 target_x_;
    u8 target_y_;

    static const auto mt = Header::MessageType::chr_set_target_v2;
};



// This message should be unnecessary in most cases. Just as a failsafe.
struct CoOpOpponentDestroyed
{
    Header header_;
    u8 unused_[5];

    static const auto mt = Header::MessageType::co_op_opponent_destroyed;
};



struct MacroSetBlock
{
    Header header_;
    u8 x_ : 4;
    u8 y_ : 4;
    u8 z_ : 4;

    // The rotation of the island from the sender's perspective. I implemented
    // rotation by actually pysically rotating an island's blocks into different
    // slots, so orientation matters in multiplayer.
    u8 rot_ : 2;

    u8 unused_bits_ : 2;

    u8 type_;

    u8 unused_[2];

    static const auto mt = Header::MessageType::macro_set_block;
};



struct BlockTransferStart
{
    Header header_;
    host_u16 length_; // 1024 max!

    u8 unused_[3];

    static const auto mt = Header::MessageType::block_transfer_start;
};



struct BlockTransferData
{
    Header header_;
    u8 sequence_;
    u8 data_[4];

    static const auto mt = Header::MessageType::block_transfer_data;
};



struct BlockTransferEnd
{
    Header header_;

    u8 unused_[5];

    static const auto mt = Header::MessageType::block_transfer_end;
};



struct MacroTradeStatus
{
    Header header_;

    u8 status_;

    u8 unused_[4];

    static const auto mt = Header::MessageType::macro_trade_status;
};



struct Paused
{
    Header header_;

    bool status_;
    u8 unused_[4];

    static const auto mt = Header::MessageType::paused;
};



struct CoOpSyncBegin
{
    Header header_;
    u8 unused_[5];

    static const auto mt = Header::MessageType::co_op_sync_begin;
};



struct CoOpSyncBlock
{
    Header header_;
    u8 block_metaclass_index_;
    u8 block_x_;
    u8 block_y_;
    HostInteger<u16> health_;

    static const auto mt = Header::MessageType::co_op_sync_block;
};



struct CoOpSyncChr
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    u8 x_ : 4;
    u8 y_ : 4;
    u8 health_;
    u8 is_replicant_ : 1;
    u8 unused_ : 7;

    static const auto mt = Header::MessageType::co_op_sync_chr;
};



struct CoOpSyncEnd
{
    Header header_;
    u8 unused_[5];

    static const auto mt = Header::MessageType::co_op_sync_end;
};



} // namespace packet



class Listener
{
public:
    virtual ~Listener()
    {
    }


    virtual void unhandled_message(const packet::Header&)
    {
    }


    virtual void receive(const packet::RoomConstructed& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::RoomSalvaged& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::TerrainConstructed& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::TerrainConstructedLeft& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::WeaponSetTarget& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::DroneSetTarget& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::RoomDestroyed& p)
    {
        unhandled_message(p.header_);
    }



    virtual void receive(const packet::ChrBoardedV2& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::ChrDisembarkV2& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::ChrDiedV2& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::ReplicantCreated& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::OpponentBulkheadChanged& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::ProgramVersion& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::DroneSpawn& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::DroneDestroyed& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::GameMatchParameterUpdate& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::GameMatchSettingsCursor& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::GameMatchReady& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::Heartbeat& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::DynamiteActivated& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpCursor& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::SetWeaponGroup& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::PlayMusic& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpRoomLockAcquire& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpRoomLockRelease& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpRoomLockResponse& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpChrLockAcquire& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpChrLockRelease& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpChrLockResponse& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpOpponentDestroyed& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::MacroSetBlock& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::BlockTransferStart& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::BlockTransferData& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::BlockTransferEnd& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::MacroTradeStatus& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::Paused& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::ChrSetTargetV2& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpSyncBegin& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpSyncBlock& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpSyncChr& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::CoOpSyncEnd& p)
    {
        unhandled_message(p.header_);
    }


    virtual void receive(const packet::OpponentRoomCreated& p)
    {
        unhandled_message(p.header_);
    }


    virtual void error(const char* err)
    {
    }
};



void poll_messages(Listener& listener);



template <typename T> void transmit(T& message)
{
    static_assert(sizeof(T) == Platform::NetworkPeer::max_message_size);
    static_assert(alignof(T) == 1);
    static_assert(std::is_trivially_copyable<T>());

    message.header_.message_type_ = T::mt;

    while (PLATFORM.network_peer().is_connected() and
           not PLATFORM.network_peer().send_message(
               {(u8*)&message, sizeof message}))
        ;
}



} // namespace network



} // namespace skyland
