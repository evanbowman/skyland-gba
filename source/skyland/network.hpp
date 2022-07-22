////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
        null,
        room_constructed,
        terrain_constructed,
        room_salvaged,
        weapon_set_target,
        character_set_target,
        chr_set_target_v2,
        room_destroyed,
        character_boarded,
        chr_boarded_v2,
        character_disembark,
        chr_disembark_v2,
        character_died,
        chr_died_v2,
        replicant_created,
        bulkhead_changed,
        program_version,
        drone_set_target,
        drone_spawn,
        drone_destroyed,
        game_match_parameter_update,
        game_match_settings_cursor,
        game_match_begin,
        heartbeat,
        dynamite_activated,
        co_op_cursor,
        set_weapon_group,
        play_music,
        co_op_room_lock_acquire,
        co_op_room_lock_release,
        co_op_room_lock_response,
        co_op_chr_lock_acquire,
        co_op_chr_lock_release,
        co_op_chr_lock_response,
        co_op_opponent_destroyed,
        macro_set_block,
        block_transfer_start,
        block_transfer_data,
        block_transfer_end,
        macro_trade_status,
        paused,
        co_op_sync_begin,
        co_op_sync_block,
        co_op_sync_chr,
        co_op_sync_end,
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



struct TerrainConstructed
{
    Header header_;
    u8 new_terrain_size_;
    u8 unused_[4];

    static const auto mt = Header::MessageType::terrain_constructed;
};



struct RoomSalvaged
{
    Header header_;
    u8 x_;
    u8 y_;

    u8 unused_[3];

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



struct CharacterSetTarget
{
    Header header_;

    // FIXME: Rather than src_x_ and src_y_, we should be identifying a
    // character by character id. We already do this elsewhere in the code,
    // e.g. in some of the character-related messages for co-op mode, and in the
    // rewind logic. But, using character id becomes tricky in vs-multiplayer,
    // where we need to first add extra logic to assign character ids
    // differently than we currently do when setting up a game, as nothing
    // prevents each console from generating the same ids for each player's
    // characters. For co-op mode, both players control the same castle, so
    // there's no id issue, only a problem in vs-multiplayer.

    u8 src_x_;
    u8 src_y_;
    u8 dst_x_;
    u8 dst_y_;

    bool near_island_ : 1;
    bool owned_by_ai_ : 1;

    static const auto mt = Header::MessageType::character_set_target;
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



// Like the RoomDestroyed Event, the CharacterDied Event is intended for
// preventing scenarios where the games become out of sync. Usually, a character
// that dies in one game will already be equally dead on the connected console.
// This event generally only needs to be transmitted when a character dies in
// combat, as destruction of a room will also destroy the characters within the
// room, so the RoomDestroyed event already covers one of the major ways that a
// character can die in the game.
struct CharacterDied
{
    Header header_;
    u8 chr_x_;
    u8 chr_y_;

    bool near_island_;
    bool chr_owned_by_player_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::character_died;
};



struct ChrDiedV2
{
    Header header_;
    HostInteger<CharacterId> chr_id_;
    bool near_island_;
    u8 unused_[2];

    static const auto mt = Header::MessageType::chr_died_v2;
};



struct CharacterBoarded
{
    Header header_;

    u8 src_x_;
    u8 src_y_;
    u8 dst_x_;
    u8 dst_y_;

    bool owned_by_ai_ : 1;
    bool transporter_near_ : 1;
    bool unused_ : 6;

    static const auto mt = Header::MessageType::character_boarded;
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



struct CharacterDisembark
{
    Header header_;

    u8 src_x_;
    u8 src_y_;
    u8 dst_x_;
    u8 dst_y_;

    bool owned_by_ai_ : 1;
    bool transporter_near_ : 1;
    bool unused_ : 6;

    static const auto mt = Header::MessageType::character_disembark;
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

    u8 unused_[2];

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
    u8 block_x_ : 4;
    u8 block_y_ : 4;
    HostInteger<u16> health_;

    u8 unused_[1];

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


    virtual void unhandled_message(Platform&, App&, const packet::Header&)
    {
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::RoomConstructed& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::RoomSalvaged& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::TerrainConstructed& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::WeaponSetTarget& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::DroneSetTarget& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CharacterSetTarget& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::RoomDestroyed& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CharacterBoarded& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::ChrBoardedV2& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CharacterDisembark& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::ChrDisembarkV2& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CharacterDied& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void receive(Platform& pfrm, App& app, const packet::ChrDiedV2& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::ReplicantCreated& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::OpponentBulkheadChanged& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::ProgramVersion& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void receive(Platform& pfrm, App& app, const packet::DroneSpawn& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::DroneDestroyed& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::GameMatchParameterUpdate& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::GameMatchSettingsCursor& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::GameMatchReady& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void receive(Platform& pfrm, App& app, const packet::Heartbeat& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::DynamiteActivated& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void receive(Platform& pfrm, App& app, const packet::CoOpCursor& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::SetWeaponGroup& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void receive(Platform& pfrm, App& app, const packet::PlayMusic& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpRoomLockAcquire& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpRoomLockRelease& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpRoomLockResponse& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpChrLockAcquire& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpChrLockRelease& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpChrLockResponse& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpOpponentDestroyed& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::MacroSetBlock& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::BlockTransferStart& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::BlockTransferData& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::BlockTransferEnd& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::MacroTradeStatus& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void receive(Platform& pfrm, App& app, const packet::Paused& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::ChrSetTargetV2& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpSyncBegin& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpSyncBlock& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpSyncChr& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void
    receive(Platform& pfrm, App& app, const packet::CoOpSyncEnd& p)
    {
        unhandled_message(pfrm, app, p.header_);
    }


    virtual void error(Platform&, App&, const char* err)
    {
    }
};



void poll_messages(Platform& pfrm, App& app, Listener& listener);



template <typename T> void transmit(Platform& pfrm, T& message)
{
    static_assert(sizeof(T) == Platform::NetworkPeer::max_message_size);
    static_assert(alignof(T) == 1);
    static_assert(std::is_trivially_copyable<T>());

    message.header_.message_type_ = T::mt;

    while (
        pfrm.network_peer().is_connected() and
        not pfrm.network_peer().send_message({(u8*)&message, sizeof message}))
        ;
}



} // namespace network



} // namespace skyland
