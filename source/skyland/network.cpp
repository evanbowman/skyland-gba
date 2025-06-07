////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "network.hpp"



namespace skyland::network
{



void poll_messages(Listener& listener)
{
    while (auto message = PLATFORM.network_peer().poll_message()) {
        if (message->length_ < sizeof(packet::Header)) {
            return;
        }
        packet::Header header;
        memcpy(&header, message->data_, sizeof header);

        // if (header.parity_ not_eq parity((u8*)message->data_ + 1)) {
        //     listener.error("parity check failed!");
        //     PLATFORM.network_peer().poll_consume(
        //         Platform::NetworkPeer::max_message_size);
        // }

        switch (header.message_type_) {
        case packet::Header::invalid:
            PLATFORM.network_peer().poll_consume(sizeof(packet::Header));
            continue;

#define HANDLE_MESSAGE(MESSAGE_TYPE)                                           \
    case MESSAGE_TYPE::mt: {                                                   \
        NET_EVENT_SIZE_CHECK(MESSAGE_TYPE)                                     \
        if (message->length_ < sizeof(MESSAGE_TYPE)) {                         \
            return;                                                            \
        }                                                                      \
        listener.receive(*(const MESSAGE_TYPE*)message->data_);                \
        PLATFORM.network_peer().poll_consume(sizeof(MESSAGE_TYPE));            \
        continue;                                                              \
    }

            HANDLE_MESSAGE(packet::RoomConstructed)
            HANDLE_MESSAGE(packet::OpponentRoomCreated)
            HANDLE_MESSAGE(packet::RoomSalvaged)
            HANDLE_MESSAGE(packet::TerrainConstructed)
            HANDLE_MESSAGE(packet::TerrainConstructedLeft)
            HANDLE_MESSAGE(packet::WeaponSetTarget)
            HANDLE_MESSAGE(packet::DroneSetTarget)
            HANDLE_MESSAGE(packet::ChrSetTargetV2);
            HANDLE_MESSAGE(packet::RoomDestroyed)
            HANDLE_MESSAGE(packet::ChrBoardedV2)
            HANDLE_MESSAGE(packet::ChrDisembarkV2)
            HANDLE_MESSAGE(packet::ChrDiedV2)
            HANDLE_MESSAGE(packet::ReplicantCreated)
            HANDLE_MESSAGE(packet::OpponentBulkheadChanged)
            HANDLE_MESSAGE(packet::ProgramVersion)
            HANDLE_MESSAGE(packet::DroneSpawn)
            HANDLE_MESSAGE(packet::DroneDestroyed)
            HANDLE_MESSAGE(packet::GameMatchParameterUpdate)
            HANDLE_MESSAGE(packet::GameMatchSettingsCursor)
            HANDLE_MESSAGE(packet::GameMatchReady)
            HANDLE_MESSAGE(packet::Heartbeat)
            HANDLE_MESSAGE(packet::DynamiteActivated)
            HANDLE_MESSAGE(packet::CoOpCursor)
            HANDLE_MESSAGE(packet::SetWeaponGroup)
            HANDLE_MESSAGE(packet::PlayMusic)
            HANDLE_MESSAGE(packet::CoOpRoomLockAcquire)
            HANDLE_MESSAGE(packet::CoOpRoomLockRelease)
            HANDLE_MESSAGE(packet::CoOpRoomLockResponse)
            HANDLE_MESSAGE(packet::CoOpChrLockAcquire)
            HANDLE_MESSAGE(packet::CoOpChrLockRelease)
            HANDLE_MESSAGE(packet::CoOpChrLockResponse)
            HANDLE_MESSAGE(packet::CoOpOpponentDestroyed)
            HANDLE_MESSAGE(packet::MacroSetBlock)
            HANDLE_MESSAGE(packet::BlockTransferStart)
            HANDLE_MESSAGE(packet::BlockTransferData)
            HANDLE_MESSAGE(packet::BlockTransferEnd)
            HANDLE_MESSAGE(packet::MacroTradeStatus)
            HANDLE_MESSAGE(packet::Paused)
            HANDLE_MESSAGE(packet::CoOpSyncBegin)
            HANDLE_MESSAGE(packet::CoOpSyncBlock)
            HANDLE_MESSAGE(packet::CoOpSyncChr)
            HANDLE_MESSAGE(packet::CoOpSyncEnd)
        }

        error("garbled message!?");
        PLATFORM.network_peer().disconnect();
        return;
    }
}



} // namespace skyland::network
