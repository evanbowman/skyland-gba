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


#include "network.hpp"



namespace skyland::network
{



void poll_messages(Platform& pfrm, App& app, Listener& listener)
{
    while (auto message = pfrm.network_peer().poll_message()) {
        if (message->length_ < sizeof(packet::Header)) {
            return;
        }
        packet::Header header;
        memcpy(&header, message->data_, sizeof header);

        // if (header.parity_ not_eq parity((u8*)message->data_ + 1)) {
        //     listener.error(pfrm, app, "parity check failed!");
        //     pfrm.network_peer().poll_consume(
        //         Platform::NetworkPeer::max_message_size);
        // }

        switch (header.message_type_) {
        case packet::Header::invalid:
            pfrm.network_peer().poll_consume(sizeof(packet::Header));
            continue;

#define HANDLE_MESSAGE(MESSAGE_TYPE)                                           \
    case MESSAGE_TYPE::mt: {                                                   \
        NET_EVENT_SIZE_CHECK(MESSAGE_TYPE)                                     \
        if (message->length_ < sizeof(MESSAGE_TYPE)) {                         \
            return;                                                            \
        }                                                                      \
        listener.receive(pfrm, app, *(const MESSAGE_TYPE*)message->data_);     \
        pfrm.network_peer().poll_consume(sizeof(MESSAGE_TYPE));                \
        continue;                                                              \
    }

            HANDLE_MESSAGE(packet::RoomConstructed)
            HANDLE_MESSAGE(packet::RoomSalvaged)
            HANDLE_MESSAGE(packet::TerrainConstructed)
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

        error(pfrm, "garbled message!?");
        pfrm.network_peer().disconnect();
        return;
    }
}



} // namespace skyland::network
