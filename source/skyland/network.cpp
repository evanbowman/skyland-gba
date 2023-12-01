////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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
