#include "network.hpp"



namespace skyland::network {



void poll_messages(Platform& pfrm, App& app, Listener& listener)
{
    while (auto message = pfrm.network_peer().poll_message()) {
        if (message->length_ < sizeof(packet::Header)) {
            return;
        }
        packet::Header header;
        memcpy(&header, message->data_, sizeof header);

        if (header.parity_ not_eq parity((u8*)message->data_ + 1)) {
            listener.error(pfrm, app, "parity check failed!");
            pfrm.network_peer().poll_consume(
                Platform::NetworkPeer::max_message_size);
        }

        switch (header.message_type_) {
        case packet::Header::null:
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
            HANDLE_MESSAGE(packet::CharacterSetTarget)
            HANDLE_MESSAGE(packet::RoomDestroyed)
            HANDLE_MESSAGE(packet::CharacterBoarded)
            HANDLE_MESSAGE(packet::CharacterDisembark)
            HANDLE_MESSAGE(packet::CharacterDied)
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
            HANDLE_MESSAGE(packet::CoopCursor)
            HANDLE_MESSAGE(packet::CoopRngSyncRequest)
            HANDLE_MESSAGE(packet::CoopRngSync)
            HANDLE_MESSAGE(packet::CoopRngSyncAck)
            HANDLE_MESSAGE(packet::SetWeaponGroup)
        }

        error(pfrm, "garbled message!?");
        pfrm.network_peer().disconnect();
        return;
    }
}



} // namespace skyland::network
