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
    } message_type_;
};
static_assert(sizeof(Header) == 1);



#define NET_EVENT_SIZE_CHECK(TYPE)                                             \
    static_assert(sizeof(TYPE) == Platform::NetworkPeer::max_message_size);


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



struct WeaponSetTarget {
    Header header_;
    u8 weapon_x_;
    u8 weapon_y_;
    u8 target_x_;
    u8 target_y_;

    u8 unused_[1];

    static const auto mt = Header::MessageType::weapon_set_target;
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
};



void poll_messages(Platform& pfrm, App& app, Listener& listener);



template <typename T> void transmit(Platform& pfrm, T& message)
{
    static_assert(sizeof(T) <= Platform::NetworkPeer::max_message_size);

    message.header_.message_type_ = T::mt;

    while (
        pfrm.network_peer().is_connected() and
        not pfrm.network_peer().send_message({(byte*)&message, sizeof message}))
        ;
}



} // namespace network



} // namespace skyland
