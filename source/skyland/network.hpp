#pragma once


#include "number/int.h"
#include "number/endian.hpp"
#include "room_metatable.hpp"



namespace skyland {



namespace network {




struct Header {
    enum MessageType : u8 {
        null,
        room_constructed,
        room_salvaged,
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
    u8 unused_[7];

    static const auto mt = Header::MessageType::room_constructed;
};



struct RoomSalvaged {
    Header header_;
    u8 x_;
    u8 y_;

    u8 unused_[9];

    static const auto mt = Header::MessageType::room_salvaged;
};



class Listener {
public:
    virtual ~Listener()
    {
    }


    virtual void receive(Platform&, App&, const RoomConstructed&)
    {
    }


    virtual void receive(Platform&, App&, const RoomSalvaged&)
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



}



}
