#pragma once


#include "network.hpp"
#include "bitvector.hpp"
#include "bulkAllocator.hpp"



// Our network protocol includes a message type called a block transfer, used
// for copying large amounts of data from one console to another.



namespace skyland {



class BlockTransferListener : public network::Listener {
public:

    void receive(Platform&,
                 App&,
                 const network::packet::BlockTransferStart& packet) override
    {
        length_ = packet.length_.get();
    }


    void receive(Platform&,
                 App&,
                 const network::packet::BlockTransfer& packet) override
    {
        if (received_ == -1) {
            received_ = 0;
        }

        received_ += 4;

        auto write_index = packet.sequence_ * 4;

        data_[write_index++] = packet.data_[0];
        data_[write_index++] = packet.data_[1];
        data_[write_index++] = packet.data_[2];
        data_[write_index++] = packet.data_[3];
    }


    bool completed() const
    {
        return received_ == length_;
    }


    u16 length_ = 0;
    s16 received_ = -1;
    u8 data_[1024];
};



void block_transfer_receive(Platform& pfrm,
                            App& app,
                            BlockTransferListener& transfer_listener,
                            Microseconds timeout)
{
    while (timeout or not transfer_listener.completed()) {
        timeout -= pfrm.delta_clock().reset();
        timeout = clamp(timeout, 0, std::numeric_limits<Microseconds>::max());

        poll_messages(pfrm, app, transfer_listener);
        pfrm.feed_watchdog();
    }
}



}
