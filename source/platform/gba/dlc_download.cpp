////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "containers/vector.hpp"
#include "gba.h"
#include "memory/buffer.hpp"
#include "platform/platform.hpp"



typedef enum {
    multi_PlayerId_host = (1 << 0),
    multi_PlayerId_p1 = (1 << 1),
    multi_PlayerId_p2 = (1 << 2),
    multi_PlayerId_p3 = (1 << 3),
    multi_PlayerId_unknown = (1 << 4)
} multi_PlayerId;



// These enums don't have anything to do with the gba hardware, they're just
// magic constants that we're using during startup to detect which gbas are want
// to connect.
enum {
    MULTI_DEVICE_READY = 0xAA,

    // The host (master) device will broadcast a start command when the host
    // player decides to start the multiplayer game. (e.g. when the host player
    // decides that enough players have connected, and presses a button, or
    // something).
    MULTI_DEVICE_START = 0xFF,
};


volatile bool sio_got_intr = 1;


static int multiplayer_is_master()
{
    return (REG_SIOCNT & (1 << 2)) == 0 and (REG_SIOCNT & (1 << 3));
}


static void multi_dlc_isr()
{
    sio_got_intr = 1;
}



static void __attribute__((noinline)) busy_wait(unsigned max)
{
    for (unsigned i = 0; i < max; i++) {
        __asm__ volatile("" : "+g"(i) : :);
    }
}



static void multi_connect_check_device_ready(int* connection_mask,
                                             u16 state,
                                             multi_PlayerId device_id)
{
    if (state == MULTI_DEVICE_READY && !(*connection_mask & device_id)) {
        *connection_mask |= device_id;
    } else if (state != MULTI_DEVICE_READY && *connection_mask & device_id) {
        *connection_mask &= ~device_id;
    }
}



static void multi_connect_check_devices(int* connection_mask)
{
    multi_connect_check_device_ready(
        connection_mask, REG_SIOMULTI0, multi_PlayerId_host);

    multi_connect_check_device_ready(
        connection_mask, REG_SIOMULTI1, multi_PlayerId_p1);

    multi_connect_check_device_ready(
        connection_mask, REG_SIOMULTI2, multi_PlayerId_p2);

    multi_connect_check_device_ready(
        connection_mask, REG_SIOMULTI3, multi_PlayerId_p3);
}



static void multi_master_timer_isr()
{
    irqDisable(IRQ_TIMER2);

    REG_SIOCNT = REG_SIOCNT | SIO_START;
}



static void multi_serial_master_init_timer()
{
    REG_TM2CNT_H = 0x00C1;
    REG_TM2CNT_L = 65000;

    irqSet(IRQ_TIMER2, multi_master_timer_isr);
    irqEnable(IRQ_TIMER2);
}



struct DlcTransferContext
{
    Vector<char>* receive_data_;

    volatile enum class State {
        read_size,
        read_bin,
    } state_ = State::read_size;

    volatile u16 halfwords_remaining_;
};



static DlcTransferContext* dlc_transfer;



static void on_data(u16 data)
{
    switch (dlc_transfer->state_) {
    case DlcTransferContext::State::read_size:
        dlc_transfer->state_ = DlcTransferContext::State::read_bin;
        dlc_transfer->halfwords_remaining_ = data;
        break;

    case DlcTransferContext::State::read_bin:
        if (dlc_transfer->halfwords_remaining_) {
            dlc_transfer->receive_data_->push_back((data & 0xff00) >> 8);
            dlc_transfer->receive_data_->push_back(data & 0x00ff);
            dlc_transfer->halfwords_remaining_ =
                dlc_transfer->halfwords_remaining_ - 1;
        }
        break;
    }
}



static void multi_serial_isr()
{
    if (multiplayer_is_master()) {

        on_data(REG_SIOMULTI1);

        multi_serial_master_init_timer();
    } else {

        on_data(REG_SIOMULTI0);
    }
}



static void multi_serial_init()
{
    REG_SIOMLT_SEND = 0;

    if (multiplayer_is_master()) {
        multi_serial_master_init_timer();
    }

    irqSet(IRQ_SERIAL, multi_serial_isr);
}



void download_dlc_blob(Vector<char>& output)
{
    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI;
    REG_SIOCNT = REG_SIOCNT | SIO_IRQ | SIO_115200;

    irqEnable(IRQ_SERIAL);
    irqSet(IRQ_SERIAL, multi_dlc_isr);

    int connection_mask = 0;


    DlcTransferContext dlc_transfer;
    dlc_transfer.receive_data_ = &output;
    dlc_transfer.halfwords_remaining_ = 0;

    ::dlc_transfer = &dlc_transfer;


    if (multiplayer_is_master()) {
        while (1) {
            // When the host determines that it's time to advance to an active
            // multiplayer session, it writes a start command, and returns.
            if (connection_mask & multi_PlayerId_p1) {
                REG_SIOMLT_SEND = MULTI_DEVICE_START;
                REG_SIOCNT = REG_SIOCNT | SIO_START;

                // Wait a bit for the start command to propagate.
                busy_wait(40000);

                // ...

                multi_serial_init();
                break;
            } else {

                // Ok, so we're going to send out a ready integer constant, and
                // see which devices ping back a ready response.
                REG_SIOMLT_SEND = MULTI_DEVICE_READY;
                REG_SIOCNT = REG_SIOCNT | SIO_START;

                // FIXME... busy wait for now. We should really be waiting on a
                // timer interrupt. But I'm feeling lazy.
                busy_wait(20000);

                multi_connect_check_devices(&connection_mask);
            }
        }
    } else {
        while (1) {
            REG_SIOMLT_SEND = MULTI_DEVICE_READY;

            while (!sio_got_intr)
                ; // Wait for serial interrupt.
            sio_got_intr = 0;

            // If we've received a start command from the master, now let's set
            // up the multiplayer session.
            if (REG_SIOMULTI0 == MULTI_DEVICE_START) {
                multi_serial_init();
                break;
            }
        }
    }

    while (dlc_transfer.state_ == DlcTransferContext::State::read_size or
           dlc_transfer.halfwords_remaining_) {
        PLATFORM_EXTENSION(feed_watchdog);
    }

    irqDisable(IRQ_SERIAL);
    irqDisable(IRQ_TIMER2);

    output.push_back('\0');
}
