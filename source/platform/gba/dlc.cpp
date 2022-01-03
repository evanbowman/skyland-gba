#include "gba.h"
#include "memory/buffer.hpp"
#include "platform/platform.hpp"
#include "platform/ram_filesystem.hpp"



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

    REG_SIOCNT |= SIO_START;
}



static void multi_serial_master_init_timer()
{
    // These times must be carefully calibrated. If you set the time too small,
    // you risk starting a transmission before the previous transmission
    // finished. I am using these same timer values in Blind Jump, an open
    // source gba game. I think pokemon uses similar values. You can try to
    // increase the frequency of the transmissions, but do so at your own risk.
    REG_TM2CNT_H = 0x00C1;
    REG_TM2CNT_L = 65000;

    irqSet(IRQ_TIMER2, multi_master_timer_isr);
    irqEnable(IRQ_TIMER2);
}



StringBuffer<1024>* receive_data;



enum class DLCReceiveState {
    rcv_file_count,
    rcv_file_name_length,
    rcv_file_name,
    rcv_file_size,
    rcv_file_data,
};



static void multi_serial_isr()
{
    if (multiplayer_is_master()) {
        receive_data->push_back((REG_SIOMULTI1 & 0xff00) >> 8);
        receive_data->push_back(REG_SIOMULTI1 & 0x00ff);
        multi_serial_master_init_timer();
    } else {
        receive_data->push_back((REG_SIOMULTI0 & 0xff00) >> 8);
        receive_data->push_back(REG_SIOMULTI0 & 0x00ff);
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



void read_dlc(Platform& pfrm)
{
    REG_RCNT = R_MULTI;
    REG_SIOCNT = SIO_MULTI;
    REG_SIOCNT |= SIO_IRQ | SIO_115200;

    irqEnable(IRQ_SERIAL);
    irqSet(IRQ_SERIAL, multi_dlc_isr);

    int connection_mask = 0;

    StringBuffer<1024> receive_data;
    ::receive_data = &receive_data;


    if (multiplayer_is_master()) {
        while (1) {
            // When the host determines that it's time to advance to an active
            // multiplayer session, it writes a start command, and returns.
            if (connection_mask & multi_PlayerId_p1) {
                REG_SIOMLT_SEND = MULTI_DEVICE_START;
                REG_SIOCNT |= SIO_START;

                // Wait a bit for the start command to propagate.
                busy_wait(40000);

                // ...

                info(pfrm, "dlc provider detected!");

                multi_serial_init();
                break;

            } else {

                // Ok, so we're going to send out a ready integer constant, and
                // see which devices ping back a ready response.
                REG_SIOMLT_SEND = MULTI_DEVICE_READY;
                REG_SIOCNT |= SIO_START;

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
                info(pfrm, "dlc provider detected!");
                multi_serial_init();
                break;
            }
        }
    }

    while (not receive_data.full()) {
        pfrm.feed_watchdog();
    }

    Vector<char> data(pfrm);
    for (char c : receive_data) {
        data.push_back(c);
    }
    data.push_back('\0');

    ram_filesystem::store_file_data(pfrm, "/test/dlc.lisp", data);
}
