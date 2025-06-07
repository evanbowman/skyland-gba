////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "number/random.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "worldScene.hpp"



// This scene performs a handshake to coordinate rng state, to ensure that an
// rng sequence is synchronized between two game sessions.
//
// Step 1) The primary (host) device waits, while the secondary sends out sync
// requests.
//
// Step 2) The primary receives a sync request, and sends out a sync message.
//
// Step 3) The secondary receives the sync message, and sends a sync ack.
//
// Step 4) The primary receives the sync ack, and, if it matches the original
// sync message, sends its own sync ack. The primary now returns to the game.
//
// Step 5) The secondary receives the primary's sync ack. The secondary now
// returns to the game.



namespace skyland
{



class CoOpRngSyncScene : public WorldScene, public network::Listener
{
public:
    void enter(Scene& prev)
    {
        WorldScene::enter(prev);

        for (auto& room : player_island().rooms()) {
            // Just in case... should there be a bug in the locking anywhere
            // (I haven't found any), unlock everything at the end of the
            // level.
            room->co_op_peer_release_lock();
            for (auto& chr : room->characters()) {
                chr->co_op_release_lock();
            }
        }
    }

    ScenePtr update(Time delta)
    {
        network::poll_messages(*this);

        if (syncd_) {
            return make_scene<ReadyScene>();
        }

        if (not PLATFORM.network_peer().is_host()) {
            timer_ += delta;
            if (timer_ > milliseconds(100)) {
                timer_ = 0;
                network::packet::CoOpRngSyncRequest p;
                network::transmit(p);
            }
        }

        return null_scene();
    }


    void receive(const network::packet::CoOpRngSyncRequest&)
    {
        if (PLATFORM.network_peer().is_host()) {
            network::packet::CoOpRngSync p;
            p.rng_state_.set(rng::critical_state);
            network::transmit(p);
        }
    }


    void receive(const network::packet::CoOpRngSyncAck& ack)
    {
        if (PLATFORM.network_peer().is_host()) {
            if (ack.rng_state_.get() == rng::critical_state) {
                syncd_ = true;

                network::packet::CoOpRngSyncAck p;
                p.rng_state_.set(rng::critical_state);
                network::transmit(p);
            }
        } else {
            syncd_ = true;
        }
    }


private:
    Time timer_ = 0;
    bool syncd_ = false;
};



} // namespace skyland
