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

    ScenePtr<Scene> update(Microseconds delta)
    {
        network::poll_messages(*this);

        if (syncd_) {
            return scene_pool::alloc<ReadyScene>();
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
    Microseconds timer_ = 0;
    bool syncd_ = false;
};



} // namespace skyland
