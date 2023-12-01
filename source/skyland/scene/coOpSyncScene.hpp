////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


#include "readyScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/scene.hpp"


// The host console sends out its state, the peer console verifies and updates
// its state from the host. Required at the end of a level, to ensure that both
// players go into the generation algorithm for the next level with the same
// island.
//
// 1) Both devices exchange CoOpSyncBegin messages
// 2) host begins sending info about its rooms and characters
// 3) host sends CoOpSyncEnd message and waits
// 4) Peer gets CoOpSyncEnd message, syncs stuff, and then proceeds to game
// 5) Host receives CoOpSyncEnd message and then proceeds to game



namespace skyland
{



class CoOpSyncScene : public Scene, public network::Listener
{
public:
    CoOpSyncScene() : ctx_(allocate_dynamic<Context>("co_op_sync_ctx"))
    {
    }


    enum class State {
        wait,
        host_send_rooms,
        host_send_chrs,
        host_wait_after_send,
        peer_receive_rooms,
        host_await_accept,
        done,
    };


    struct RoomSyncInfo
    {
        u8 room_metaclass_;
        u8 x_ : 4;
        u8 y_ : 4;
        u16 health_;
    };


    struct Context
    {
        Buffer<RoomSyncInfo, 200> received_;
        Buffer<network::packet::CoOpSyncChr, 40> chr_info_;
        u32 room_send_index_ = 0;

        // Note: of course, there should never be this many characters on an
        // island, but who even knows what players will try to do.
        Buffer<BasicCharacter*, 64> send_chrs_;
        u32 chr_send_index_ = 0;
    };


    void enter(Scene& prev) override
    {
        // Yeah, so only the host needs to send out its characters. But collect
        // them on both consoles regardless.
        auto& island = player_island();
        for (auto& room : island.rooms()) {
            for (auto& chr : room->characters()) {
                ctx_->send_chrs_.push_back(chr.get());
            }
        }
    }


    void to_state(State s)
    {
        state_ = s;
        timer_ = 0;
    }


    void receive(const network::packet::CoOpSyncBegin& p) override
    {
        if (state_ == State::wait) {
            if (PLATFORM.network_peer().is_host()) {
                to_state(State::host_send_rooms);
            } else {
                to_state(State::peer_receive_rooms);
            }
            // Just in case of a race condition (where first device sends out
            // SyncBegin, second device is arrives in the scene in time to
            // receive the syncbegin but not in time to send out its own).
            network::packet::CoOpSyncBegin pkt;
            network::transmit(pkt);
        }
    }


    void receive(const network::packet::CoOpSyncBlock& p) override
    {
        RoomSyncInfo info;
        info.x_ = p.block_x_;
        info.y_ = p.block_y_;
        info.room_metaclass_ = p.block_metaclass_index_;
        info.health_ = p.health_.get();

        ctx_->received_.push_back(info);
    }


    void receive(const network::packet::CoOpSyncChr& p) override
    {
        ctx_->chr_info_.push_back(p);
    }


    void receive(const network::packet::CoOpSyncEnd& p) override
    {
        if (state_ == State::peer_receive_rooms) {
            peer_synchronize();
            network::packet::CoOpSyncEnd pkt;
            network::transmit(pkt);
            to_state(State::done);
        } else if (state_ == State::host_await_accept) {
            to_state(State::done);
        }
    }


    ScenePtr<Scene> update(Microseconds delta) override
    {
        network::poll_messages(*this);

        switch (state_) {
        case State::wait:
            timer_ += delta;
            if (timer_ > milliseconds(300)) {
                timer_ = 0;
                // Repeatedly transmit until other player sends its own sync
                // begin.
                network::packet::CoOpSyncBegin pkt;
                network::transmit(pkt);
            }
            break;

        case State::host_send_rooms: {
            auto& rooms = player_island().rooms();
            if (ctx_->room_send_index_ == rooms.size()) {
                to_state(State::host_send_chrs);
            } else {

                // Yeah, I'm only transmitting one per frame. But no reason to
                // try to push to the limits of the bandwidth and drop messages,
                // as this is a sanity check game state where we want to make
                // sure that everything is absolutely correct. In practice, we
                // could send at least a few messages per update.
                auto& room = rooms[ctx_->room_send_index_++];
                network::packet::CoOpSyncBlock pkt;
                pkt.block_metaclass_index_ = room->metaclass_index();
                pkt.block_x_ = room->position().x;
                pkt.block_y_ = room->position().y;
                pkt.health_.set(room->health());
                network::transmit(pkt);
            }
            break;
        }

        case State::host_send_chrs: {
            if (ctx_->chr_send_index_ == ctx_->send_chrs_.size()) {
                to_state(State::host_wait_after_send);
            } else {
                auto chr = ctx_->send_chrs_[ctx_->chr_send_index_++];

                network::packet::CoOpSyncChr pkt;
                pkt.chr_id_.set(chr->id());
                pkt.x_ = chr->grid_position().x;
                pkt.y_ = chr->grid_position().y;
                pkt.health_ = chr->health();
                pkt.is_replicant_ = chr->is_replicant();
                network::transmit(pkt);
            }
            break;
        }

        case State::host_wait_after_send:
            // Wait after send to make sure that the receiver was able to
            // process all of it's packets before sending a sync end
            // packet. We're just being extra careful to make sure that we don't
            // lose any data.
            timer_ += delta;
            if (timer_ > milliseconds(200)) {
                network::packet::CoOpSyncEnd pkt;
                network::transmit(pkt);
                to_state(State::host_await_accept);
            }
            break;

        case State::host_await_accept:
            // Handled in Listener overrides
            break;

        case State::peer_receive_rooms:
            // Handled in Listener overrides
            break;

        case State::done:
            return scene_pool::alloc<ReadyScene>();
        }

        return null_scene();
    }


    // Called by the receiver to sync its state after the sender finished
    // sending everything.
    void peer_synchronize()
    {
        auto& island = player_island();

        for (auto& room : island.rooms()) {
            room->unmark();

            for (auto& chr : room->characters()) {
                chr->unmark();
            }
        }

        for (auto& rx : ctx_->received_) {
            if (auto room = island.get_room({rx.x_, rx.y_})) {
                if (room->metaclass_index() == rx.room_metaclass_) {
                    room->mark();
                    room->__set_health(rx.health_);
                }
            } else {
                // Create room if it doesn't exist.
                auto mt = load_metaclass(rx.room_metaclass_);
                (*mt)->create(&island, {rx.x_, rx.y_});
                if (auto room = island.get_room({rx.x_, rx.y_})) {
                    room->__set_health(rx.health_);
                    room->mark();
                }
            }
        }

        for (auto& room : island.rooms()) {
            if (not room->marked()) {
                // Destroy any leftover rooms that the other console didn't tell
                // us about.
                room->apply_damage(Room::health_upper_limit());
            }
        }

        for (auto& chr_rx : ctx_->chr_info_) {
            auto info = island.find_character_by_id(chr_rx.chr_id_.get());
            if (auto chr = info.first) {
                chr->__set_health(chr_rx.health_);
                chr->mark();
            } else {
                const bool is_replicant = chr_rx.is_replicant_;
                const Vec2<u8> coord{chr_rx.x_, chr_rx.y_};
                auto e = alloc_entity<BasicCharacter>(
                    &island, &player(), coord, is_replicant);
                e->__set_health(chr_rx.health_);
                e->mark();

                if (e) {
                    island.add_character(std::move(e));
                }
            }
        }

        for (auto& room : island.rooms()) {
            for (auto& chr : room->characters()) {
                if (not chr->marked()) {
                    chr->__set_health(0);
                }
            }
        }
    }


private:
    Microseconds timer_ = 0;

    State state_ = State::wait;
    DynamicMemory<Context> ctx_;
};



} // namespace skyland
