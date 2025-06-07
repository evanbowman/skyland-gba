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


#include "allocator.hpp"
#include "macroverseScene.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/macrocosmSector.hpp"
#include "skyland/network.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



class ExchangeColonyScene : public Scene, public network::Listener
{
public:
    struct Schema
    {
        struct Payload
        {
            terrain::Sector::Orientation orientation_;
            terrain::Sector::Shape shape_;
            char name_[terrain::Sector::name_len];
            u8 population_[sizeof(Population)];
            u8 blocks_[576]; // Max blocks in a sector
        } payload_;

        u8 reserved_[1024 - sizeof(Payload)];
    };
    static_assert(sizeof(Schema) == 1024);


    ExchangeColonyScene(Vec2<s8> exchange_sector)
        : data_in_(allocate_dynamic<Schema>("exchange_colony_buffer")),
          data_out_(allocate_dynamic<Schema>("exchange_colony_buffer")),
          input_ptr_(reinterpret_cast<u8*>(&*data_in_)),
          exchange_sector_(exchange_sector)
    {
        output_ptr_ = reinterpret_cast<u8*>(&*data_out_);
        output_size_ = sizeof(Schema);

        static_assert(sizeof(Schema) % 4 == 0);
    }


    ScenePtr update(Time) override
    {
        auto& m = skyland::macrocosm();

        PLATFORM.screen().fade(1.f);

        if (PLATFORM.network_peer().is_connected()) {
            network::poll_messages(*this);
        }

        switch (state_) {
        case State::init: {

            auto s = m.load_sector(exchange_sector_);

            PLATFORM.network_peer().listen();
            if (not PLATFORM.network_peer().is_connected()) {
                return make_scene<MacroverseScene>(true);
            }

            data_out_->payload_.orientation_ = s->orientation();
            data_out_->payload_.shape_ = s->persistent().shape_;

            memcpy(data_out_->payload_.name_,
                   s->persistent().name_,
                   terrain::Sector::name_len);

            memcpy(data_out_->payload_.population_,
                   &s->persistent().population_,
                   sizeof data_out_->payload_.population_);

            Buffer<u8, 576> blocks;
            for (u8 z = 0; z < s->size().z; ++z) {
                for (u8 x = 0; x < s->size().x; ++x) {
                    for (u8 y = 0; y < s->size().y; ++y) {
                        blocks.push_back(s->get_block({x, y, z}).type_);
                    }
                }
            }

            memcpy(data_out_->payload_.blocks_,
                   blocks.data(),
                   sizeof data_out_->payload_.blocks_);

            state_ = State::send;
            break;
        }

        case State::send: {
            if (output_ptr_ <
                reinterpret_cast<u8*>(&*data_out_) + output_size_) {
                network::packet::BlockTransferData pkt;

                auto send_segment = [&] {
                    pkt.sequence_ = output_sequence_++;
                    pkt.data_[0] = *(output_ptr_++);
                    pkt.data_[1] = *(output_ptr_++);
                    pkt.data_[2] = *(output_ptr_++);
                    pkt.data_[3] = *(output_ptr_++);
                    network::transmit(pkt);
                };

                // NOTE: we don't want to overflow the send queue, don't
                // transmit too many packets all at once (four per frame).

                send_segment();
                send_segment();

                static_assert(sizeof(Schema) % 8 == 0);

            } else {
                network::packet::BlockTransferEnd pkt;
                network::transmit(pkt);
                state_ = State::wait;
            }
            break;
        }

        case State::wait: {
            if (receive_complete_) {
                network::packet::MacroTradeStatus pkt;
                // NOTE: we do not want to allow the trade to occur if we have
                // no room to store the received island. We allow twenty large
                // islands and fourty small ones (at time of writing), if the
                // recieved island would not fit, we have to cancel the
                // trade. This comment is only relevant when trading a tiny
                // island (outpost) for a fullsize island, as they're stored in
                // different containers.
                pkt.status_ = not m.data_->other_sectors_.full();
                network::transmit(pkt);
                state_ = State::await_status;
            }
            break;
        }

        case State::await_status: {
            if (result_status_ == 1) {
                state_ = State::done;
            } else if (result_status_ == 0) {
                return make_scene<MacroverseScene>(true);
            }
            break;
        }

        case State::done: {
            auto rx = reinterpret_cast<Schema*>(&*data_in_);
            StringBuffer<11> name;
            for (char c : rx->payload_.name_) {
                name.push_back(c);
            }

            m.erase_sector(exchange_sector_);

            if (auto s = m.make_sector(exchange_sector_, rx->payload_.shape_)) {
                s->set_name(name);
                for (u8 z = 0; z < s->size().z; ++z) {
                    for (u8 x = 0; x < s->size().x; ++x) {
                        for (u8 y = 0; y < s->size().y; ++y) {
                            const auto ind = z * s->size().x * s->size().y +
                                             x * s->size().y + y;

                            auto b = (terrain::Type)rx->payload_.blocks_[ind];

                            if (b == terrain::Type::selector) {
                                s->set_block({x, y, z}, terrain::Type::air);
                            } else {
                                s->set_block({x, y, z}, b);
                            }
                        }
                    }
                }
                Population population;
                memcpy((u8*)&population,
                       rx->payload_.population_,
                       sizeof rx->payload_.population_);

                s->set_population(population);
            }

            PLATFORM.network_peer().disconnect();

            // TODO: m.save();

            return make_scene<MacroverseScene>(true);
        }
        }

        return null_scene();
    }



    void receive(const network::packet::BlockTransferData& p) override
    {
        auto start = input_ptr_ + p.sequence_ * 4;

        *(start++) = p.data_[0];
        *(start++) = p.data_[1];
        *(start++) = p.data_[2];
        *(start++) = p.data_[3];
    }


    void receive(const network::packet::BlockTransferEnd& p) override
    {
        receive_complete_ = true;
    }


    void receive(const network::packet::MacroTradeStatus& p) override
    {
        if (p.status_ > 1) {
            // For future extensions. In case I add more descriptive error
            // codes, maintian backwards compatibility by treating >1 as
            // erroneous.
            result_status_ = 0;
        } else {
            result_status_ = p.status_;
        }
    }


private:
    enum class State {
        init,
        send,
        wait,
        await_status,
        done,
    } state_ = State::init;

    DynamicMemory<Schema> data_in_;
    DynamicMemory<Schema> data_out_;

    u8* const input_ptr_;
    u8* output_ptr_;

    u32 output_size_ = 0;
    int output_sequence_ = 0;

    bool receive_complete_ = false;
    int result_status_ = -1;

    Vec2<s8> exchange_sector_;
};



} // namespace skyland::macro
