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


#include "opponent.hpp"
#include "skyland/network.hpp"



namespace skyland
{



class MultiplayerPeer : public Opponent, public network::Listener
{
public:
    void update(Time delta) override;


    void receive(const network::packet::RoomConstructed& packet) override;


    void receive(const network::packet::RoomSalvaged& packet) override;


    void receive(const network::packet::WeaponSetTarget& packet) override;


    void receive(const network::packet::DroneSetTarget& packet) override;


    void receive(const network::packet::RoomDestroyed& packet) override;


    void receive(const network::packet::ChrSetTargetV2& packet) override;


    void receive(const network::packet::ChrBoardedV2& packet) override;


    void receive(const network::packet::ChrDisembarkV2& packet) override;


    void receive(const network::packet::ChrDiedV2& packet) override;


    void receive(const network::packet::ReplicantCreated& packet) override;


    void receive(const network::packet::DroneSpawn& packet) override;


    void receive(const network::packet::DroneDestroyed& packet) override;


    void
    receive(const network::packet::OpponentBulkheadChanged& packet) override;


    void receive(const network::packet::ProgramVersion& packet) override;


    void receive(const network::packet::Heartbeat& packet) override;


    void receive(const network::packet::DynamiteActivated& packet) override;


    void receive(const network::packet::PlayMusic& packet) override;


private:
    static const auto heartbeat_interval = seconds(5);
    Time heartbeat_send_counter_ = 0;
    Time heartbeat_recv_counter_ = 0;
};



} // namespace skyland
