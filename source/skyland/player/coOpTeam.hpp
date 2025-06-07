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

#include "playerP1.hpp"
#include "skyland/network.hpp"



namespace skyland
{



class CoOpTeam : public PlayerP1, public network::Listener
{
public:
    void update(Time delta) override;


    void receive(const network::packet::RoomConstructed& packet) override;


    void receive(const network::packet::RoomDestroyed& packet) override;


    void receive(const network::packet::WeaponSetTarget& packet) override;


    void receive(const network::packet::CoOpCursor& packet) override;


    void receive(const network::packet::RoomSalvaged& packet) override;


    void receive(const network::packet::ChrSetTargetV2& packet) override;


    void receive(const network::packet::ChrDiedV2& packet) override;


    void receive(const network::packet::ChrBoardedV2& packet) override;


    void receive(const network::packet::ChrDisembarkV2& packet) override;


    void receive(const network::packet::ReplicantCreated& packet) override;


    void receive(const network::packet::SetWeaponGroup& packet) override;


    void receive(const network::packet::TerrainConstructed& packet) override;


    void
    receive(const network::packet::TerrainConstructedLeft& packet) override;


    void receive(const network::packet::DynamiteActivated& packet) override;


    void
    receive(const network::packet::OpponentBulkheadChanged& packet) override;


    void receive(const network::packet::DroneSpawn& packet) override;


    void receive(const network::packet::DroneDestroyed&) override;


    void receive(const network::packet::DroneSetTarget&) override;


    void receive(const network::packet::Heartbeat& packet) override;


    void receive(const network::packet::CoOpRoomLockAcquire& packet) override;


    void receive(const network::packet::CoOpRoomLockRelease& packet) override;


    void receive(const network::packet::CoOpRoomLockResponse& packet) override;


    void receive(const network::packet::CoOpChrLockAcquire& packet) override;


    void receive(const network::packet::CoOpChrLockRelease& packet) override;


    void receive(const network::packet::CoOpChrLockResponse& packet) override;


    void receive(const network::packet::CoOpOpponentDestroyed&) override;


    void receive(const network::packet::Paused&) override;


    void network_sync_cursor(const RoomCoord& cursor,
                             u8 cursor_icon,
                             bool near) override;

protected:
    void update_ai(Time delta) override
    {
    }

private:
    static const auto heartbeat_interval = seconds(5);
    Time heartbeat_send_counter_ = 0;
    Time heartbeat_recv_counter_ = 0;
};



} // namespace skyland
