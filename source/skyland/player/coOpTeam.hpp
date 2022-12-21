////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomConstructed& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomDestroyed& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::WeaponSetTarget& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpCursor& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomSalvaged& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ChrSetTargetV2& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ChrDiedV2& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ChrBoardedV2& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ChrDisembarkV2& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ReplicantCreated& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::SetWeaponGroup& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::TerrainConstructed& packet) override;


    void
    receive(Platform& pfrm,
            App& app,
            const network::packet::TerrainConstructedLeft& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DynamiteActivated& packet) override;


    void
    receive(Platform& pfrm,
            App& app,
            const network::packet::OpponentBulkheadChanged& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DroneSpawn& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DroneDestroyed&) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DroneSetTarget&) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::Heartbeat& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpRoomLockAcquire& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpRoomLockRelease& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpRoomLockResponse& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpChrLockAcquire& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpChrLockRelease& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpChrLockResponse& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoOpOpponentDestroyed&) override;


    void
    receive(Platform& pfrm, App& app, const network::packet::Paused&) override;


    void network_sync_cursor(Platform& pfrm,
                             const RoomCoord& cursor,
                             u8 cursor_icon,
                             bool near) override;

private:
    static const auto heartbeat_interval = seconds(5);
    Microseconds heartbeat_send_counter_ = 0;
    Microseconds heartbeat_recv_counter_ = 0;
};



} // namespace skyland
