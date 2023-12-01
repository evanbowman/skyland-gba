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


#include "opponent.hpp"
#include "skyland/network.hpp"



namespace skyland
{



class MultiplayerPeer : public Opponent, public network::Listener
{
public:
    void update(Microseconds delta) override;


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
    Microseconds heartbeat_send_counter_ = 0;
    Microseconds heartbeat_recv_counter_ = 0;
};



} // namespace skyland
