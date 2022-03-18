#pragma once

#include "playerP1.hpp"
#include "skyland/network.hpp"



namespace skyland
{



class CoopTeam : public PlayerP1, public network::Listener
{
public:
    // TODO:
    // drone_set_target,
    // drone_spawn,
    // drone_destroyed,


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
                 const network::packet::CoopCursor& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CoopRngSync& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomSalvaged& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterSetTarget& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterDied& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterBoarded& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterDisembark& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ReplicantCreated& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::SetWeaponGroup& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::TerrainConstructed& packet) override;


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


    void network_sync_cursor(Platform& pfrm,
                             const Vec2<u8>& cursor,
                             u8 cursor_icon,
                             bool near) override;

private:
    static const auto heartbeat_interval = seconds(5);
    Microseconds heartbeat_send_counter_ = 0;
    Microseconds heartbeat_recv_counter_ = 0;
};



} // namespace skyland
