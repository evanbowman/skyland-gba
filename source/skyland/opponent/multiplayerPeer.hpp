#pragma once


#include "opponent.hpp"
#include "skyland/network.hpp"



namespace skyland {



class MultiplayerPeer : public Opponent, public network::Listener {
public:
    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomConstructed& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomSalvaged& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::WeaponSetTarget& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DroneSetTarget& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomDestroyed& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterSetTarget& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterBoarded& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterDisembark& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::CharacterDied& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ReplicantCreated& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DroneSpawn& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::DroneDestroyed& packet) override;


    void
    receive(Platform& pfrm,
            App& app,
            const network::packet::OpponentBulkheadChanged& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::ProgramVersion& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::Heartbeat& packet) override;


private:

    static const auto heartbeat_interval = seconds(5);
    Microseconds heartbeat_send_counter_ = 0;
    Microseconds heartbeat_recv_counter_ = 0;
};



} // namespace skyland
