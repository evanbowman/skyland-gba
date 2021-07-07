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
                 const network::packet::OpponentBulkheadChanged& packet) override;


private:
    Microseconds sync_micros_ = 0;
    int sync_seconds_ = 0;

    static const auto timekeeper_sync_rate = seconds(2);
    Microseconds next_timekeeper_sync_ = timekeeper_sync_rate;
};



} // namespace skyland
