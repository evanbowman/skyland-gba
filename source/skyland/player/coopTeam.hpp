#pragma once

#include "playerP1.hpp"
#include "skyland/network.hpp"



namespace skyland {



class CoopTeam : public PlayerP1, public network::Listener {
public:

    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomConstructed& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::WeaponSetTarget& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::RoomSalvaged& packet) override;


};



}
