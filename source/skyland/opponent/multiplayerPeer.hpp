#pragma once


#include "opponent.hpp"
#include "skyland/network.hpp"



namespace skyland {



class Cannon;
class MissileSilo;



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

};



} // namespace skyland
