#include "coopTeam.hpp"



namespace skyland {



void CoopTeam::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.network_peer().is_connected()) {
        network::poll_messages(pfrm, app, *this);
    } else {
        pfrm.fatal("connection interrupted");
    }

    PlayerP1::update(pfrm, app, delta);
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomConstructed& packet)
{
    // TODO...
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomSalvaged& packet)
{
    // TODO...
}



}
