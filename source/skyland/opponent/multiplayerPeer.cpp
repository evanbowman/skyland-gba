#include "multiplayerPeer.hpp"
#include "skyland/skyland.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



void MultiplayerPeer::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.network_peer().is_connected()) {
        network::poll_messages(pfrm, app, *this);
    } else {
        // hmm... we should do something here.
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomConstructed& packet)
{
    auto metac = load_metaclass(packet.metaclass_index_.get());

    if (app.opponent_island()) {
        (*metac)->create(pfrm, &*app.opponent_island(), {packet.x_, packet.y_});
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomSalvaged& packet)
{
    if (app.opponent_island()) {
        app.opponent_island()->destroy_room(pfrm, {packet.x_, packet.y_});
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::TerrainConstructed& packet)
{
    if (app.opponent_island()) {
        // TODO...
    }
}



}
