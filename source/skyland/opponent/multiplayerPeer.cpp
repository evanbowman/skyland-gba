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



static u8 invert_axis(App& app, u8 x_coord)
{
    return (app.opponent_island()->terrain().size() - 1) - x_coord;
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomConstructed& packet)
{
    auto metac = load_metaclass(packet.metaclass_index_.get());

    if (app.opponent_island()) {
        const auto size = (*metac)->size();
        Vec2<u8> pos{packet.x_, packet.y_};

        pos.x = ((app.opponent_island()->terrain().size() - 1) - pos.x) - (size.x - 1);

        (*metac)->create(pfrm, &*app.opponent_island(), pos);
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomSalvaged& packet)
{
    if (app.opponent_island()) {
        app.opponent_island()->destroy_room(pfrm,
                                            {invert_axis(app, packet.x_),
                                             packet.y_});
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::WeaponSetTarget& packet)
{
    if (app.opponent_island()) {
        if (auto room = app.opponent_island()->get_room({
                    invert_axis(app, packet.weapon_x_),
                    packet.weapon_y_
                })) {

            room->set_target({
                    invert_axis(app, packet.target_x_),
                    packet.target_y_
                });
        }
    }
}



}
