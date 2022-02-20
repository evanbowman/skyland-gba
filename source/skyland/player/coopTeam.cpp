#include "coopTeam.hpp"
#include "skyland/island.hpp"



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
    auto metac = load_metaclass(packet.metaclass_index_.get());

    Vec2<u8> pos{packet.x_, packet.y_};

    (*metac)->create(pfrm, app, &player_island(app), pos);
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::WeaponSetTarget& packet)
{
    if (packet.weapon_near_) {
        if (auto room = player_island(app).get_room({packet.weapon_x_,
                packet.weapon_y_})) {

            room->set_target(pfrm, app, {packet.target_x_, packet.target_y_});
        }
    } else {
        // TODO...
    }
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomSalvaged& packet)
{
    // TODO...
}



}
