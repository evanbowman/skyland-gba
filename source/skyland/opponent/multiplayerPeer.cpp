#include "multiplayerPeer.hpp"
#include "localization.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/skyland.hpp"
#include "version.hpp"



namespace skyland {



void MultiplayerPeer::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (pfrm.network_peer().is_connected()) {
        network::poll_messages(pfrm, app, *this);
    } else {
        pfrm.fatal("connection interrupted");
    }

    heartbeat_send_counter_ += delta;
    heartbeat_recv_counter_ += delta;

    if (heartbeat_send_counter_ > heartbeat_interval) {
        heartbeat_send_counter_ = 0;

        network::packet::Heartbeat heartbeat;
        network::transmit(pfrm, heartbeat);
    }

    if (heartbeat_recv_counter_ > heartbeat_interval * 2) {
        pfrm.fatal("connection interrupted");
    }
}



// The engine always puts the player on the left side of the map, and the enemy
// on the right side of the map. So we need to flip all x coordinates to
// transform the other console's coordinate system to our own coordinate system.
static u8 invert_axis(App& app, u8 x_coord)
{
    // NOTE: Terrain should be guaranteed to be the same size for each
    // multiplayer peer. Our multiplayer setup code syncs the configured terrain
    // size between games, and players are not allowed to build terrain in
    // multiplayer mode.
    return (app.player_island().terrain().size() - 1) - x_coord;
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomConstructed& packet)
{
    auto metac = load_metaclass(packet.metaclass_index_.get());

    if (app.opponent_island()) {
        const auto size = (*metac)->size();
        Vec2<u8> pos{packet.x_, packet.y_};

        pos.x = ((app.opponent_island()->terrain().size() - 1) - pos.x) -
                (size.x - 1);

        (*metac)->create(pfrm, app, &*app.opponent_island(), pos);
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomSalvaged& packet)
{
    if (app.opponent_island()) {
        app.opponent_island()->destroy_room(
            pfrm, app, {invert_axis(app, packet.x_), packet.y_});
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::WeaponSetTarget& packet)
{
    if (app.opponent_island()) {
        if (auto room = app.opponent_island()->get_room(
                {invert_axis(app, packet.weapon_x_), packet.weapon_y_})) {

            room->set_target(pfrm, app,
                             {invert_axis(app, packet.target_x_), packet.target_y_});
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::DroneSetTarget& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    Island* island = nullptr;

    if (packet.drone_near_) {
        island = &*app.opponent_island();
    } else {
        island = &app.player_island();
    }

    const auto drone_x = invert_axis(app, packet.drone_x_);
    if (auto drone = island->get_drone({drone_x, packet.drone_y_})) {
        (*drone)->set_target(
            {invert_axis(app, packet.target_x_), packet.target_y_},
            not packet.target_near_);
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::RoomDestroyed& packet)
{
    Island* island = nullptr;

    if (packet.near_island_) {
        island = &app.player_island();
    } else {
        if (app.opponent_island()) {
            island = &*app.opponent_island();
        }
    }

    if (island) {
        // Ok, so if the game's clocks are sufficently in sync, then the room
        // should already have been destroyed on both consoles by the time this
        // message is received. We're only sending this message in the first
        // place in case stuff gets out of sync.
        if (auto room = island->get_room(
                {invert_axis(app, packet.room_x_), packet.room_y_})) {
            // We want to at least make sure that the destroyed room is an
            // instance of the same class as the room at the target
            // coordinates. This clears up a few edge-cases.
            if (str_cmp(
                    (*room->metaclass())->name(),
                    (*load_metaclass(packet.metaclass_index_.get()))->name()) ==
                0) {
                room->apply_damage(pfrm, app, 9999);
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::CharacterSetTarget& packet)
{
    Island* island = nullptr;

    if (packet.near_island_) {
        island = &app.player_island();
    } else {
        if (app.opponent_island()) {
            island = &*app.opponent_island();
        }
    }

    if (island) {
        const Vec2<u8> src_coord{invert_axis(app, packet.src_x_),
                                 packet.src_y_};
        const Vec2<u8> dst_coord{invert_axis(app, packet.dst_x_),
                                 packet.dst_y_};

        if (auto room = island->get_room(src_coord)) {
            for (auto& chr : room->characters()) {
                if (chr->grid_position() == src_coord and
                    chr->owner() not_eq &app.player()) {

                    auto path =
                        find_path(pfrm, app, island, src_coord, dst_coord);

                    if (path and *path) {
                        chr->set_movement_path(std::move(*path));
                        return;
                    }
                }
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::CharacterBoarded& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    const auto src = Vec2<u8>{invert_axis(app, packet.src_x_), packet.src_y_};
    const auto dst = Vec2<u8>{invert_axis(app, packet.dst_x_), packet.dst_y_};

    if (auto room = app.opponent_island()->get_room(src)) {
        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            if ((*it)->grid_position() == src and
                (*it)->owner() not_eq &app.player()) {

                auto unlinked = std::move(*it);
                room->characters().erase(it);

                unlinked->set_grid_position(dst);
                unlinked->set_parent(&app.player_island());
                unlinked->transported();

                if (auto dst_room = app.player_island().get_room(dst)) {
                    dst_room->characters().push(std::move(unlinked));
                }

                return;

            } else {
                ++it;
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::CharacterDisembark& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    // TODO: pretty much identical to the code above for the CharacterBoarded
    // event, refactor this stuff to one function.x

    const auto src = Vec2<u8>{invert_axis(app, packet.src_x_), packet.src_y_};
    const auto dst = Vec2<u8>{invert_axis(app, packet.dst_x_), packet.dst_y_};

    if (auto room = app.player_island().get_room(src)) {
        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            if ((*it)->grid_position() == src and
                (*it)->owner() not_eq &app.player()) {

                auto unlinked = std::move(*it);
                room->characters().erase(it);

                unlinked->set_grid_position(dst);
                unlinked->set_parent(&*app.opponent_island());
                unlinked->transported();

                if (auto dst_room = app.opponent_island()->get_room(dst)) {
                    dst_room->characters().push(std::move(unlinked));
                }

                return;
            } else {
                ++it;
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::CharacterDied& packet)
{
    Island* island = nullptr;

    if (packet.near_island_) {
        island = &app.player_island();
    } else {
        if (app.opponent_island()) {
            island = &*app.opponent_island();
        }
    }

    const Vec2<u8> chr_loc = {invert_axis(app, packet.chr_x_), packet.chr_y_};

    if (island) {
        if (auto room = island->get_room(chr_loc)) {
            for (auto it = room->characters().begin();
                 it not_eq room->characters().end();) {

                const bool owned_by_player = (*it)->owner() == &app.player();
                if ((*it)->grid_position() == chr_loc and
                    packet.chr_owned_by_player_ == owned_by_player and
                    (*it)->health() < 20) {
                    // FIXME: Added the < 20 heuristic in case another character
                    // happens to be walking to be walking through the slot at
                    // the same that the character dies. Eventually, we should
                    // associate unique identifiers with all characters.
                    (*it)->apply_damage(pfrm, app, 20);
                    return;
                } else {
                    ++it;
                }
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::ReplicantCreated& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    const Vec2<u8> loc = {invert_axis(app, packet.src_x_), packet.src_y_};

    auto chr = alloc_entity<BasicCharacter>(
        &*app.opponent_island(), &app.opponent(), loc, true);

    if (chr) {
        chr->apply_damage(pfrm, app, 255 - packet.health_);
        chr->transported();
        app.opponent_island()->add_character(std::move(chr));
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::DroneSpawn& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    Island* island = nullptr;
    if (packet.destination_near_) {
        island = &*app.opponent_island();
    } else {
        island = &app.player_island();
    }

    const auto x_origin = invert_axis(app, packet.origin_x_);

    auto [dt, ds] = drone_metatable();
    if (packet.drone_class_ >= ds) {
        StringBuffer<32> err("invalid index! ");
        err += to_string<10>(packet.drone_class_);
        pfrm.fatal(err.c_str());
    }
    auto drone_meta = &dt[packet.drone_class_];
    if (auto drone = (*drone_meta)
                         ->create(&*app.opponent_island(),
                                  island,
                                  Vec2<u8>{x_origin, packet.origin_y_})) {

        const Vec2<u8> drone_bay_pos = {x_origin, u8(packet.origin_y_ + 1)};

        if (auto room = app.opponent_island()->get_room(drone_bay_pos)) {
            if (auto db = dynamic_cast<DroneBay*>(room)) {
                db->attach_drone(pfrm, app, *drone);
                db->start_reload();
                island->drones().push(*drone);
                (*drone)->set_movement_target(Vec2<u8>{
                    invert_axis(app, packet.deploy_x_), packet.deploy_y_});
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::DroneDestroyed& packet)
{
    Island* island = nullptr;
    if (packet.destination_near_) {
        island = &*app.opponent_island();
    } else {
        island = &app.player_island();
    }

    Vec2<u8> pos;
    pos.x = invert_axis(app, packet.drone_x_);
    pos.y = packet.drone_y_;

    if (auto drone = island->get_drone(pos)) {
        if (drone) {
            (*drone)->kill();
        }
    }
}



void MultiplayerPeer::receive(
    Platform& pfrm,
    App& app,
    const network::packet::OpponentBulkheadChanged& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    const Vec2<u8> loc = {invert_axis(app, packet.room_x_), packet.room_y_};

    if (auto room = app.opponent_island()->get_room(loc)) {
        if (auto bulkhead = dynamic_cast<Bulkhead*>(room)) {
            bulkhead->set_open(pfrm, app, packet.open_);
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::ProgramVersion& packet)
{
    if (packet.major_.get() not_eq PROGRAM_MAJOR_VERSION or
        packet.minor_ not_eq PROGRAM_MINOR_VERSION or
        packet.subminor_ not_eq PROGRAM_SUBMINOR_VERSION or
        packet.revision_ not_eq PROGRAM_VERSION_REVISION) {

        StringBuffer<48> err;
        err += "got incompatible program version ";
        err += to_string<8>(packet.major_.get());
        err += ".";
        err += to_string<8>(packet.minor_);
        err += ".";
        err += to_string<8>(packet.subminor_);
        err += ".";
        err += to_string<8>(packet.revision_);

        pfrm.fatal(err.c_str());
    }
}


void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::Heartbeat& packet)
{
    heartbeat_recv_counter_ = 0;
}



} // namespace skyland
