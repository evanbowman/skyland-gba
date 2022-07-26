////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "multiplayerPeer.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/tnt.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/skyland.hpp"
#include "version.hpp"



namespace skyland
{



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
        RoomCoord pos{packet.x_, packet.y_};

        pos.x = ((app.opponent_island()->terrain().size() - 1) - pos.x) -
                (size.x - 1);

        (*metac)->create(pfrm, app, app.opponent_island(), pos);
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

            room->set_target(
                pfrm,
                app,
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
        island = app.opponent_island();
    } else {
        island = &app.player_island();
    }

    const auto drone_x = invert_axis(app, packet.drone_x_);
    if (auto drone = island->get_drone({drone_x, packet.drone_y_})) {
        (*drone)->set_target(
            pfrm,
            app,
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
            island = app.opponent_island();
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
                room->apply_damage(pfrm, app, Room::health_upper_limit());
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::ChrSetTargetV2& packet)
{
    Island* island = nullptr;

    if (packet.near_island_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island) {
        const RoomCoord dst_coord{invert_axis(app, packet.target_x_),
                                  packet.target_y_};

        auto info = island->find_character_by_id(packet.chr_id_.get());

        if (info.first) {
            auto path = find_path(
                pfrm, app, island, info.first->grid_position(), dst_coord);
            if (path and *path) {
                info.first->set_movement_path(pfrm, app, std::move(*path));
                return;
            }
        } else {
            auto str = format("chr id % not found!", packet.chr_id_.get());
            for (auto& room : island->rooms()) {
                for (auto& chr : room->characters()) {
                    str += " ";
                    str += stringify(chr->id());
                }
            }
            Platform::fatal(str.c_str());
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::ChrBoardedV2& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const auto dst = RoomCoord{invert_axis(app, packet.dst_x_),
                               packet.dst_y_};

    auto source_island =
        packet.transporter_near_ ? opponent_island(app) : &player_island(app);

    if (auto room = source_island->get_room(
            {invert_axis(app, packet.transporter_x_), packet.transporter_y_})) {
        if (auto t = dynamic_cast<Transporter*>(room)) {
            t->begin_recharge();
            if (t->parent()->interior_visible()) {
                t->parent()->schedule_repaint();
            }
        }
    }

    auto dest_island =
        packet.transporter_near_ ? &player_island(app) : opponent_island(app);

    transport_character_impl(
        app, source_island, dest_island, packet.chr_id_.get(), dst);
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::ChrDisembarkV2& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const auto dst = RoomCoord{invert_axis(app, packet.dst_x_), packet.dst_y_};

    auto dest_island =
        packet.transporter_near_ ? opponent_island(app) : &player_island(app);

    if (auto room = dest_island->get_room(
            {invert_axis(app, packet.transporter_x_), packet.transporter_y_})) {
        if (auto t = dynamic_cast<Transporter*>(room)) {
            t->begin_recharge();
            if (t->parent()->interior_visible()) {
                t->parent()->schedule_repaint();
            }
        }
    }

    auto source_island =
        packet.transporter_near_ ? &player_island(app) : opponent_island(app);

    transport_character_impl(
        app, source_island, dest_island, packet.chr_id_.get(), dst);

}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::ChrDiedV2& packet)
{
    Island* island = nullptr;

    if (packet.near_island_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island) {

        auto found = island->find_character_by_id(packet.chr_id_.get());

        if (found.first) {
            // kill character
            found.first->apply_damage(pfrm, app, BasicCharacter::max_health);
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

    const RoomCoord loc = {invert_axis(app, packet.src_x_), packet.src_y_};

    auto chr = app.alloc_entity<BasicCharacter>(
        pfrm, app.opponent_island(), &app.opponent(), loc, true);

    chr->__assign_id(packet.chr_id_.get());

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
        island = app.opponent_island();
    } else {
        island = &app.player_island();
    }

    const auto x_origin = invert_axis(app, packet.origin_x_);

    auto [dt, ds] = drone_metatable();
    if (packet.drone_class_ >= ds) {
        StringBuffer<32> err("invalid index! ");
        err += stringify(packet.drone_class_);
        pfrm.fatal(err.c_str());
    }
    auto drone_meta = &dt[packet.drone_class_];
    if (auto drone = (*drone_meta)
                         ->create(app.opponent_island(),
                                  island,
                                  RoomCoord{x_origin, packet.origin_y_})) {

        const RoomCoord drone_bay_pos = {x_origin, u8(packet.origin_y_ + 1)};

        if (auto room = app.opponent_island()->get_room(drone_bay_pos)) {

            if (room->attach_drone(pfrm, app, *drone)) {
                island->drones().push(*drone);
            }

            (*drone)->set_movement_target(RoomCoord{
                invert_axis(app, packet.deploy_x_), packet.deploy_y_});
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::DroneDestroyed& packet)
{
    Island* island = nullptr;
    if (packet.destination_near_) {
        island = app.opponent_island();
    } else {
        island = &app.player_island();
    }

    RoomCoord pos;
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

    const RoomCoord loc = {invert_axis(app, packet.room_x_), packet.room_y_};

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
        err += stringify(packet.major_.get());
        err += ".";
        err += stringify(packet.minor_);
        err += ".";
        err += stringify(packet.subminor_);
        err += ".";
        err += stringify(packet.revision_);

        pfrm.fatal(err.c_str());
    }
}


void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::Heartbeat& packet)
{
    heartbeat_recv_counter_ = 0;
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::DynamiteActivated& packet)
{
    if (app.opponent_island()) {
        RoomCoord pos{invert_axis(app, packet.x_), packet.y_};

        if (auto room = app.opponent_island()->get_room(pos)) {
            // We technically don't really need to do a cast here, as we don't
            // need anything from the Explosive class. Just a sanity check, as
            // we shouldn't simply blindly trust whatever the linked game tells
            // us.
            if (dynamic_cast<Explosive*>(room)) {
                // Selecting an explosive starts off a countdown by applying one
                // damage point. We want to do the same thing here.
                room->apply_damage(pfrm, app, 1);
            } else {
                // TODO: fatal error?
            }
        }
    }
}



void MultiplayerPeer::receive(Platform& pfrm,
                              App& app,
                              const network::packet::PlayMusic& packet)
{
    switch (packet.music_id_.get()) {
    case 0:
        pfrm.speaker().play_music("life_in_silco", 0);
        break;

    case 1:
        pfrm.speaker().play_music("solecism", 0);
        break;
    }
}



} // namespace skyland
