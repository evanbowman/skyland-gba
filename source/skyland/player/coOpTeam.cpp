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


#include "coOpTeam.hpp"
#include "number/random.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/island.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/tnt.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/scene/multiplayerCoOpAwaitLockScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void CoOpTeam::update(Platform& pfrm, App& app, Microseconds delta)
{
    PlayerP1::update(pfrm, app, delta);

    if (pfrm.network_peer().is_connected()) {
        network::poll_messages(pfrm, app, *this);
    } else {
        info(pfrm, "lost connection to co-op peer!");

        // Lost connection to peer player!
        app.swap_player<PlayerP1>();
        app.game_mode() = App::GameMode::skyland_forever;
        return;
    }

    heartbeat_send_counter_ += delta;
    heartbeat_recv_counter_ += delta;

    if (heartbeat_send_counter_ > heartbeat_interval) {
        heartbeat_send_counter_ = 0;

        network::packet::Heartbeat heartbeat;
        network::transmit(pfrm, heartbeat);
    }

    if (heartbeat_recv_counter_ > heartbeat_interval * 2) {
        pfrm.network_peer().disconnect();
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::Heartbeat& packet)
{
    heartbeat_recv_counter_ = 0;
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomConstructed& packet)
{
    if (auto room = app.player_island().get_room({packet.x_, packet.y_})) {
        // Co-op player managed to construct a room in exactly the same slot as
        // one of our own, tell the other player that the room was destroyed.
        network::packet::RoomDestroyed d;
        d.room_x_ = packet.x_;
        d.room_y_ = packet.y_;
        d.near_island_ = false;
        d.metaclass_index_.set(packet.metaclass_index_.get());
        network::transmit(pfrm, d);

        // Destroy our own copy, as it overlaps with the other player's.
        room->apply_damage(pfrm, app, Room::health_upper_limit());
    }

    auto metac = load_metaclass(packet.metaclass_index_.get());

    RoomCoord pos{packet.x_, packet.y_};

    (*metac)->create(pfrm, app, &player_island(app), pos);
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::WeaponSetTarget& packet)
{
    if (packet.weapon_near_) {
        if (auto room = player_island(app).get_room(
                {packet.weapon_x_, packet.weapon_y_})) {

            room->set_target(pfrm, app, {packet.target_x_, packet.target_y_});
        }
    } else {
        if (not opponent_island(app)) {
            // TODO: raise error?
            return;
        }
        if (auto room = opponent_island(app)->get_room(
                {packet.weapon_x_, packet.weapon_y_})) {

            room->set_target(pfrm, app, {packet.target_x_, packet.target_y_});
        }
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpCursor& packet)
{
    std::get<SkylandGlobalData>(globals()).co_op_cursor_.x = packet.x_;
    std::get<SkylandGlobalData>(globals()).co_op_cursor_.y = packet.y_;
    std::get<SkylandGlobalData>(globals()).co_op_cursor_near_ = packet.near_;

    std::get<SkylandGlobalData>(globals()).co_op_cursor_icon_ = [&] {
        switch (packet.icon_) {
        default:
            // Why not simply add an offset? I guess I could... But doing so
            // would put some dependency on the ordering of tiles in vram,
            // which is guaranteed to be the same, at least for now, but who
            // knows how I might change things in the future. So anyway, I
            // want to create a separation between what constitutes an icon
            // and a vram tile offset.
        case 0:
            return 30;

        case 1:
            return 31;

        case 2:
            return 32;

        case 3:
            return 33;
        }
    }();
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::DynamiteActivated& packet)
{
    RoomCoord pos{packet.x_, packet.y_};

    if (auto room = player_island(app).get_room(pos)) {
        if (dynamic_cast<Explosive*>(room)) {
            room->apply_damage(pfrm, app, 1);
        } else {
            // TODO: fatal error?
        }
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::SetWeaponGroup& packet)
{
    if (auto room = player_island(app).get_room({packet.x_, packet.y_})) {
        room->set_group((Room::Group)packet.group_);
        player_island(app).show_groups(true);
        player_island(app).repaint(pfrm, app);
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomSalvaged& packet)
{
    player_island(app).destroy_room(pfrm, app, {packet.x_, packet.y_});
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::ChrDiedV2& packet)
{
    Island* island = nullptr;

    if (not packet.near_island_) {
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



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::ChrBoardedV2& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const auto dst = RoomCoord{packet.dst_x_, packet.dst_y_};

    auto source_island =
        packet.transporter_near_ ? &player_island(app) : opponent_island(app);

    if (auto room = source_island->get_room(
            {packet.transporter_x_, packet.transporter_y_})) {
        if (auto t = dynamic_cast<Transporter*>(room)) {
            t->begin_recharge();
            if (t->parent()->interior_visible()) {
                t->parent()->schedule_repaint();
            }
        }
    }

    auto dest_island =
        packet.transporter_near_ ? opponent_island(app) : &player_island(app);

    transport_character_impl(
        app, source_island, dest_island, packet.chr_id_.get(), dst);
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::ChrDisembarkV2& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const auto dst = RoomCoord{packet.dst_x_, packet.dst_y_};

    auto dest_island =
        packet.transporter_near_ ? &player_island(app) : opponent_island(app);

    if (auto room = dest_island->get_room(
            {packet.transporter_x_, packet.transporter_y_})) {
        if (auto t = dynamic_cast<Transporter*>(room)) {
            t->begin_recharge();
            if (t->parent()->interior_visible()) {
                t->parent()->schedule_repaint();
            }
        }
    }

    auto source_island =
        packet.transporter_near_ ? opponent_island(app) : &player_island(app);

    transport_character_impl(
        app, source_island, dest_island, packet.chr_id_.get(), dst);
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::ChrSetTargetV2& packet)
{
    Island* island = nullptr;

    if (not packet.near_island_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island) {
        const RoomCoord dst_coord{packet.target_x_, packet.target_y_};

        auto info = island->find_character_by_id(packet.chr_id_.get());

        if (info.first) {
            auto path = find_path(
                pfrm, app, island, info.first->grid_position(), dst_coord);
            if (path and *path) {
                info.first->set_movement_path(pfrm, app, std::move(*path));
                return;
            } else {
                ::info(pfrm,
                       format("path not found from %,% to %,%",
                              dst_coord.x,
                              dst_coord.y,
                              info.first->grid_position().x,
                              info.first->grid_position().y));
            }
        } else {
            ::info(pfrm, "chr not found!!!");
        }
    } else {
        ::info(pfrm, "island null");
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::ReplicantCreated& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const RoomCoord loc = {packet.src_x_, packet.src_y_};

    auto chr = app.alloc_entity<BasicCharacter>(
        pfrm, &app.player_island(), &app.player(), loc, true);

    if (chr) {
        chr->apply_damage(pfrm, app, 255 - packet.health_);
        chr->transported();
        player_island(app).add_character(std::move(chr));
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomDestroyed& packet)
{
    Island* island = nullptr;

    // NOTE: for the other multiplayer vs mode, the original implmentation
    // flipped the "near" flag, so that the value would be correct from the
    // perspective of the receiver, as the x-coordinate system was inverted
    // between the two consoles. When adding a co-op mode, where both players
    // control the same island, I realized that my prior decision was probably a
    // bad idea, but I haven't fixed it yet.
    if (not packet.near_island_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island) {
        if (auto room = island->get_room({packet.room_x_, packet.room_y_})) {
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



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::TerrainConstructed& packet)
{
    player_island(app).init_terrain(pfrm, packet.new_terrain_size_);
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::OpponentBulkheadChanged& packet)
{
    const RoomCoord loc = {packet.room_x_, packet.room_y_};

    if (auto room = player_island(app).get_room(loc)) {
        if (auto bulkhead = dynamic_cast<Bulkhead*>(room)) {
            bulkhead->set_open(pfrm, app, packet.open_);
        }
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::DroneSpawn& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    Island* island = nullptr;
    if (packet.destination_near_) {
        island = &app.player_island();
    } else {
        island = app.opponent_island();
    }

    const auto x_origin = packet.origin_x_;

    auto [dt, ds] = drone_metatable();
    if (packet.drone_class_ >= ds) {
        StringBuffer<32> err("invalid index! ");
        err += stringify(packet.drone_class_);
        pfrm.fatal(err.c_str());
    }
    auto drone_meta = &dt[packet.drone_class_];
    if (auto drone = (*drone_meta)
                         ->create(&app.player_island(),
                                  island,
                                  RoomCoord{x_origin, packet.origin_y_})) {

        const RoomCoord drone_bay_pos = {x_origin, u8(packet.origin_y_ + 1)};

        if (auto room = app.player_island().get_room(drone_bay_pos)) {

            if (room->attach_drone(pfrm, app, *drone)) {
                island->drones().push(*drone);
            }

            (*drone)->set_movement_target(
                RoomCoord{packet.deploy_x_, packet.deploy_y_});
        }
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::DroneDestroyed& packet)
{
    Island* island = nullptr;
    if (packet.destination_near_) {
        island = &app.player_island();
    } else {
        island = app.opponent_island();
    }

    RoomCoord pos;
    pos.x = packet.drone_x_;
    pos.y = packet.drone_y_;

    if (auto drone = island->get_drone(pos)) {
        if (drone) {
            (*drone)->kill();
        }
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::DroneSetTarget& packet)
{
    if (not app.opponent_island()) {
        return;
    }

    Island* island = nullptr;

    if (packet.drone_near_) {
        island = &app.player_island();
    } else {
        island = app.opponent_island();
    }

    const auto drone_x = packet.drone_x_;
    if (auto drone = island->get_drone({drone_x, packet.drone_y_})) {
        (*drone)->set_target(pfrm,
                             app,
                             {packet.target_x_, packet.target_y_},
                             not packet.target_near_);
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpRoomLockAcquire& packet)
{
    using RespType = network::packet::CoOpRoomLockResponse;

    RespType resp;
    resp.x_ = packet.x_;
    resp.y_ = packet.y_;
    resp.status_ = RespType::failure;

    if (auto room = app.player_island().get_room({packet.x_, packet.y_})) {
        if (room->co_op_peer_acquire_lock()) {
            resp.status_ = RespType::success;
        }
    }

    network::transmit(pfrm, resp);
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpRoomLockRelease& packet)
{
    if (auto room = app.player_island().get_room({packet.x_, packet.y_})) {
        room->co_op_peer_release_lock();
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpRoomLockResponse& packet)
{
    using RespType = network::packet::CoOpRoomLockResponse;

    Scene* s = &app.scene();

    if (auto scene = dynamic_cast<MultiplayerCoOpAwaitLockScene*>(s)) {
        scene->signal_result(packet.status_ == RespType::success);
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpChrLockAcquire& packet)
{
    using RespType = network::packet::CoOpChrLockResponse;

    const auto chr_id = packet.chr_id_.get();

    RespType resp;
    resp.chr_id_.set(chr_id);
    resp.status_ = RespType::failure;

    if (auto chr = BasicCharacter::find_by_id(app, chr_id).first) {
        if (chr->co_op_acquire_lock()) {
            resp.status_ = RespType::success;
        }
    }

    network::transmit(pfrm, resp);
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpChrLockRelease& packet)
{
    const auto chr_id = packet.chr_id_.get();

    if (auto chr = BasicCharacter::find_by_id(app, chr_id).first) {
        chr->co_op_release_lock();
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpChrLockResponse& packet)
{
    using RespType = network::packet::CoOpChrLockResponse;

    Scene* s = &app.scene();

    if (auto scene = dynamic_cast<MultiplayerCoOpAwaitChrLockScene*>(s)) {
        scene->signal_result(packet.status_ == RespType::success);
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoOpOpponentDestroyed&)
{
    if (app.opponent_island()) {
        for (auto& room : app.opponent_island()->rooms()) {
            auto category = (*room->metaclass())->category();
            if (category == Room::Category::power) {
                auto ko_hp = std::numeric_limits<Health>::max();
                room->apply_damage(pfrm, app, ko_hp);
            }
        }
    }
}



void CoOpTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::Paused& pkt)
{
    Scene* s = &app.scene();

    if (auto scene = dynamic_cast<WorldScene*>(s)) {
        if (pkt.status_) {
            scene->set_gamespeed(pfrm, app, GameSpeed::stopped);
        } else {
            scene->set_gamespeed(pfrm, app, GameSpeed::normal);
        }
    }
}



void CoOpTeam::network_sync_cursor(Platform& pfrm,
                                   const RoomCoord& cursor,
                                   u8 cursor_icon,
                                   bool near)
{
    // Don't bother to send an updated position of the cursor if we have
    // outgoing stuff in the send queue. Updating the cursor graphics for co-op
    // players isn't super essential.
    if (pfrm.network_peer().send_queue_empty()) {
        network::packet::CoOpCursor p;
        p.x_ = cursor.x;
        p.y_ = cursor.y;
        p.near_ = near;
        p.icon_ = cursor_icon;

        network::transmit(pfrm, p);
    }
}



} // namespace skyland
