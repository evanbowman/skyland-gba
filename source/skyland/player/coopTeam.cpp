#include "coopTeam.hpp"
#include "skyland/island.hpp"
#include "skyland/rooms/tnt.hpp"
#include "number/random.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/transporter.hpp"



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
        if (not opponent_island(app)) {
            // TODO: raise error?
            return;
        }
        if (auto room = opponent_island(app)->get_room({packet.weapon_x_,
                packet.weapon_y_})) {

            room->set_target(pfrm, app, {packet.target_x_, packet.target_y_});
        }
    }
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoopRngSync& packet)
{
    rng::critical_state = packet.rng_state_.get();
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CoopCursor& packet)
{
    std::get<SkylandGlobalData>(globals()).coop_cursor_.x = packet.x_;
    std::get<SkylandGlobalData>(globals()).coop_cursor_.y = packet.y_;
    std::get<SkylandGlobalData>(globals()).coop_cursor_near_ = packet.near_;

    std::get<SkylandGlobalData>(globals()).coop_cursor_icon_ =
        [&] {
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



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::DynamiteActivated& packet)
{
    Vec2<u8> pos{packet.x_, packet.y_};

    if (auto room = player_island(app).get_room(pos)) {
        if (dynamic_cast<Explosive*>(room)) {
            room->apply_damage(pfrm, app, 1);
        } else {
            // TODO: fatal error?
        }
    }
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::SetWeaponGroup& packet)
{
    if (auto room = player_island(app).get_room({packet.x_, packet.y_})) {
        room->set_group((Room::Group)packet.group_);
        player_island(app).show_groups(true);
        player_island(app).repaint(pfrm, app);
    }
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::RoomSalvaged& packet)
{
    player_island(app).destroy_room(pfrm, app, {packet.x_, packet.y_});
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CharacterDied& packet)
{
    Island* island = nullptr;

    if (not packet.near_island_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    const Vec2<u8> chr_loc = {packet.chr_x_, packet.chr_y_};

    if (island) {
        if (auto room = island->get_room(chr_loc)) {
            for (auto it = room->characters().begin();
                 it not_eq room->characters().end();) {

                const bool owned_by_player = (*it)->owner() == &player(app);
                if ((*it)->grid_position() == chr_loc and
                    (not packet.chr_owned_by_player_) == owned_by_player and
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



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CharacterBoarded& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const auto src = Vec2<u8>{packet.src_x_, packet.src_y_};
    const auto dst = Vec2<u8>{packet.dst_x_, packet.dst_y_};

    auto source_island = packet.transporter_near_ ?
        &player_island(app) : opponent_island(app);

    auto dest_island = packet.transporter_near_ ?
        opponent_island(app) : &player_island(app);

    transport_character_impl(app,
                             packet.owned_by_ai_,
                             source_island,
                             dest_island,
                             src,
                             dst);
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CharacterDisembark& packet)
{
    if (not opponent_island(app)) {
        return;
    }

    const auto src = Vec2<u8>{packet.src_x_, packet.src_y_};
    const auto dst = Vec2<u8>{packet.dst_x_, packet.dst_y_};

    auto dest_island = packet.transporter_near_ ?
        &player_island(app) : opponent_island(app);

    auto source_island = packet.transporter_near_ ?
        opponent_island(app) : &player_island(app);

    transport_character_impl(app,
                             packet.owned_by_ai_,
                             source_island,
                             dest_island,
                             src,
                             dst);
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::CharacterSetTarget& packet)
{
    Island* island = nullptr;

    if (not packet.near_island_) {
        island = &player_island(app);
    } else {
        island = opponent_island(app);
    }

    if (island) {
        const Vec2<u8> src_coord{packet.src_x_, packet.src_y_};
        const Vec2<u8> dst_coord{packet.dst_x_, packet.dst_y_};

        if (auto room = island->get_room(src_coord)) {
            for (auto& chr : room->characters()) {
                if (chr->grid_position() == src_coord and
                    ((not packet.owned_by_ai_) == (chr->owner() == &player(app)))) {

                    auto path =
                        find_path(pfrm, app, island, src_coord, dst_coord);

                    if (path and *path) {
                        chr->set_movement_path(pfrm, app, std::move(*path));
                        return;
                    }
                }
            }
        }
    }
}



void CoopTeam::receive(Platform& pfrm,
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
                room->apply_damage(pfrm, app, 9999);
            }
        }
    }
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::TerrainConstructed& packet)
{
    player_island(app).init_terrain(pfrm, packet.new_terrain_size_);
}



void CoopTeam::receive(Platform& pfrm,
                       App& app,
                       const network::packet::OpponentBulkheadChanged& packet)
{
    const Vec2<u8> loc = {packet.room_x_, packet.room_y_};

    if (auto room = player_island(app).get_room(loc)) {
        if (auto bulkhead = dynamic_cast<Bulkhead*>(room)) {
            bulkhead->set_open(pfrm, app, packet.open_);
        }
    }
}



void CoopTeam::network_sync_cursor(Platform& pfrm,
                                   const Vec2<u8>& cursor,
                                   u8 cursor_icon,
                                   bool near)
{
    // Don't bother to send an updated position of the cursor if we have
    // outgoing stuff in the send queue. Updating the cursor graphics for co-op
    // players isn't super essential.
    if (pfrm.network_peer().send_queue_empty()) {
        network::packet::CoopCursor p;
        p.x_ = cursor.x;
        p.y_ = cursor.y;
        p.near_ = near;
        p.icon_ = cursor_icon;

        network::transmit(pfrm, p);
    }
}



}
