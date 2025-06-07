////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "droneBay.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/constructDroneScene.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/placeDroneScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(drone_bay_reload_ms);



DroneBay::DroneBay(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void DroneBay::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_drone_bay)->c_str();
}



void DroneBay::update(Time delta)
{
    Room::update(delta);

    Room::ready();

    if (drone_ and not(*drone_)->alive()) {
        detach_drone(false);
    }

    if (is_powered_down()) {
        return;
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        if (drone_) {
            (*drone_)->kill();
        }
    }

    if (reload_ > 0) {
        reload_ -= delta;
        if (reload_ < 0) {
            if (is_player_island(parent())) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                APP.time_stream().push(APP.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                APP.time_stream().push(APP.level_timer(), e);
            }
        }
    }
}



void DroneBay::rewind(Time delta)
{
    if (drone_ and not(*drone_)->alive()) {
        ___rewind___ability_used();
        detach_drone(false);
    }

    if (is_powered_down()) {
        return;
    }

    if (reload_ <= 0) {
        // fully reloaded
    } else if (reload_ < 1000 * drone_bay_reload_ms) {
        reload_ += delta;
    }
}



void DroneBay::___rewind___finished_reload()
{
    reload_ = 1;
}



void DroneBay::___rewind___ability_used()
{
    reload_ = 0;
}



void DroneBay::display(Platform::Screen& screen)
{
    if (drone_) {
        (*drone_)->display(screen);
    }
}



void DroneBay::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::drone_bay_1;
    buffer[position().x + 1][position().y] = Tile::drone_bay_2;
}



void DroneBay::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::drone_bay_1;
    buffer[position().x + 1][position().y] = InteriorTile::drone_bay_2;
}



ScenePtr DroneBay::select_impl(const RoomCoord& cursor)
{
    if (reload_ > 0) {
        return null_scene();
    }

    if (parent() not_eq &APP.player_island()) {
        return null_scene();
    }

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (auto scn = reject_if_friendly()) {
        return scn;
    }

    if (not drone_) {
        auto pos = position();
        bool free[2] = {true, true};
        pos.y -= 1;
        if (auto room = parent()->get_room(pos)) {
            if (not is_forcefield(room->metaclass())) {
                free[0] = false;
            }
        }
        pos.x += 1;
        if (auto room = parent()->get_room(pos)) {
            if (not is_forcefield(room->metaclass())) {
                free[1] = false;
            }
        }
        if (not free[0] or not free[1]) {
            auto future_scene = []() { return make_scene<ReadyScene>(); };
            PLATFORM.speaker().play_sound("beep_error", 2);
            return make_scene<NotificationScene>("drone-bay covered!",
                                                 future_scene);
        }
        return make_scene<ConstructDroneScene>(position());
    }
    return null_scene();
}



void DroneBay::on_powerchange()
{
    if (is_powered_down()) {
        detach_drone(false);
    }

    start_reload();
}



bool DroneBay::allows_powerdown()
{
    return true;
}



bool DroneBay::attach_drone(SharedEntityRef<Drone> drone)
{
    if (drone_) {
        detach_drone(false);
    }
    drone_ = drone;

    start_reload();

    return true;
}



Time DroneBay::reload_interval() const
{
    Time ret = 1000 * drone_bay_reload_ms;
    if (amplify_) {
        ret /= 2;
    }
    return ret;
}



void DroneBay::detach_drone(bool quiet)
{
    if (drone_ and not quiet) {
        (*drone_)->kill();

        (*drone_)->clear_target_queue();

        time_stream::event::DroneDestroyed e;
        e.x_pos_ = (*drone_)->position().x;
        e.y_pos_ = (*drone_)->position().y;
        e.destination_near_ = is_player_island((*drone_)->destination());
        e.parent_near_ = is_player_island((*drone_)->parent());
        e.type_ = (*drone_)->metaclass_index();
        e.state_ = (*drone_)->state();
        e.timer_.set((*drone_)->timer());
        e.duration_.set((*drone_)->duration());
        e.db_x_pos_ = position().x;
        e.db_y_pos_ = position().y;
        APP.time_stream().push(APP.level_timer(), e);

        if (not PLATFORM.screen().fade_active()) {
            PLATFORM.speaker().play_sound("explosion1", 0);
            APP.camera()->shake(6);
        }
    }

    drone_.reset();
}



void DroneBay::amplify(bool enable)
{
    amplify_ = enable;

    reload_ = std::min(reload_, reload_interval());
}



void DroneBay::finalize()
{
    detach_drone(false);

    if (not rq_.empty()) {
        time_stream::event::AttachReconstructionQueue e;
        e.db_x_ = position().x;
        e.db_y_ = position().y;
        e.previous_queue_memory_ = rq_.mem_;
        e.previous_queue_size_ = rq_.count_;
        APP.time_stream().push(APP.level_timer(), e);
    }

    Room::finalize();
}



void DroneBay::parent_layout_changed(RoomCoord moved_from, RoomCoord to)
{
    for (int i = 0; i < rq_.size(); ++i) {
        auto& elem = rq_[i];
        if (elem.x_ == moved_from.x and elem.y_ == moved_from.y) {
            elem.x_ = to.x;
            elem.y_ = to.y;
        }
    }
}



} // namespace skyland
