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



void DroneBay::update(App& app, Microseconds delta)
{
    Room::update(app, delta);

    Room::ready();

    if (drone_ and not(*drone_)->alive()) {
        detach_drone(app, false);
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        if (drone_) {
            (*drone_)->kill();
        }
    }

    if (reload_ > 0) {
        reload_ -= delta;
        if (reload_ < 0) {
            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            }
        }
    }
}



void DroneBay::rewind(App& app, Microseconds delta)
{
    if (drone_ and not(*drone_)->alive()) {
        ___rewind___ability_used(app);
        detach_drone(app, false);
    }

    if (reload_ <= 0) {
        // fully reloaded
    } else if (reload_ < 1000 * drone_bay_reload_ms) {
        reload_ += delta;
    }
}



void DroneBay::___rewind___finished_reload(App&)
{
    reload_ = 1;
}



void DroneBay::___rewind___ability_used(App&)
{
    reload_ = 0;
}



void DroneBay::display(Platform::Screen& screen, App& app)
{
    if (drone_) {
        (*drone_)->display(screen, app);
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



ScenePtr<Scene> DroneBay::select(App& app, const RoomCoord& cursor)
{
    if (reload_ > 0) {
        return null_scene();
    }

    if (parent() not_eq &app.player_island()) {
        return null_scene();
    }

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (auto scn = reject_if_friendly(app)) {
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
            auto future_scene = []() {
                return scene_pool::alloc<ReadyScene>();
            };
            PLATFORM.speaker().play_sound("beep_error", 2);
            return scene_pool::alloc<NotificationScene>("drone-bay covered!",
                                                        future_scene);
        }
        return scene_pool::alloc<ConstructDroneScene>(position());
    }
    return null_scene();
}



bool DroneBay::attach_drone(App& app, SharedEntityRef<Drone> drone)
{
    if (drone_) {
        detach_drone(app, false);
    }
    drone_ = drone;

    start_reload();

    return true;
}



void DroneBay::detach_drone(App& app, bool quiet)
{
    if (drone_ and not quiet) {
        (*drone_)->kill();

        time_stream::event::DroneDestroyed e;
        e.x_pos_ = (*drone_)->position().x;
        e.y_pos_ = (*drone_)->position().y;
        e.destination_near_ = (*drone_)->destination() == &app.player_island();
        e.parent_near_ = (*drone_)->parent() == &app.player_island();
        e.type_ = (*drone_)->metaclass_index();
        e.state_ = (*drone_)->state();
        e.timer_.set((*drone_)->timer());
        e.duration_.set((*drone_)->duration());
        e.db_x_pos_ = position().x;
        e.db_y_pos_ = position().y;
        app.time_stream().push(app.level_timer(), e);
    }

    drone_.reset();
}



void DroneBay::finalize(App& app)
{
    detach_drone(app, false);

    Room::finalize(app);
}



} // namespace skyland
