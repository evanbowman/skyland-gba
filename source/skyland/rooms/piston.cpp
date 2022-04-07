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


#include "piston.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/setupPistonScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStream.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Piston::Piston(Island* parent, const Vec2<u8>& position, const char* name)
    : Room(parent, name, position)
{
}



void Piston::render_interior(App& app, u8 buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    u8 base_tile = InteriorTile::piston_closed_r + (dir_ * 3);

    if (opened_) {
        switch (dir_) {
        case Direction::left:
            buffer[x][y] = base_tile + 2;
            buffer[x - 1][y] = base_tile + 1;
            break;

        case Direction::right:
            buffer[x][y] = base_tile + 1;
            buffer[x + 1][y] = base_tile + 2;
            break;

        case Direction::up:
            buffer[x][y] = base_tile + 1;
            buffer[x][y - 1] = base_tile + 2;
            break;

        case Direction::down:
            buffer[x][y] = base_tile + 1;
            buffer[x][y + 1] = base_tile + 2;
            break;
        }
    } else {
        buffer[x][y] = base_tile;
    }
}



void Piston::render_exterior(App& app, u8 buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    u8 base_tile = InteriorTile::piston_closed_r + (dir_ * 3);

    if (opened_) {
        switch (dir_) {
        case Direction::left:
            buffer[x][y] = base_tile + 2;
            buffer[x - 1][y] = base_tile + 1;
            break;

        case Direction::right:
            buffer[x][y] = base_tile + 1;
            buffer[x + 1][y] = base_tile + 2;
            break;

        case Direction::up:
            buffer[x][y] = base_tile + 1;
            buffer[x][y - 1] = base_tile + 2;
            break;

        case Direction::down:
            buffer[x][y] = base_tile + 1;
            buffer[x][y + 1] = base_tile + 2;
            break;
        }
    } else {
        buffer[x][y] = base_tile;
    }
}



ScenePtr<Scene> Piston::select(Platform& pfrm, App& app, const Vec2<u8>& cursor)
{
    if ((dir_ == down and position().y == 14) or
        (dir_ == left and position().x == 0) or
        (dir_ == right and position().x == parent()->terrain().size() - 1)) {
        // TODO: Return error: extending piston would access out of bounds tile.
        return null_scene();
    }


    auto record_event = [&] {
        if (parent() == &player_island(app)) {
            // I repurposed the ReloadComplete event because it's already looped
            // into a callback (overriden below).
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
    };


    parent()->schedule_repaint();

    if (opened_) {
        Vec2<u8> start = position();
        auto end = start;

        switch (dir_) {
        case Direction::right:
            start.x += 2;
            end.x += 1;
            break;

        case Direction::left:
            start.x -= 2;
            end.x -= 1;
            break;

        case Direction::up:
            start.y -= 2;
            end.y -= 1;
            break;

        case Direction::down:
            start.y += 2;
            end.y += 1;
            break;
        }

        if (auto invalid = parent()->get_room(end)) {
            // Crushed by retracting piston.
            invalid->apply_damage(pfrm, app, health_upper_limit());
        }

        if (auto room = parent()->get_room(start)) {
            if (is_sticky() and room->size().x == 1 and room->size().y == 1) {
                parent()->move_room(app, start, end);
            }
        }

        opened_ = false;

        record_event();

    } else {
        Vec2<u8> start = position();
        auto end = start;

        switch (dir_) {
        case Direction::right:
            end.x += 2;
            start.x += 1;
            break;

        case Direction::left:
            end.x -= 2;
            start.x -= 1;
            break;

        case Direction::up:
            end.y -= 2;
            start.y -= 1;
            break;

        case Direction::down:
            end.y += 2;
            start.y += 1;
            break;
        }

        auto room = parent()->get_room(start);
        auto invalid = parent()->get_room(end);

        if (room and invalid) {
            // Crushed by extending piston.
            invalid->apply_damage(pfrm, app, health_upper_limit());
        }

        if (room) {
            if (room->size().x == 1 and room->size().y == 1) {

                parent()->move_room(app, start, end);
            } else {
                // Play error sound effect. Cannot open, large block in the way.
                return null_scene();
            }
        }

        opened_ = true;

        record_event();
    }


    return null_scene();
}



bool Piston::is_sticky() const
{
    return false;
}



void Piston::___rewind___finished_reload(Platform& pfrm, App& app)
{
    opened_ = not opened_;
    parent()->schedule_repaint();
}



ScenePtr<Scene> Piston::setup(Platform& pfrm, App& app)
{
    const bool near = parent() == &player_island(app);

    return scene_pool::alloc<SetupPistonScene>(pfrm, position(), near);
}



} // namespace skyland
