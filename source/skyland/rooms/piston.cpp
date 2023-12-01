////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



Piston::Piston(Island* parent, const RoomCoord& position, const char* name)
    : Room(parent, name, position)
{
}



void Piston::render_interior(App* app, TileId buffer[16][16])
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



void Piston::render_exterior(App* app, TileId buffer[16][16])
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



ScenePtr<Scene> Piston::select(const RoomCoord& cursor)
{
    if ((dir_ == down and position().y == 14) or
        (dir_ == left and position().x == 0) or
        (dir_ == right and position().x == parent()->terrain().size() - 1)) {
        // TODO: Return error: extending piston would access out of bounds tile.
        return null_scene();
    }


    auto record_event = [&] {
        if (parent() == &player_island()) {
            // I repurposed the ReloadComplete event because it's already looped
            // into a callback (overriden below).
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
    };


    parent()->schedule_repaint();

    if (opened_) {
        RoomCoord start = position();
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
            invalid->apply_damage(health_upper_limit());
        }

        if (auto room = parent()->get_room(start)) {
            if (is_sticky() and room->size().x == 1 and room->size().y == 1) {
                parent()->move_room(start, end);
            }
        }

        opened_ = false;

        record_event();

    } else {
        RoomCoord start = position();
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
            invalid->apply_damage(health_upper_limit());
        }

        if (room) {
            if (room->size().x == 1 and room->size().y == 1) {

                parent()->move_room(start, end);
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



void Piston::___rewind___finished_reload()
{
    opened_ = not opened_;
    parent()->schedule_repaint();
}



ScenePtr<Scene> Piston::setup()
{
    const bool near = parent() == &player_island();

    return scene_pool::alloc<SetupPistonScene>(position(), near);
}



} // namespace skyland
