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


#include "crane.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"
#include "skyland/scene/craneDropScene.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/skyland.hpp"


namespace skyland
{



void Crane::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_crane)->c_str();
}



Crane::Crane(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Crane::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);
    timer_ = 0;
    state_ = State::idle;
}



void Crane::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
    Room::ready();

    switch (state_) {
    case State::idle:
        break;

    case State::drop:
        timer_ += delta;
        break;

    case State::retract:
        timer_ -= delta;
        if (timer_ < 0) {
            timer_ = 0;
            state_ = State::idle;
        }
        break;
    }
}



void Crane::display(Platform::Screen& screen)
{
    Sprite spr;
    spr.set_texture_index(92);
    spr.set_size(Sprite::Size::w16_h32);
    auto pos = center();
    pos.x += 14;
    pos.y += parent()->get_ambient_movement();
    pos.y += 2;
    const auto start_pos = pos;

    pos.y += timer_ * Fixnum(0.00004f);

    spr.set_position(pos);
    screen.draw(spr);

    const auto claw_pos = pos;

    spr.set_texture_index(93);

    pos = start_pos;
    while (pos.y < claw_pos.y - 3) {
        spr.set_position(pos);
        screen.draw(spr);
        pos.y += 8;
    }

}



ScenePtr<Scene> Crane::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    if (state_ == State::idle) {
        state_ = State::drop;
        timer_ = 0;
    }

    if (app.coins() < 2000) {
        auto str = SYSTR(construction_insufficient_funds);
        auto next = scene_pool::make_deferred_scene<ReadyScene>();
        return scene_pool::alloc<NotificationScene>(str->c_str(), next);
    }

    return scene_pool::alloc<CraneDropScene>(position());
}



void Crane::render_interior(App& app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    buffer[x][y] = InteriorTile::crane_1;
    buffer[x + 1][y] = InteriorTile::crane_2;
    buffer[x + 2][y] = InteriorTile::crane_3;
    buffer[x][y + 1] = InteriorTile::crane_4;
    buffer[x + 1][y + 1] = InteriorTile::crane_5;
    buffer[x + 2][y + 1] = InteriorTile::crane_6;
}



void Crane::render_exterior(App& app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    buffer[x][y] = Tile::crane_1;
    buffer[x + 1][y] = Tile::crane_2;
    buffer[x + 2][y] = Tile::crane_3;
    buffer[x][y + 1] = Tile::crane_4;
    buffer[x + 1][y + 1] = Tile::crane_5;
    buffer[x + 2][y + 1] = Tile::crane_6;
}



} // namespace skyland
