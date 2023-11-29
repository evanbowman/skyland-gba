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


#include "cargoBay.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"
#include <string.h>



namespace skyland
{



CargoBay::CargoBay(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
    set_cargo("", 0);
}



void CargoBay::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_cargo_bay)->c_str();
}



bool CargoBay::set_cargo(const char* cargo, u8 count)
{
    if (str_len(cargo) + 1 > sizeof cargo_) {
        return false;
    }

    count_ = 0;

    auto dest = cargo_;
    auto src = cargo;

    while (*src not_eq '\0') {
        ++count_;
        *dest++ = *src++;
    }
    *dest = '\0';

    Room::ready();

    return true;
}



void CargoBay::update(Microseconds delta)
{
    Room::update(delta);

    if (count_) {
        Room::ready();
    }
}



void CargoBay::display(Platform::Screen& screen)
{
    if (parent()->interior_visible()) {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y.as_integer() < 700) {
                screen.draw(c->prepare_sprite());
            }
        }

        if (*cargo() not_eq '\0') {
            Sprite sprite;
            sprite.set_texture_index(51);
            auto pos = origin();
            pos.y +=
                Fixnum::from_integer(16 + parent()->get_ambient_movement());
            sprite.set_position(pos);
            sprite.set_size(Sprite::Size::w16_h32);
            screen.draw(sprite);
        }
    }
}



void CargoBay::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::cargo_bay;
    buffer[position().x][position().y + 1] = InteriorTile::plain_floor;
}



void CargoBay::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



lisp::Value* CargoBay::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    builder.push_back(lisp::make_string(cargo()));

    if (health() not_eq max_health()) {
        builder.push_back(lisp::make_integer(health()));
    }

    return builder.result();
}



void CargoBay::deserialize(lisp::Value* list)
{
    if (lisp::length(list) >= 4) {
        auto c = lisp::get_list(list, 3);
        if (c->type() == lisp::Value::Type::string) {
            set_cargo(c->string().value(), str_len(c->string().value()));
        }
    }

    if (lisp::length(list) >= 5) {
        __set_health(lisp::get_list(list, 4)->integer().value_);
    }
}



void CargoBay::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());

        if (cargo_[0] not_eq '\0') {
            time_stream::event::CargoBayContents e;

            static_assert(sizeof e.cargo_ == sizeof cargo_);
            memcpy(e.cargo_, cargo_, sizeof cargo_);
            e.count_ = count_;
            e.x_ = position().x;
            e.y_ = position().y;
            e.near_ = parent() == &APP.player_island();

            APP.time_stream().push(APP.level_timer(), e);
        }
    }
}



} // namespace skyland
