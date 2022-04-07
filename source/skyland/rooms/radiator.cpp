////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "radiator.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Radiator::Radiator(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Radiator::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_radiator)->c_str();
}



void Radiator::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    damage_timer_ += delta;

    if (damage_timer_ > seconds(1)) {
        damage_timer_ -= seconds(1);

        emit_radiation(pfrm, app);
    }
}



static SharedVariable radiation_damage("radiation_damage", 20);



void Radiator::emit_radiation(Platform& pfrm, App& app)
{
    Buffer<BasicCharacter*, 10> queue;

    auto pos = position();
    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        for (int y = pos.y - 2; y < pos.y + 3; ++y) {
            if (x < 0 or y < 0) {
                continue;
            }
            if (auto room = parent()->get_room({u8(x), u8(y)})) {
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() not_eq Vec2<u8>{(u8)x, (u8)y}) {
                        continue;
                    }
                    const bool found = [&] {
                        for (auto pushed : queue) {
                            if (pushed == chr.get()) {
                                return true;
                            }
                        }
                        return false;
                    }();
                    if (not found) {
                        queue.push_back(chr.get());
                    }
                }
            }
        }
    }

    for (auto& chr : queue) {
        chr->apply_damage(pfrm, app, radiation_damage);
    }
}



void Radiator::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::radiator;
}



void Radiator::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::radiator;
}



void Radiator::display_on_hover(Platform::Screen& screen,
                                App& app,
                                const Vec2<u8>& cursor)
{
    auto pos = position();

    auto origin = parent()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position({origin.x + x * 16, origin.y + (pos.y - 2) * 16});
        screen.draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_texture_index(14);

            sprite.set_position(
                {origin.x + x * 16, origin.y + (pos.y - 1) * 16});

            screen.draw(sprite);

            sprite.set_texture_index(13);
        }


        sprite.set_position({origin.x + x * 16, origin.y + (pos.y + 1) * 16});

        screen.draw(sprite);
    }
}



} // namespace skyland
