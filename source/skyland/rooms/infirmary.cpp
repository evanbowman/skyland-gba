////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "infirmary.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/island.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Infirmary::Infirmary(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



static const auto infirmary_heal_interval = milliseconds(1000);



Time Infirmary::heal_interval() const
{
    Time interval = infirmary_heal_interval;
    if (amplify_) {
        interval /= 2;
    }
    return interval;
}



void Infirmary::on_powerchange()
{
    heal_timer_ = heal_interval();
}



bool Infirmary::allows_powerdown()
{
    return true;
}



void Infirmary::update(Time delta)
{
    Room::update(delta);

    if (is_powered_down()) {
        return;
    }

    // Optimization: room has no inhabitants, don't schedule for updates.
    if (characters().empty()) {
        return;
    }

    Room::ready();

    int characters_healing = 0;

    for (auto& character : characters()) {
        if (character->owner() == &parent()->owner() and
            character->state() not_eq Character::State::fighting) {
            ++characters_healing;
        }
    }

    if (characters_healing) {
        heal_timer_ += delta;
        if (heal_timer_ > heal_interval()) {
            heal_timer_ -= heal_interval();
            int distribute_health = 20;
            distribute_health /= characters_healing;
            for (auto& character : characters()) {
                if (character->owner() == &parent()->owner() and
                    character->state() not_eq Character::State::fighting) {
                    character->heal(distribute_health);
                }
            }
        }
    }
}



void Infirmary::amplify(bool enabled)
{
    amplify_ = enabled;
}



void Infirmary::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_infirmary)->c_str();
}


void Infirmary::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::infirmary_1;
    buffer[position().x][position().y + 1] = InteriorTile::infirmary_2;
    buffer[position().x + 1][position().y] = InteriorTile::infirmary_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::plain_floor;
}


void Infirmary::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



void Infirmary::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



void Infirmary::display(Platform::Screen& screen)
{
    if (parent()->interior_visible()) {

        static const u8 anim_frames = 48;

        anim_cyc_++;
        if (anim_cyc_ > anim_frames) {
            anim_cyc_ = 0;
        }

        static const u8 half_cyc = anim_frames / 2 - 1;

        if (anim_cyc_ > half_cyc) {
            for (auto& chr : characters()) {
                if (chr->owner() == &parent()->owner() and
                    chr->state() not_eq Character::State::fighting and
                    not chr->is_replicant() and
                    chr->health() < chr->get_max_health()) {
                    Sprite spr;

                    spr.set_tidx_8x8(43, 0);
                    spr.set_size(Sprite::Size::w8_h8);
                    auto pos = chr->sprite().get_position();
                    pos.y -= 8.0_fixed;
                    pos.x += 4.0_fixed;
                    spr.set_position(pos);

                    static const u8 anim_pad = 3;

                    if (anim_cyc_ < (half_cyc + anim_pad) or
                        anim_cyc_ > anim_frames - anim_pad) {
                        spr.set_alpha(Sprite::Alpha::translucent);
                    }

                    screen.draw(spr);
                }
            }
        }
    }

    Room::display(screen);
}


} // namespace skyland
