////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "radiator.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



Radiator::Radiator(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Radiator::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_radiator)->c_str();
}



void Radiator::update(Time delta)
{
    Room::update(delta);

    Room::ready();

    damage_timer_ += delta;

    if (damage_timer_ > seconds(1)) {
        damage_timer_ -= seconds(1);

        emit_radiation();
    }
}



void Radiator::collect_nearby_chrs(ChrBuffer& output)
{
    auto pos = position();
    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        for (int y = pos.y - 2; y < pos.y + 3; ++y) {
            if (x < 0 or y < 0) {
                continue;
            }
            if (auto room = parent()->get_room({u8(x), u8(y)})) {
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() not_eq RoomCoord{(u8)x, (u8)y}) {
                        continue;
                    }
                    const bool found = [&] {
                        for (auto pushed : output) {
                            if (pushed == chr.get()) {
                                return true;
                            }
                        }
                        return false;
                    }();
                    if (not found) {
                        output.push_back(chr.get());
                    }
                }
            }
        }
    }
}



class RadiationAnim : public Entity
{
public:
    RadiationAnim(Vec2<Fixnum> pos) : Entity({})
    {
        sprite_.set_size(Sprite::Size::w16_h16);
        sprite_.set_tidx_16x16(31, 0);
        sprite_.set_position(pos);
        sprite_.set_origin({});
        // sprite_.set_alpha(Sprite::Alpha::translucent);
        sprite_.set_mix({custom_color(0xe81858), 255});
    }


    void update(Time delta) override
    {
        // The game manipulates the time delta for slow motion stuff, etc. But
        // we always want this UI effect to play at the same rate.
        delta = PLATFORM.delta_clock().last_delta();

        timer_ += delta;
        if (timer_ >= milliseconds(80)) {
            timer_ -= milliseconds(80);
            auto t = sprite_.get_texture_index();
            if (t == 31 * 2 + 5) {
                kill();
                return;
            }

            ++t;
            sprite_.set_texture_index(t);
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }


    Sprite& sprite()
    {
        return sprite_;
    }

private:
    Time timer_ = 0;
};



void make_radiation_effect(Vec2<Fixnum> pos)
{
    auto segment = [&](Fixnum xoff, Fixnum yoff, bool xflip, bool yflip) {
        auto p = pos;
        p.x += xoff;
        p.y += yoff;
        if (auto e = APP.alloc_entity<RadiationAnim>(p)) {
            e->sprite().set_flip({xflip, yflip});
            APP.effects().push(std::move(e));
        }
    };

    segment(Fixnum::from_integer(-16), Fixnum::from_integer(-16), false, false);
    segment(Fixnum::from_integer(-16), 0.0_fixed, false, true);
    segment(0.0_fixed, Fixnum::from_integer(-16), true, false);
    segment(0.0_fixed, 0.0_fixed, true, true);
}



static SharedVariable radiation_damage("radiation_damage", 20);



void Radiator::emit_radiation()
{
    ChrBuffer queue;
    collect_nearby_chrs(queue);

    for (auto& chr : queue) {
        chr->apply_radiation_damage(radiation_damage);
    }

    make_radiation_effect(visual_center());
}



void Radiator::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::radiator;
}



void Radiator::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::radiator;
}



bool Radiator::opponent_display_on_hover() const
{
    return true;
}



void Radiator::display_on_hover(Platform::Screen& screen,

                                const RoomCoord& cursor)
{
    auto pos = position();

    auto origin = parent()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y - 2) * 16)});
        screen.draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);

            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y) * 16)});

            screen.draw(sprite);
            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
        }


        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});

        screen.draw(sprite);
    }
}



} // namespace skyland
