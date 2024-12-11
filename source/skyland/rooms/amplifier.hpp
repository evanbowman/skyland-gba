////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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


#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class AmpAnim : public Entity
{
public:
    AmpAnim(Vec2<Fixnum> pos) : Entity({})
    {
        sprite_.set_size(Sprite::Size::w16_h16);
        sprite_.set_tidx_16x16(31, 0);
        sprite_.set_position(pos);
        sprite_.set_origin({});
        sprite_.set_mix({ColorConstant::electric_blue, 255});
    }


    void update(Time delta) override
    {
        delta = PLATFORM.delta_clock().last_delta();

        timer_ += delta;
        if (timer_ >= milliseconds(80)) {

            if (PLATFORM.screen().fade_active()) {
                kill();
            }

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



class Amplifier : public Room
{
public:
    Amplifier(Island* parent, const RoomCoord& position)
        : Room(parent, name(), position)
    {
    }


    void amplify_adjacent()
    {
        u8 xs = clamp(position().x - 1, 0, 15);
        u8 ys = clamp(position().y - 1, 0, 15);

        for (u8 x = xs; x < xs + 3; ++x) {
            for (u8 y = ys; y < ys + 3; ++y) {
                if (auto r = parent()->get_room({x, y})) {
                    r->amplify(true);
                }
            }
        }
    }


    void update(Time delta) override
    {
        Room::update(delta);
        Room::ready();

        static const auto burst_time = milliseconds(1000);

        burst_timer_ += delta;
        if (burst_timer_ > burst_time) {
            burst_timer_ -= burst_time;

            auto segment =
                [&](Fixnum xoff, Fixnum yoff, bool xflip, bool yflip) {
                    auto p = visual_center();
                    p.x += xoff;
                    p.y += yoff;
                    if (auto e = APP.alloc_entity<AmpAnim>(p)) {
                        e->sprite().set_flip({xflip, yflip});
                        APP.effects().push(std::move(e));
                    }
                };

            if (not PLATFORM.screen().fade_active()) {
                segment(Fixnum::from_integer(-16),
                        Fixnum::from_integer(-16),
                        false,
                        false);
                segment(Fixnum::from_integer(-16), 0.0_fixed, false, true);
                segment(0.0_fixed, Fixnum::from_integer(-16), true, false);
                segment(0.0_fixed, 0.0_fixed, true, true);
            }
        }

        amplify_adjacent();
    }


    void rewind(Time delta) override
    {
        Room::rewind(delta);

        amplify_adjacent();
    }


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::amplifier;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::amplifier;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer = SYSTR(description_amplifier)->c_str();
    }


    static Category category()
    {
        return Category::misc;
    }


    void display_on_hover(Platform::Screen& screen,
                          const RoomCoord& cursor) override
    {
        auto pos = position();

        auto origin = parent()->visual_origin();

        Sprite sprite;
        sprite.set_size(Sprite::Size::w16_h32);
        sprite.set_texture_index(13);
        sprite.set_mix({ColorConstant::electric_blue, 255});

        for (int x = pos.x - 1; x < pos.x + 2; ++x) {

            if (x == pos.x) {

                sprite.set_size(Sprite::Size::w16_h16);
                sprite.set_tidx_16x16(13, 1);
                sprite.set_position(
                    {origin.x + Fixnum::from_integer(x * 16),
                     origin.y + Fixnum::from_integer((pos.y - 1) * 16)});
            } else {
                sprite.set_size(Sprite::Size::w16_h32);
                sprite.set_texture_index(13);
                sprite.set_position(
                    {origin.x + Fixnum::from_integer(x * 16),
                     origin.y + Fixnum::from_integer((pos.y - 1) * 16)});
            }

            screen.draw(sprite);

            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);
            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y + 1) * 16)});
            screen.draw(sprite);
            sprite.set_texture_index(13);
        }
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::only_constructible_in_sandbox |
               RoomProperties::accepts_ion_damage;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1000.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "amplifier";
    }


    int debris_tile() override
    {
        return 1;
    }


    static SystemString ui_name()
    {
        return SystemString::block_amplifier;
    }


    static Icon icon()
    {
        return 4184;
    }


    static Icon unsel_icon()
    {
        return 4168;
    }

    TileId tile() const;

protected:
    TileId last_tile_;
    Time burst_timer_ = 0;
};



} // namespace skyland
