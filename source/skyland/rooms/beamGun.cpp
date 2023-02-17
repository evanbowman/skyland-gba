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


#include "beamGun.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/beam.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(beam_reload_ms);



extern Sound cannon_sound;



void BeamGun::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_beam_gun)->c_str();
}



BeamGun::BeamGun(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * beam_reload_ms)
{
}



class BeamSpawner : public Entity
{
public:
    BeamSpawner(const Vec2<Fixnum>& position,
                const Vec2<Fixnum>& target,
                Island* source,
                const RoomCoord& origin_tile)
        : Entity({}), position_(position), target_(target), source_(source),
          origin_tile_(origin_tile)
    {
        sprite_.set_alpha(Sprite::Alpha::transparent);
    }

    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta;
        if (timer_ > milliseconds(32)) {
            timer_ -= milliseconds(32);
            if (++beam_count_ < 10) {
                auto c = app.alloc_entity<Beam>(pfrm,
                                                position_,
                                                target_,
                                                source_,
                                                origin_tile_,
                                                beam_count_);
                if (c) {
                    source_->projectiles().push(std::move(c));
                }

            } else {
                kill();

                auto e = alloc_entity<AnimatedEffect>(
                    position_, 47, 49, milliseconds(100));
                if (e) {
                    app.effects().push(std::move(e));
                }
            }
        }
    }


private:
    const Vec2<Fixnum> position_;
    const Vec2<Fixnum> target_;
    Island* source_;
    Microseconds timer_ = 0;
    int beam_count_ = 0;
    RoomCoord origin_tile_;
};



void BeamGun::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += Fixnum::from_integer(target_->x * 16 + 8);
    origin.y += Fixnum::from_integer(target_->y * 16 + 8);
    target = origin;

    app.camera()->shake(6);

    auto start = center();

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    if (island == &app.player_island()) {
        start.x -= 32.0_fixed;
    } else {
        start.x += 32.0_fixed;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<4>(target, rng::critical_state);
    }

    cannon_sound.play(pfrm, 3);

    auto c = app.alloc_entity<BeamSpawner>(
        pfrm, start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Microseconds BeamGun::reload() const
{
    return 1000 * beam_reload_ms;
}



void BeamGun::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::beam_gun_1;
    buffer[position().x + 1][position().y] = Tile::beam_gun_2;
    buffer[position().x + 2][position().y] = Tile::beam_gun_3;
}



void BeamGun::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::beam_gun_1;
    buffer[position().x + 1][position().y] = Tile::beam_gun_2;
    buffer[position().x + 2][position().y] = Tile::beam_gun_3;
}



} // namespace skyland
