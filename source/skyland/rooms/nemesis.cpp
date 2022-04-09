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


#include "nemesis.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/nemesisBlast.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern Sound cannon_sound;



SHARED_VARIABLE(nemesis_reload_ms);
extern SharedVariable nemesis_blast_damage;



void Nemesis::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    auto secs = nemesis_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_nemesis)->c_str(),
                nemesis_blast_damage,
                nemesis_blast_damage * 2,
                nemesis_blast_damage * 4,
                secs,
                (nemesis_reload_ms / 100 - secs * 10));
}



Nemesis::Nemesis(Island* parent, const Vec2<u8>& position)
    : Weapon(parent, name(), position, 1000 * nemesis_reload_ms)
{
}



void Nemesis::fire(Platform& pfrm, App& app)
{
    auto island = other_island(app);

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += target_->x * 16 + 8;
    origin.y += target_->y * 16 + 8;
    target = origin;

    app.camera()->shake(4);

    auto start = center();

    if (island == &app.player_island()) {
        start.x -= 22;
    } else {
        start.x += 22;
    }

    if (not pfrm.network_peer().is_connected() and
        app.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(pfrm, 3);

    auto v = app.alloc_entity<NemesisBlast>(
        pfrm, start, target, parent(), position());
    if (v) {
        if (health() < max_health() / 4) {
            v->set_variant(2);
        } else if (health() < max_health() / 2) {
            v->set_variant(1);
        }

        parent()->projectiles().push(std::move(v));
    }
}



Microseconds Nemesis::reload() const
{
    return 1000 * nemesis_reload_ms;
}



void Nemesis::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::nemesis_1;
    buffer[position().x + 1][position().y] = InteriorTile::nemesis_2;
}



void Nemesis::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::nemesis_1;
    buffer[position().x + 1][position().y] = Tile::nemesis_2;
}



} // namespace skyland
