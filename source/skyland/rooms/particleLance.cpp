////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "particleLance.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/explosion/exploTrail.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void ParticleLance::format_description(StringBuffer<512>& buffer)
{
    buffer = SYSTR(description_particle_lance)->c_str();
}



ParticleLance::ParticleLance(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void ParticleLance::rewind(Time delta)
{
    Room::rewind(delta);

    if (active_) {
        ++flicker_cyc_;
        if (flicker_cyc_ == 5) {
            flicker_cyc_ = 0;
        }

        timer_ -= delta;
        if (timer_ <= 0) {
            timer_ += milliseconds(200);
            dmg_count_ -= 10;
            APP.camera()->shake(16);
            if (dmg_count_ <= 0) {
                active_ = false;
                dmg_count_ = 0;
                timer_ = 0;
            }
        }
    }
}



void ParticleLance::update(Time delta)
{
    Room::update(delta);

    if (parent() == APP.opponent_island()) {
        // Don't allow opponents to build this block.
        if (not APP.opponent().is_friendly()) {
            apply_damage(9999);
        }
    }

    if (active_) {
        ++flicker_cyc_;
        if (flicker_cyc_ == 5) {
            flicker_cyc_ = 0;
        }

        timer_ += delta;
        if (timer_ > milliseconds(200)) {
            timer_ -= milliseconds(200);
            project_damage(10);
            APP.camera()->shake(16);
        }
        ready();
    }
}



void ParticleLance::finalize()
{
    Room::finalize();

    if (health() == 0) {
        ExploSpawner::create(center());
        for (int i = 0; i < 3; ++i) {
            if (auto e =
                alloc_entity<ExploTrail>(center(),
                                         rng::choice<360>(rng::utility_state),
                                         1.25_fixed,
                                         seconds(2))) {
                APP.effects().push(std::move(e));
            }
        }
    }

    on_destroy();
}



void ParticleLance::on_destroy()
{
    time_stream::event::ParticleLanceDestroyed e;
    e.active_ = active_;
    e.accum_dmg_.set(dmg_count_);
    e.near_ = is_player_island(parent());
    e.x_ = position().x;
    e.y_ = position().y;
    APP.push_time_stream(e);
}



void ParticleLance::on_salvage()
{
    on_destroy();
}



ScenePtr ParticleLance::select_impl(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (not APP.opponent_island() or APP.opponent().is_friendly()) {
        return null_scene();
    }

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (active_) {
        return null_scene();
    }

    PLATFORM.speaker().play_sound("particle_beam.raw", 7);

    active_ = true;
    project_damage(10);

    ready();

    return null_scene();
}



void ParticleLance::display(Platform::Screen& screen)
{

    if (not APP.opponent_island()) {
        return;
    }

    if (not active_) {
        return;
    }

    Sprite spr;
    auto pos = visual_center();
    spr.set_size(Sprite::Size::w16_h16);

    if (parent() == &player_island()) {
        spr.set_palette(2);
    }

    pos.y -= 8.0_fixed;
    pos.x += 8.0_fixed + 16.0_fixed;

    spr.set_tidx_16x16(91, 0);
    spr.set_mix({ColorConstant::silver_white, 255});

    if (flicker_cyc_ > 2) {
        spr.set_tidx_16x16(74, 0);
        pos.y += 4.0_fixed;
    }

    Fixnum dist;
    dist +=
        pos.x + Fixnum::from_integer(
                    (APP.player_island().terrain().size() - position().x) * 16);
    if (APP.opponent_island()) {
        dist += APP.opponent_island()->get_position().x -
                APP.player_island().get_position().x;
        dist +=
            Fixnum::from_integer(APP.opponent_island()->terrain().size() * 16);
    }

    while (dist >= 16.0_fixed) {
        spr.set_position(pos);
        screen.draw(spr);
        pos.x += 16.0_fixed;
        dist -= 16.0_fixed;
    }
}



void ParticleLance::project_damage(Health damage)
{
    // NOTE: map is 16 wide, so for two islands, the max intersecting blocks
    // would be 32, I suppose minus the size of this room....
    Buffer<Room*, 32> rooms;

    dmg_count_ += damage;
    if (dmg_count_ >= max_health()) {
        apply_damage(dmg_count_);
    }

    auto push_room = [&](Room* room) {
        if (room not_eq this and not contains(rooms, room)) {
            rooms.push_back(room);
        }
    };

    for (u8 x = position().x; x < (u8)parent()->terrain().size(); ++x) {
        if (auto r = parent()->get_room({x, position().y})) {
            if (not is_forcefield(r->metaclass())) {
                push_room(r);
            }
            if (r not_eq this) {
                parent()->fire_create({x, position().y});
            }
        }
    }

    if (auto isle = other_island()) {
        for (u8 x = 0; x < 16; ++x) {
            if (auto r = isle->get_room({x, position().y})) {
                push_room(r);
            }
            isle->fire_create({x, position().y});
        }
    }

    for (auto& r : rooms) {
        r->apply_damage(damage);
    }
}



void ParticleLance::render_interior(App* app, TileId buffer[16][16])
{
    auto pos = position();
    buffer[pos.x][pos.y] = InteriorTile::particle_lance_1;
    buffer[pos.x + 1][pos.y] = InteriorTile::particle_lance_2;
    buffer[pos.x + 2][pos.y] = InteriorTile::particle_lance_3;
}



void ParticleLance::render_exterior(App* app, TileId buffer[16][16])
{
    auto pos = position();
    buffer[pos.x][pos.y] = Tile::particle_lance_1;
    buffer[pos.x + 1][pos.y] = Tile::particle_lance_2;
    buffer[pos.x + 2][pos.y] = Tile::particle_lance_3;
}



} // namespace skyland
