////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "decimator.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(decimator_reload_ms);



void Decimator::format_description(StringBuffer<512>& buffer)
{
    auto secs = decimator_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_decimator)->c_str(),
                secs,
                decimator_reload_ms / 100 - secs * 10);
}



Decimator::Decimator(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



static const Time burst_interval = milliseconds(200);



void Decimator::unset_target()
{
}



void Decimator::on_level_start()
{
    reload_ = 1000 * decimator_reload_ms;
    firing_ = false;
    counter_ = 0;
}



void Decimator::display(Platform::Screen& screen)
{
    Room::display(screen);

    if (not APP.opponent_island()) {
        return;
    }

    Sprite spr;
    auto pos = visual_center();
    spr.set_size(Sprite::Size::w16_h16);

    pos.y -= 16.0_fixed;

    if (reload_ < seconds(7) and not counter_) {
        ++flicker_cyc_;
        flicker_cyc_ %= 4;
        if (flicker_cyc_ < 2) {
            return;
        }
        spr.set_tidx_16x16(103, 1);
    } else {
        return;
    }

    if (is_player_island(parent())) {

        Fixnum dist;

        auto cy = position().y;
        bool found_any = false;

        for (u8 x = 0; x < APP.opponent_island()->terrain().size(); ++x) {
            for (u8 y = 0; y < 2; ++y) {
                RoomCoord c;
                c.x = x;
                c.y += cy + y;
                if (APP.opponent_island()->get_room(c)) {
                    found_any = true;
                    goto NEXT;
                }
            }
            dist += 16.0_fixed;
        }
    NEXT:
        if (not found_any) {
            dist += 48.0_fixed;
        }

        pos.x += 15.0_fixed;

        // Add dist between islands:
        dist += APP.opponent_island()->get_position().x - pos.x;

        while (dist >= 16.0_fixed) {
            spr.set_position(pos);
            screen.draw(spr);
            pos.x += 16.0_fixed;
            dist -= 16.0_fixed;
        }

        if (dist >= 0.0_fixed) {
            pos.x -= (16.0_fixed - dist);
            spr.set_position(pos);
            screen.draw(spr);
        }


    } else {
        Fixnum dist;

        auto cy = position().y;
        bool found_any = false;

        s8 x = APP.player_island().terrain().size() - 1;
        for (; x > -1; --x) {
            for (u8 y = 0; y < 2; ++y) {
                RoomCoord c;
                c.x = x;
                c.y += cy + y;
                if (APP.player_island().get_room(c)) {
                    found_any = true;
                    goto NEXT2;
                }
            }
            dist += 16.0_fixed;
        }
    NEXT2:
        if (not found_any) {
            dist += 48.0_fixed;
        }

        pos.x -= 31.0_fixed;

        // Add dist between islands:
        dist += pos.x - (APP.player_island().get_position().x +
                         Fixnum::from_integer(
                             (APP.player_island().terrain().size() - 1) * 16));

        while (dist >= 16.0_fixed) {
            spr.set_position(pos);
            screen.draw(spr);
            pos.x -= 16.0_fixed;
            dist -= 16.0_fixed;
        }

        if (dist >= 0.0_fixed) {
            pos.x += (16.0_fixed - dist);
            spr.set_position(pos);
            screen.draw(spr);
        }
    }
}



void Decimator::update(Time delta)
{
    Room::update(delta);

    Room::ready();

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (is_powered_down()) {
        return;
    }

    if (mt_prep_seconds) {
        return;
    }

    bool has_pilot = false;
    for (auto& chr : characters()) {
        if (chr->owner() == &parent()->owner()) {
            has_pilot = true;
        }
    }

    if (not APP.opponent_island()) {
        return;
    }

    const bool opponent_friendly =
        is_player_island(parent()) and
        static_cast<Opponent&>(APP.opponent_island()->owner()).is_friendly();


    if (has_pilot and reload_ > 0) {

        if (not opponent_friendly) {
            reload_ -= delta;

            if (parent()->phase() and reload_ < 0) {
                // wait until un-phazed
                reload_ = 1;
            }

            if (reload_ < 0 and not firing_) {
                if (is_player_island(parent())) {
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
            }
        }
    } else if (has_pilot) {
        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island();

        if (island and not island->is_destroyed()) {
            APP.camera()->shake(4);

            auto start = center();

            // This just makes it a bit less likely for cannonballs to
            // run into the player's own buildings, especially around
            // corners.
            if (is_player_island(island)) {
                start.x -= 18.0_fixed;
            } else {
                start.x += 18.0_fixed;
            }

            auto target = center();
            if (is_player_island(parent())) {
                target.x += 100.0_fixed;
            } else {
                target.x -= 100.0_fixed;
            }

            if (is_player_island(parent())) {
                time_stream::event::PlayerDecimatorBurstCreated e;
                e.src_x_ = position().x;
                e.src_y_ = position().y;
                e.prev_counter_ = counter_;
                APP.time_stream().push(APP.level_timer(), e);
            } else {
                time_stream::event::OpponentDecimatorBurstCreated e;
                e.src_x_ = position().x;
                e.src_y_ = position().y;
                e.prev_counter_ = counter_;
                APP.time_stream().push(APP.level_timer(), e);
            }

            firing_ = true;

            auto c = APP.alloc_entity<DecimatorBurst>(
                start, target, parent(), position());

            if (c) {
                c->burst_index_ = counter_;
                parent()->projectiles().push(std::move(c));
                set_ai_aware(true);
            }

            if (counter_ < 6) {
                ++counter_;
                reload_ += burst_interval;
            } else {
                reload_ += interval();
                counter_ = 0;
                flicker_cyc_ = 0;
                firing_ = false;
            }
        }
    }
}



void Decimator::rewind(Time delta)
{
    Room::rewind(delta);

    if (parent()->phase()) {
        return;
    }

    if (is_powered_down()) {
        return;
    }

    if (firing_) {
        reload_ += delta;
    } else {
        if (reload_ <= 0) {
            // Reloaded.
        } else if (reload_ < interval()) {
            reload_ += delta;
        }
    }
}



Time Decimator::interval() const
{
    Time ret = 1000 * decimator_reload_ms;
    if (amplify_) {
        // reload time times 5/8
        ret *= 5;
        ret /= 8;
    }
    return ret;
}



void Decimator::___rewind___finished_reload()
{
    reload_ = 1;
}



void Decimator::___rewind___ability_used()
{
    // reload_ = 0;
}



void Decimator::plot_walkable_zones(bool matrix[16][16],
                                    Character* for_character)
{
    auto pos = position();

    if (is_player_island(parent())) {
        matrix[pos.x][pos.y + 1] = true;
    } else {
        matrix[pos.x + 1][pos.y + 1] = true;
    }
}



void Decimator::render_interior(App* app, TileId buffer[16][16])
{
    auto pos = position();

    bool right = false;
    if (app) {
        right = parent() == &app->player_island();
    }

    if (right) {
        buffer[pos.x + 1][pos.y] = InteriorTile::decimator_1;
        buffer[pos.x + 1][pos.y + 1] = InteriorTile::decimator_2;
        buffer[pos.x][pos.y + 1] = InteriorTile::plain_floor;
        buffer[pos.x][pos.y] = InteriorTile::decimator_int;
    } else {
        buffer[pos.x][pos.y] = InteriorTile::decimator_1;
        buffer[pos.x][pos.y + 1] = InteriorTile::decimator_2;
        buffer[pos.x + 1][pos.y + 1] = InteriorTile::plain_floor;
        buffer[pos.x + 1][pos.y] = InteriorTile::decimator_int;
    }
}



void Decimator::render_exterior(App* app, TileId buffer[16][16])
{
    auto pos = position();

    bool right = false;
    if (app) {
        right = parent() == &app->player_island();
    }

    if (right) {
        buffer[pos.x + 1][pos.y] = Tile::decimator_1;
        buffer[pos.x + 1][pos.y + 1] = Tile::decimator_2;
        buffer[pos.x][pos.y] = Tile::armored_wall_1;
        buffer[pos.x][pos.y + 1] = Tile::wall_plain_2;
    } else {
        buffer[pos.x][pos.y] = Tile::decimator_1;
        buffer[pos.x][pos.y + 1] = Tile::decimator_2;
        buffer[pos.x + 1][pos.y] = Tile::armored_wall_1;
        buffer[pos.x + 1][pos.y + 1] = Tile::wall_plain_2;
    }
}



void Decimator::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



void Decimator::rewind_started_firing()
{
    for (auto& p : parent()->projectiles()) {
        if (auto b = p->cast_decimator_burst()) {
            if (b->origin_tile() == position()) {
                b->kill();
            }
        }
    }
    firing_ = false;
    counter_ = 0;
    reload_ = 0;
}



void Decimator::rewind_projectile_created(int new_counter)
{
    for (auto& p : parent()->projectiles()) {
        if (auto b = p->cast_decimator_burst()) {
            if (b->burst_index_ >= counter_) {
                b->kill();
            }
        }
    }

    counter_ = new_counter;

    reload_ = burst_interval;
}



void Decimator::amplify(bool enable)
{
    amplify_ = enable;

    if (amplify_) {
        reload_ = std::min(reload_, interval());
    }
}



} // namespace skyland
