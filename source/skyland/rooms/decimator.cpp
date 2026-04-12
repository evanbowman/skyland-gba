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
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static const Time electric_ripple_effect_rate = milliseconds(140);



class ElectricRippleEffectQuarter : public Entity
{
public:
    ElectricRippleEffectQuarter(Platform::DynamicTexturePtr dt,
                                const Vec2<Fixnum>& pos,
                                int quarter)
        : Entity({{}, {}}), dt_(dt), timer_(0), quarter_(quarter)
    {
        sprite_.set_texture_index(dt->mapping_index());

        auto p = pos;

        switch (quarter) {
        case 1:
            sprite_.set_flip({true, false});
            p.x += 32.0_fixed;
            break;

        case 2:
            sprite_.set_flip({false, true});
            p.y += 32.0_fixed;
            break;

        case 3:
            sprite_.set_flip({true, true});
            p.x += 32.0_fixed;
            p.y += 32.0_fixed;
            break;
        }

        sprite_.set_position(p);

        dt_->remap(74 * 2);
    }


    void shade(ColorConstant clr)
    {
        sprite_.set_mix({clr, 255});
    }


    void update(Time delta) override
    {
        timer_ += delta * 2;

        if (timer_ > electric_ripple_effect_rate) {
            timer_ -= electric_ripple_effect_rate;


            if (keyframe_ == 4) {
                kill();
            } else {
                keyframe_++;
                if (quarter_ == 0) {
                    dt_->remap((74 - keyframe_) * 2);
                }
            }
        }
    }


    void rewind(Time delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ += electric_ripple_effect_rate;

            if (keyframe_ == 0) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((74 - keyframe_) * 2);
                }
                keyframe_--;
            }
        }
    }


private:
    Platform::DynamicTexturePtr dt_;
    Time timer_ = 0;
    int quarter_;
    int keyframe_ = 0;
};



extern SharedVariable energy_glow_color;



void electric_ripple(const Vec2<Fixnum>& pos)
{
    auto dt = PLATFORM.make_dynamic_texture();
    if (dt) {
        auto p = pos;
        p.x -= 32.0_fixed;
        p.y -= 32.0_fixed;
        auto make_segment = [&](int q) {
            auto e = APP.alloc_entity<ElectricRippleEffectQuarter>(*dt, p, q);
            if (e) {
                e->shade(custom_color(energy_glow_color));
                return APP.effects().push(std::move(e));
            }
        };
        make_segment(3);
        make_segment(2);
        make_segment(1);
        make_segment(0);
    }
}



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

    if (parent() == &player_island()) {
        spr.set_palette(2);
    }

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

    if (is_offline()) {
        if (is_cold_boot()) {
            // Decimator has a long boot time and doesn't have cold reboot.
            cold_boot_completed();
        }
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

            const auto charge_sound_trigger = milliseconds(1450);

            if (reload_ > charge_sound_trigger and
                reload_ - delta <= charge_sound_trigger) {
                auto dec_charge_sound = "decimator_charge.raw";
                if (not PLATFORM.speaker().is_sound_playing(dec_charge_sound)) {
                    PLATFORM.speaker().play_sound(dec_charge_sound, 5);
                }

            }

            auto ripple_effect_trigger = electric_ripple_effect_rate * 3 - milliseconds(40);
            if (reload_ > ripple_effect_trigger and reload_ - delta <= ripple_effect_trigger) {
                auto c = visual_center();
                if (is_player_island(parent())) {
                    c.x += 16.0_fixed;
                } else {
                    c.x -= 16.0_fixed;
                }
                electric_ripple(c);
            }

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
                    APP.push_time_stream(e);
                } else {
                    time_stream::event::OpponentRoomReloadComplete e;
                    e.room_x_ = position().x;
                    e.room_y_ = position().y;
                    APP.push_time_stream(e);
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
                APP.push_time_stream(e);
            } else {
                time_stream::event::OpponentDecimatorBurstCreated e;
                e.src_x_ = position().x;
                e.src_y_ = position().y;
                e.prev_counter_ = counter_;
                APP.push_time_stream(e);
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

    if (is_offline()) {
        if (is_cold_boot()) {
            cold_boot_completed();
        }
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



void Decimator::rewind_enter_cold_boot()
{
    // ignore
}


void Decimator::force_disable_cold_boot_impl()
{
    // ignore
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
        dramatic_explosion(center());
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
