////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "phaseShifter.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class PhaseRippleEffectQuarter : public Entity
{
public:
    PhaseRippleEffectQuarter(Platform::DynamicTexturePtr dt,
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

        dt_->remap(70 * 2);
    }


    void shade(ColorConstant clr)
    {
        sprite_.set_mix({clr, 255});
    }


    void update(Time delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(200)) {
            timer_ -= milliseconds(200);


            if (keyframe_ == 4) {
                kill();
            } else {
                keyframe_++;
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
            }
        }
    }


    void jump_to_end()
    {
        timer_ = milliseconds(200);
        keyframe_ = 4;
        dt_->remap((70 + keyframe_) * 2);
    }


    void rewind(Time delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ += milliseconds(200);

            if (keyframe_ == 0) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
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



void phase_ripple(const Vec2<Fixnum>& pos)
{
    auto dt = PLATFORM.make_dynamic_texture();
    if (dt) {
        auto p = pos;
        p.x -= 32.0_fixed;
        p.y -= 32.0_fixed;
        auto make_segment = [&](int q) {
            auto e = APP.alloc_entity<PhaseRippleEffectQuarter>(*dt, p, q);
            if (e) {
                e->shade(ColorConstant::electric_blue);
                return APP.effects().push(std::move(e));
            }
        };
        make_segment(3);
        make_segment(2);
        make_segment(1);
        make_segment(0);
    }
}


SHARED_VARIABLE(phase_shifter_cooldown_ms);
SHARED_VARIABLE(phase_shifter_duration_ms);



Time PhaseShifter::cooldown_interval() const
{
    auto ret = milliseconds(phase_shifter_cooldown_ms);
    if (amplify_) {
        // reload time times 5/8
        ret *= 5;
        ret /= 8;
    }
    return ret;
}



PhaseShifter::PhaseShifter(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
    timer_ = cooldown_interval();
}



void PhaseShifter::format_description(StringBuffer<512>& buffer)
{
    make_format(buffer,
                SYSTR(description_phase_shifter)->c_str(),
                phase_shifter_duration_ms / 1000,
                phase_shifter_cooldown_ms / 1000);
}



void PhaseShifter::rewind_state(time_stream::event::PhaseMode p)
{
    switch (p) {
    case time_stream::event::PhaseMode::loading:
        timer_ = 0;
        activated_ = false;
        loaded_ = false;
        break;

    case time_stream::event::PhaseMode::loaded:
        timer_ = 0;
        loaded_ = true;
        activated_ = true;
        break;

    case time_stream::event::PhaseMode::phased:
        timer_ = 0;
        loaded_ = false;
        activated_ = true;
        break;
    }

    schedule_repaint();
}



void PhaseShifter::finalize()
{
    Room::finalize();

    if (activated_) {
        parent()->set_phase(0);
    }
}



void PhaseShifter::update(Time delta)
{
    Room::update(delta);

    if (is_powered_down()) {
        parent()->set_phase(0);
        return;
    }

    Room::ready();

    if (activated_) {

        if (timer_ >= 0) {
            timer_ -= delta;

            if (timer_ <= 0) {
                activated_ = false;
                parent()->set_phase(0);
                timer_ = cooldown_interval();
                schedule_repaint();
                PLATFORM.speaker().play_sound("exit_phase.raw", 6);
                time_stream::event::PhaseShifterStateChange e;
                e.x_ = position().x;
                e.y_ = position().y;
                e.prev_mode_ = time_stream::event::PhaseMode::phased;
                e.near_ = is_player_island(parent());
                APP.time_stream().push(APP.level_timer(), e);
            }
        }
    } else {

        if (loaded_) {
            timer_ = 0;
        } else if (timer_ >= 0) {

            if (not parent()->phase()) {
                timer_ -= delta;
            }

            if (timer_ <= 0) {
                loaded_ = true;
                schedule_repaint();

                time_stream::event::PhaseShifterStateChange e;
                e.x_ = position().x;
                e.y_ = position().y;
                e.prev_mode_ = time_stream::event::PhaseMode::loading;
                e.near_ = is_player_island(parent());
                APP.time_stream().push(APP.level_timer(), e);
            }
        }
    }
}



void PhaseShifter::rewind(Time delta)
{
    Room::rewind(delta);

    if (is_powered_down()) {
        return;
    }

    if (activated_) {
        timer_ += delta;
        if (timer_ > milliseconds(phase_shifter_duration_ms)) {
            timer_ = milliseconds(phase_shifter_duration_ms);
        }
    } else if (not loaded_) {
        timer_ += delta;
        if (timer_ > cooldown_interval()) {
            timer_ = cooldown_interval();
        }
    }
}



ScenePtr PhaseShifter::select_impl(const RoomCoord& cursor)
{
    if (not loaded_) {
        return null_scene();
    }

    const bool was_activated = activated_;

    activated_ = true;
    loaded_ = false;
    schedule_repaint();

    PLATFORM.speaker().play_sound("enter_phase.raw", 6);

    phase_ripple(visual_center());

    time_stream::event::PhaseShifterStateChange e;
    e.x_ = position().x;
    e.y_ = position().y;
    e.prev_mode_ = time_stream::event::PhaseMode::loaded;
    e.near_ = is_player_island(parent());
    APP.time_stream().push(APP.level_timer(), e);

    if (not was_activated) {
        schedule_repaint();
        timer_ = milliseconds(phase_shifter_duration_ms);
    }

    parent()->set_phase(1);

    Room::ready();

    return null_scene();
}



void PhaseShifter::amplify(bool enabled)
{
    amplify_ = enabled;
}



void PhaseShifter::render_interior(App* app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    TileId base = InteriorTile::phase_shifter_1;
    if (not loaded_) {
        base = InteriorTile::phase_shifter_1_charging;
    }

    buffer[x][y] = base;
    buffer[x][y + 1] = base + 1;
    buffer[x][y + 2] = base + 2;
}



void PhaseShifter::render_exterior(App* app, TileId buffer[16][16])
{
    const auto x = position().x;
    const auto y = position().y;

    TileId base = Tile::phase_shifter_1;
    if (not loaded_) {
        base = Tile::phase_shifter_1_charging;
    }

    buffer[x][y] = base;
    buffer[x][y + 1] = base + 1;
    buffer[x][y + 2] = base + 2;
}



} // namespace skyland
