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


#include "phaseShifter.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland
{



SHARED_VARIABLE(phase_shifter_cooldown_ms);
SHARED_VARIABLE(phase_shifter_duration_ms);



PhaseShifter::PhaseShifter(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
    timer_ = milliseconds(phase_shifter_cooldown_ms);
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
                timer_ = milliseconds(phase_shifter_cooldown_ms);
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
            timer_ -= delta;

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
        if (timer_ > milliseconds(phase_shifter_cooldown_ms)) {
            timer_ = milliseconds(phase_shifter_cooldown_ms);
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
