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


#pragma once



#include "allocator.hpp"
#include "confetti.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



// This class in particlar needs to be refactored... but not a priority right
// now. I originally lifted this explosion code from another one of my games,
// then I needed to graft on a feature to display the victory/defeat text
// without displaying the explosion animation, and overall, the code is a bit
// cluttered.



namespace skyland
{



class Island;



class PlayerIslandDestroyedScene : public WorldScene
{
public:
    enum class AnimState {
        init,
        explosion_wait1,
        explosion_wait2,
        begin_fade,
        begin_fade2,
        fade,
        wait_1,
        show_coins,
        wait_2,
        fade_out,
        idle,
        fade_complete,
        show_options,
        level_exit_forced,
    };


    PlayerIslandDestroyedScene(Island* island)
        : sink_speed_(0.000011f), island_(island)
    {
    }


    // Skip the explosion and just display the victory/defeat screen. TODO:
    // split this class into two scenes.
    PlayerIslandDestroyedScene(Island* island, bool victory) : island_(island)
    {
        if (victory) {
            anim_state_ = AnimState::level_exit_forced;
        }

        sink_speed_ = 0.f;

        options_allowed_ = false;
    }


    ScenePtr<Scene> update(Microseconds delta) override;
    void display() override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


private:
    Microseconds timer_ = 0;
    Fixnum sink_speed_;
    Island* island_;

    bool circ_effect_ = false;

    Buffer<Text, 5> lines_;

    bool options_allowed_ = true;

    bool fastforward_ = false;

    std::optional<DynamicMemory<ConfettiBuffer>> confetti_;

    void show_stats();

    Microseconds stat_timer_ = 0;

    AnimState anim_state_ = AnimState::init;

    enum class ConfettiState {
        dormant,
        wait_1,
        confetti_pop_1,
        wait_2,
        confetti_pop_2,
        wait_3,
    } confetti_state_ = ConfettiState::dormant;

    u16 circ_effect_radius_ = 0;
    u16 last_radius_ = 0;
    Microseconds confetti_timer_ = 0;
    Microseconds music_fadeback_timer_ = 0;
    bool restore_volume_ = true;
    bool forced_defeat_ = false;

    int level_seconds_ = 0;
    int rooms_built_ = 0;
    int rooms_lost_ = 0;
};



} // namespace skyland
