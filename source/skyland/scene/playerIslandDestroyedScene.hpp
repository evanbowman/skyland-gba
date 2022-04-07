////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
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


    PlayerIslandDestroyedScene(Island* island) : island_(island)
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


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
    void display(Platform&, App&) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


private:
    Microseconds timer_ = 0;
    Float sink_speed_ = 0.000011f;
    Island* island_;

    Buffer<Text, 5> lines_;

    bool options_allowed_ = true;

    std::optional<DynamicMemory<ConfettiBuffer>> confetti_;

    void show_stats(Platform&, App&);

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

    Microseconds confetti_timer_ = 0;
    Microseconds music_fadeback_timer_ = 0;
};



} // namespace skyland
