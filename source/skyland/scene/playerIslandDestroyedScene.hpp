////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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


    ScenePtr update(Time delta) override;
    void display() override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


private:
    Time timer_ = 0;
    Fixnum sink_speed_;
    Island* island_;

    bool circ_effect_ = false;
    bool saved_already_ = false;

    Buffer<Text, 5> lines_;

    bool options_allowed_ = true;

    Optional<DynamicMemory<ConfettiBuffer>> confetti_;

    void show_stats();

    Time stat_timer_ = 0;

    AnimState anim_state_ = AnimState::init;

    void handle_zone_exit();

    void try_autosave();

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
    Time confetti_timer_ = 0;
    Time music_fadeback_timer_ = 0;
    bool restore_volume_ = true;
    bool forced_defeat_ = false;

    int level_seconds_ = 0;
    int rooms_built_ = 0;
    int rooms_lost_ = 0;
};



} // namespace skyland
