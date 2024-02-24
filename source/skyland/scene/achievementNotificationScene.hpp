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

#include "skyland/achievement.hpp"
#include "worldScene.hpp"



namespace skyland
{



class AchievementNotificationScene : public WorldScene
{
public:
    AchievementNotificationScene(achievements::Achievement achievement,
                                 DeferredScene next_scene,
                                 bool skip_fade = false)
        : achievement_(achievement), next_scene_(next_scene),
          skip_fade_(skip_fade)
    {
    }


    ScenePtr<Scene> update(Time delta) override;


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


private:
    enum class State {
        fade_in,
        animate_box_sweep,
        wait,
        fade_out,
    } state_ = State::fade_in;


    achievements::Achievement achievement_;

    Optional<Text> achievement_text_;
    Optional<Text> achievement_name_;
    Optional<Text> item_name_;
    Optional<Text> item_details_;

    Optional<Text> unlocked_text_;


    DeferredScene next_scene_;

    bool skip_fade_;


    Time timer_ = 0;
};



} // namespace skyland
