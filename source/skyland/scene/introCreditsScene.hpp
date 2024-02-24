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


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class IntroCreditsScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


    static void show_sunflowers(int scroll, Float darken);


    bool flower_effect_ = false;


private:
    bool wait_ = true;
    bool exit_ = false;
    Optional<Text> text_;
    Optional<Text> copyright_text_;
    Time timer_ = 0;
    Time flower_effect_timer_ = 0;

    Time bird_anim_timer_ = 0;
    Time bird_seq_timer_ = 0;
    Time bird_seq_timer2_ = 0;
    Time bird_seq_timer3_ = 0;
    u8 bird_anim_ = 1;
    u8 bird_state_ = 0;
};



} // namespace skyland
