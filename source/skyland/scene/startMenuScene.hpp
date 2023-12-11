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
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class StartMenuScene : public Scene
{
public:
    StartMenuScene(int fade_direction, int default_cursor = 0);


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


    bool cascade_anim_in_ = false;

private:
    enum class State {
        init,
        enter,
        idle,
        clear,
        partial_clear,
        fade_out,
        sweep_up,
        after_sweep,
        exit,
        cut,
    } state_ = State::init;


    static constexpr const u32 option_count = 10;


    enum TransitionMode {
        kill_menu,
        cut,
        fade_sweep,
        fade_sweep_transparent_text,
    };


    struct Data
    {
        Buffer<Text, option_count> text_;

        Buffer<StringBuffer<32>, option_count> disp_queue_;
        u8 disp_count_;
        Time disp_timer_ = 0;

        struct TransitionInfo
        {
            DeferredScene next_scene_;
            TransitionMode mode_;
        };
        Buffer<TransitionInfo, option_count> on_click_;
        Buffer<StringBuffer<32>, option_count> option_names_;

        u8 cursor_ = 0;
    };

    Float y_offset_ = 0.f;

    Float diff_percent_ = 0.3f;
    int add_offset_ = 0;

    void add_option(const char* str,
                    DeferredScene on_click,
                    TransitionMode transition_mode);


    DynamicMemory<Data> data_;

    int fade_direction_ = 0;

    Time timer_ = 0;
    int start_y_ = 3;

    bool preserve_transparency_ = false;
};



} // namespace skyland
