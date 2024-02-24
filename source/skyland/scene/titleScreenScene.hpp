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



class TitleScreenScene : public Scene
{
public:
    TitleScreenScene(int start_page = 1);

    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


    Float ambient_movement() const
    {
        return ambient_movement_;
    }


    void macro_gen_sample_island();


private:
    bool wait_ = true;
    Optional<Text> text_;
    Time timer_ = 0;
    Time hover_timer_ = 0;
    Time selector_timer_ = 0;
    Float ambient_movement_ = 0.f;
    bool selector_shaded_ = false;
    bool flower_effect_ = false;

    void run_init_scripts(bool allow_mods);


    void show_module_icons(int page);

    static u8 module_page_;
    static Optional<Vec2<u8>> module_cursor_;


    void play_gust_sound();


    struct Pong
    {
        struct Pad
        {
            Float speed_;
            Float pos_;
        };

        Pad pad1_ = {0.2f, 0};
        Pad pad2_ = {0.2f, 10};

        Buffer<Float, 3> pad1_trail_;
        Buffer<Float, 3> pad2_trail_;

        Buffer<Vec2<Float>, 3> ball_trail_;
        Time trail_timer_ = 0;
        Time pad_trail_timer_ = 0;

        Vec2<Float> ball_ = {3, 3};
        Vec2<Float> ball_speed_ = {0.35f, 0.35f};

        void update(bool sound_effects);
        void display(int x_scroll);
    } pong_;


    void window_image_hack(u16 empty_tile);

    enum class State {
        fade_in,
        quick_fade_in,
        wait,
        fade_out,
        scroll_right,
        scroll_left,
        scroll_macro,
        scroll_multiplayer,
        scroll_to_center,
        scroll_to_macro,
        wait_2,
        scroll_to_end,
        scroll_from_end,
        fade_modules_1,
        show_modules,
        fade_modules_backout,
        resume_end,
        resume_macro,
        resume_challenges,
        macro_island_init,
        macro_island_enter,
        macro_island,
        macro_island_exit,
        scroll_archives,
    } state_ = State::fade_in;

    int menu_selection_ = 0;

    void put_menu_text();
    void put_module_text();
    void redraw_margins();

    int menu_selection_start_ = 0;
    int menu_selection_stop_ = 0;

    int x_scroll_ = 0;
    s16 v_scroll_1_ = 0;
    s16 v_scroll_2_ = 0;

    bool dev_;

    bool repeat_left_ = false;
    bool repeat_right_ = false;
    bool repeat_action1_ = false;

    Time island_mov_timer_ = 0;
    int island_offset_ = 0;
    Time bird_timer_ = seconds(7);
    Time note_timer_ = milliseconds(600);
    Time furnace_timer_ = 0;
};



} // namespace skyland
