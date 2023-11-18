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


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class TitleScreenScene : public Scene
{
public:
    TitleScreenScene(int start_page = 1);

    void enter(App&, Scene& prev) override;
    void exit(App&, Scene& next) override;


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


    Float ambient_movement() const
    {
        return ambient_movement_;
    }


    void macro_gen_sample_island(App&);


private:
    bool wait_ = true;
    std::optional<Text> text_;
    Microseconds timer_ = 0;
    Microseconds hover_timer_ = 0;
    Microseconds selector_timer_ = 0;
    Float ambient_movement_ = 0.f;
    bool selector_shaded_ = false;
    bool flower_effect_ = false;

    void run_init_scripts(App& app, bool allow_mods);


    void show_module_icons(int page);

    static u8 module_page_;
    static std::optional<Vec2<u8>> module_cursor_;


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

    Microseconds island_mov_timer_ = 0;
    int island_offset_ = 0;
    Microseconds bird_timer_ = seconds(7);
    Microseconds note_timer_ = milliseconds(600);
    Microseconds furnace_timer_ = 0;
};



} // namespace skyland
