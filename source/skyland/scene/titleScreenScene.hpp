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

    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    bool wait_ = true;
    std::optional<Text> text_;
    Microseconds timer_ = 0;
    Microseconds hover_timer_ = 0;
    Microseconds selector_timer_ = 0;
    Float ambient_movement_ = 0.f;
    bool selector_shaded_ = false;
    bool flower_effect_ = false;

    void run_init_scripts(Platform& pfrm, App& app, bool allow_mods);


    void show_module_icons(Platform&, int page);

    u8 module_page_ = 0;
    std::optional<Vec2<u8>> module_cursor_;


    void play_gust_sound(Platform& pfrm);


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

        void update();
        void display(Platform&, int x_scroll);
    } pong_;


    void window_image_hack(Platform&, u16 empty_tile);

    enum class State {
        fade_in,
        quick_fade_in,
        wait,
        fade_out,
        scroll_right,
        scroll_left,
        scroll_multiplayer,
        scroll_to_center,
        wait_2,
        scroll_to_end,
        scroll_from_end,
        fade_modules_1,
        show_modules,
        fade_modules_backout,
        resume_end,
        resume_challenges,
    } state_ = State::fade_in;

    int menu_selection_ = 0;

    void put_menu_text(Platform&);
    void put_module_text(Platform&);
    void redraw_margins(Platform&);

    int menu_selection_start_ = 0;
    int menu_selection_stop_ = 0;

    int x_scroll_ = 0;

    Microseconds island_mov_timer_ = 0;
    int island_offset_ = 0;
    Microseconds bird_timer_ = seconds(7);
};



} // namespace skyland
