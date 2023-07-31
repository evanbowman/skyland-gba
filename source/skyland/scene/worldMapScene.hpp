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
#include "skyland/worldGraph.hpp"



namespace skyland
{



class WorldMap;



class WorldMapScene : public Scene
{
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev_scene) override;


    void exit(Platform&, App&, Scene& next_scene) override;


    void display(Platform&, App&) override;


private:
    void show_map(Platform&, WorldGraph& map, int storm_depth);


    void update_storm_frontier(Platform& pfrm, WorldGraph& map, int offset);


    bool can_abort_move() const;


    enum class State {
        fade_in,
        storm_scroll_in,
        storm_advance,
        deselected,
        selected,
        move,
        wait,
        save_selected,
        save_button_depressed,
        save_button_released_wait,
        help_selected,
        help_button_depressed,
        help_button_released_wait,
        settings_selected,
        settings_button_depressed,
        settings_button_released_wait,
        logbook_selected,
        logbook_button_depressed,
        logbook_button_released_wait,
        fade_out,
        fade_out_saved,
        fade_out_help,
        fade_out_settings,
        fade_out_logbook,
        print_saved_text,
        show_saved_text,
        show_node_death_icons,
        save_animate_out,
        save_exit,
        abort_move,
    } state_ = State::fade_in;

    bool move_arrow_sel_[3] = {false, true, false};

    void redraw_icons(Platform& pfrm);

    int prev_world_loc_ = 0;
    int cursor_ = 0;
    u8 cursor_keyframe_ = 0;
    Microseconds cursor_anim_timer_ = 0;
    Microseconds timer_ = 0;
    ColorMix cmix_;

    int movement_cursor_ = 0;
    Buffer<Vec2<s8>, 10> movement_targets_;
    Buffer<Vec2<s8>, 10> dead_nodes_;

    Microseconds storm_scroll_timer_ = 0;

    void render_map_key(Platform& pfrm, App&);

    static bool show_tier_2_;
    bool tier_2_visible_ = false;
    bool fast_ = false;
    bool has_radar_ = false;

    Microseconds tier_2_timer_ = 0;

    std::optional<Text> heading_;
    std::optional<Text> warning_;
    std::optional<Text> exit_label_;
    std::optional<Text> map_key_;
    // std::optional<Text> key_[3];
    std::optional<MediumIcon> save_icon_;
    std::optional<MediumIcon> help_icon_;
    std::optional<MediumIcon> settings_icon_;
    std::optional<MediumIcon> logbook_icon_;
};



} // namespace skyland
