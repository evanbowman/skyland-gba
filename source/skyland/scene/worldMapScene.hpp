////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "skyland/worldGraph.hpp"



namespace skyland
{



class WorldMap;



namespace detail
{

enum class ShadeIntensity : u8 {
    none = 0,
    light,
    medium,
    dark
};

struct ShadeRangeComponent
{
    u8 x_;
    u8 y_;
    Sprite::Size sz_;
    u8 intensity_;
};

}



class WorldMapScene : public Scene
{
public:
    ScenePtr update(Time delta) override;


    void enter(Scene& prev_scene) override;


    void exit(Scene& next_scene) override;


    void display() override;


    static void reset_nav_path();


    using WorldGraphIndex = u8;
    using NavBuffer = Buffer<WorldGraphIndex, 16>;


    static NavBuffer& nav_path();


private:
    void show_map(WorldGraph& map, int storm_depth);


    void update_storm_frontier(WorldGraph& map, int offset);


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
        save_options,
        help_selected,
        help_button_depressed,
        help_button_released_wait,
        logbook_selected,
        logbook_button_depressed,
        logbook_button_released_wait,
        edit_selected,
        edit_button_depressed,
        edit_button_released_wait,
        fade_out,
        fade_out_saved,
        fade_out_help,
        fade_out_logbook,
        fade_out_edit,
        print_saved_text,
        show_saved_text,
        show_node_death_icons,
        save_animate_out,
        save_exit,
        abort_move,
        plot_moves,
        save_plot,
    } state_ = State::fade_in;

    void redraw_icons();

    void draw_border();

    Buffer<Vec2<s8>, 10> movement_targets_;
    Buffer<Vec2<s8>, 10> dead_nodes_;

    NavBuffer navigation_buffer_;
    NavBuffer cached_navigation_path_;
    static NavBuffer navigation_path_;
    NavBuffer render_backup_nav_buffer_;

    void build_range_cache(detail::ShadeIntensity matrix[30][20],
                           Vector<detail::ShadeRangeComponent>& out);

    Optional<Vector<detail::ShadeRangeComponent>> discretized_range_cache_;

    Optional<Text> heading_;
    Optional<Text> warning_;
    Optional<Text> exit_label_;
    Optional<Text> map_key_;
    Optional<MediumIcon> save_icon_;
    Optional<MediumIcon> help_icon_;
    Optional<MediumIcon> logbook_icon_;
    Optional<MediumIcon> edit_icon_;

    Optional<DynamicMemory<WorldGraph>> cached_world_graph_;

    int movement_cursor_ = 0;
    int prev_world_loc_ = 0;
    int cursor_ = 0;
    int cached_cursor_ = 0;
    Time cursor_anim_timer_ = 0;
    Time timer_ = 0;
    Time tier_2_timer_ = 0;
    Time storm_scroll_timer_ = 0;

    ColorMix cmix_;
    u8 cursor_keyframe_ = 0;
    bool tier_2_visible_ = false;
    bool fast_ = false;
    bool has_radar_ = false;
    u8 save_opt_sel_ = 0;
    u8 save_opt_len_ = 0;
    bool nav_mode_ = false;
    u8 palette_cyc_counter_ = 0;
    u8 palette_cyc_simulation_ = 0;

    void render_map_key();

    static bool show_tier_2_;
};



} // namespace skyland
