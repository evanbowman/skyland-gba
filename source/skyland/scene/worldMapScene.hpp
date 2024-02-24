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
#include "skyland/worldGraph.hpp"



namespace skyland
{



class WorldMap;



class WorldMapScene : public Scene
{
public:
    ScenePtr<Scene> update(Time delta) override;


    void enter(Scene& prev_scene) override;


    void exit(Scene& next_scene) override;


    void display() override;


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
    } state_ = State::fade_in;

    bool move_arrow_sel_[3] = {false, true, false};

    void redraw_icons();

    int prev_world_loc_ = 0;
    int cursor_ = 0;
    u8 cursor_keyframe_ = 0;
    Time cursor_anim_timer_ = 0;
    Time timer_ = 0;
    ColorMix cmix_;

    int movement_cursor_ = 0;
    Buffer<Vec2<s8>, 10> movement_targets_;
    Buffer<Vec2<s8>, 10> dead_nodes_;

    Time storm_scroll_timer_ = 0;

    void render_map_key();

    static bool show_tier_2_;
    bool tier_2_visible_ = false;
    bool fast_ = false;
    bool has_radar_ = false;

    Time tier_2_timer_ = 0;

    Optional<Text> heading_;
    Optional<Text> warning_;
    Optional<Text> exit_label_;
    Optional<Text> map_key_;
    // Optional<Text> key_[3];
    Optional<MediumIcon> save_icon_;
    Optional<MediumIcon> help_icon_;
    Optional<MediumIcon> logbook_icon_;
    Optional<MediumIcon> edit_icon_;
};



} // namespace skyland
