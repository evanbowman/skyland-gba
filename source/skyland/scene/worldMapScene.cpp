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


#include "worldMapScene.hpp"
#include "adventureLogScene.hpp"
#include "adventureModeSettingsScene.hpp"
#include "graphics/overlay.hpp"
#include "hintScene.hpp"
#include "loadLevelScene.hpp"
#include "platform/platform.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "skyland/worldGraph.hpp"
#include "titleScreenScene.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



static const auto node_death_sequence_time = milliseconds(1500);



// TODO: create a worldGraph.cpp and move this function there.
void WorldGraph::generate(App& app)
{
    storm_depth_ = 1;

    // Basically, we start by doing a semi-random walk from left to right.
    // Therefore, we ensure that at least one path from start to finish
    // exists. We place the remaining map nodes in somewhat arbitrary locations,
    // not too close to existing nodes, but within the player's movement range.
    // We place at least eight hostile levels, such that at least three hostile
    // levels lie along the guaranteed path from start to finish. We convert one
    // graph node outside of the central path to a quest level.

    for (auto& node : nodes_) {
        node.type_ = WorldGraph::Node::Type::neutral;
        node.coord_ = {0, 0};
    }

    int walk_point = 0;
    nodes_[0].coord_ = {0,
                        s8(4 + rng::choice(height - 8, rng::critical_state))};

    while (true) {
        if (nodes_[walk_point].coord_.x >
            (width - (max_movement_distance - 3))) {
            break;
        }

        int rise = 0;
        if (rng::choice<2>(rng::critical_state)) {
            rise = rng::choice<4>(rng::critical_state);
        } else {
            rise = -rng::choice<4>(rng::critical_state);
        }

        int run = max_movement_distance;

        nodes_[walk_point + 1].coord_.x =
            clamp(nodes_[walk_point].coord_.x + run, 0, width);

        nodes_[walk_point + 1].coord_.y =
            clamp(nodes_[walk_point].coord_.y + rise, 0, height);

        ++walk_point;
    }

    nodes_[0].type_ = Node::Type::visited;

    const auto exit_node = walk_point - 1;
    nodes_[exit_node].type_ = Node::Type::exit;


    u8 placement_map[width][height];


    auto invalidate_zones = [&] {
        for (int x = 0; x < width; ++x) {
            for (int y = 0; y < height; ++y) {
                placement_map[x][y] = 0;
            }
        }

        for (auto& node : nodes_) {
            for (int x = node.coord_.x - 4; x < node.coord_.x + 5; ++x) {
                for (int y = node.coord_.y - 4; y < node.coord_.y + 5; ++y) {
                    if (x > 0 and x < width and y > 0 and y < height) {
                        // Only allow the placement of a new node within a
                        // reachable distance of an existing one.
                        placement_map[x][y] = 1;
                    }
                }
            }
        }

        for (auto& node : nodes_) {
            for (int x = node.coord_.x - 2; x < node.coord_.x + 3; ++x) {
                for (int y = node.coord_.y - 2; y < node.coord_.y + 3; ++y) {
                    if (x > 0 and x < width and y > 0 and y < height) {
                        // Do not allow placement of a new node directly
                        // adjacent to an existing one.
                        placement_map[x][y] = 0;
                    }
                }
            }
        }
    };

    invalidate_zones();


    for (int i = walk_point; i < 18; ++i) {

        int tries = 0;
        while ((++tries) < 255) {
            auto x = rng::choice(width, rng::critical_state);
            auto y = rng::choice(height, rng::critical_state);

            if (placement_map[x][y]) {
                nodes_[i].coord_.x = x;
                nodes_[i].coord_.y = y;
                invalidate_zones();
                break;
            }
        }
        if (tries == 255) {
            nodes_[i].type_ = Node::Type::null;
        }
    }

    for (int i = 18; i < 20; ++i) {

        // Place two trading hubs, somewhere in the map.

        int tries = 0;
        while ((++tries) < 255) {
            auto x = rng::choice(width, rng::critical_state);
            auto y = rng::choice(height, rng::critical_state);

            if (placement_map[x][y]) {
                nodes_[i].coord_.x = x;
                nodes_[i].coord_.y = y;
                nodes_[i].type_ = WorldGraph::Node::Type::shop;
                invalidate_zones();
                break;
            }
        }
        if (tries == 255 or i == 19 or // only place one store actually
            app.zone() < 2) {
            nodes_[i].type_ = WorldGraph::Node::Type::null;
        }
    }



    int hostile_levels = 8;
    int hostile_levels_critical_path = 0;

    for (int i = 1; i < exit_node; ++i) {
        if (hostile_levels == 0) {
            break;
        }

        if (i < exit_node and hostile_levels_critical_path < 3) {
            if (rng::choice<2>(rng::critical_state)) {
                nodes_[i].type_ = Node::Type::hostile;
                ++hostile_levels_critical_path;
                --hostile_levels;
            }
        } else if (i >= exit_node and hostile_levels_critical_path < 3) {
            i = 1;
        }
    }

    while (hostile_levels) {
        for (int i = exit_node + 1; i < 18; ++i) {
            if (hostile_levels == 0) {
                break;
            }

            if (rng::choice<2>(rng::critical_state)) {
                nodes_[i].type_ = Node::Type::hostile;
                --hostile_levels;
            }
        }
    }

    int hidden_levels = 8;
    while (hidden_levels) {
        for (int i = exit_node + 1; i < 18; ++i) {
            if (hidden_levels == 0) {
                break;
            }

            if (rng::choice<2>(rng::critical_state)) {
                if (nodes_[i].type_ == Node::Type::hostile) {
                    nodes_[i].type_ = Node::Type::hostile_hidden;
                    --hidden_levels;
                } else if (nodes_[i].type_ == Node::Type::neutral) {
                    nodes_[i].type_ = Node::Type::neutral_hidden;
                    --hidden_levels;
                }
            }
        }
    }


    int place_quest_levels = 1;
    int iters = 0;
    while (place_quest_levels) {
        if (iters++ > 1024) {
            info(Platform::instance(), "failed to place quest level...");
            break;
        }
        for (int i = exit_node + 2; i < 18; ++i) {
            if (place_quest_levels == 0) {
                break;
            }
            if (nodes_[i].type_ == Node::Type::neutral and
                rng::choice<2>(rng::critical_state)) {
                nodes_[i].type_ = Node::Type::quest;
                --place_quest_levels;
            }
        }
    }
}



static void draw_stormcloud_background(Platform& pfrm,
                                       App& app,
                                       int cloud_depth,
                                       bool clear = true)
{
    if (clear) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                pfrm.set_tile(Layer::map_1_ext, x, y, 1);

                // In case the player's island overlaps with the map_1
                // background.
                pfrm.set_tile(Layer::map_0_ext, x, y, 0);
            }
        }
    }

    for (int x = 0; x < cloud_depth; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 2);
        }
    }

    int edge = cloud_depth;
    for (int y = 0; y < 16; ++y) {
        const auto t = 3 + (y % 4);
        if (t == 3) {
            pfrm.set_tile(Layer::map_1_ext, edge - 1, y, 8);
            pfrm.set_tile(Layer::map_1_ext, edge - 1, y + 1, 10);
            if (edge > 1) {
                pfrm.set_tile(Layer::map_1_ext, edge - 2, y, 12);
                pfrm.set_tile(Layer::map_1_ext, edge - 2, y + 1, 11);
            }
        } else if (t == 6) {
            pfrm.set_tile(Layer::map_1_ext, edge - 1, y, 9);
        }
        pfrm.set_tile(Layer::map_1_ext, edge, y, t);
    }
}



void WorldMapScene::update_storm_frontier(Platform& pfrm,
                                          WorldGraph& map,
                                          int offset)
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            const auto t = pfrm.get_tile(Layer::overlay, x, y);
            if (t == 83) {
                pfrm.set_tile(Layer::overlay, x, y, 0);
            } else if (t == 86) {
                pfrm.set_tile(Layer::overlay, x, y, 89);
            } else if (t == 87) {
                pfrm.set_tile(Layer::overlay, x, y, 90);
            }
        }
    }
    for (int y = 0; y < 20; ++y) {
        const int x = 1 + (map.storm_depth_ + 1 + offset) * 2;
        auto t = pfrm.get_tile(Layer::overlay, x, y);
        if (t == 0) {
            pfrm.set_tile(Layer::overlay, x, y, 83);
        } else if (t == 89) {
            pfrm.set_tile(Layer::overlay, x, y, 86);
        } else if (t == 90) {
            pfrm.set_tile(Layer::overlay, x, y, 87);
        }
    }
}



static const int map_start_x = 5;
static const int map_start_y = 3;



void WorldMapScene::redraw_icons(Platform& pfrm)
{
    save_icon_.emplace(pfrm, 126, OverlayCoord{27, 17});
    help_icon_.emplace(pfrm, 134, OverlayCoord{21, 17});
    settings_icon_.emplace(pfrm, 142, OverlayCoord{24, 17});
    logbook_icon_.emplace(pfrm, 150, OverlayCoord{18, 17});
}



void WorldMapScene::render_map_key(Platform& pfrm, App& app)
{
    StringBuffer<32> text_ = "error";

    WorldGraph::Node* node = nullptr;
    for (auto& n : app.world_graph().nodes_) {
        if (n.coord_ == movement_targets_[movement_cursor_]) {
            node = &n;
            break;
        }
    }

    if (not node) {
        return;
    }

    switch (node->type_) {
    case WorldGraph::Node::Type::visited:
        text_ = SYSTR(wg_visited)->c_str();
        break;

    case WorldGraph::Node::Type::neutral:
        text_ = SYSTR(wg_neutral)->c_str();
        break;

    case WorldGraph::Node::Type::hostile:
        text_ = SYSTR(wg_hostile)->c_str();
        break;

    case WorldGraph::Node::Type::corrupted:
        text_ = SYSTR(wg_storm)->c_str();
        break;

    case WorldGraph::Node::Type::exit:
        text_ = "";
        break;

    case WorldGraph::Node::Type::quest:
        text_ = SYSTR(wg_quest)->c_str();
        break;

    case WorldGraph::Node::Type::shop:
        text_ = SYSTR(wg_outpost)->c_str();
        break;

    case WorldGraph::Node::Type::hostile_hidden:
    case WorldGraph::Node::Type::neutral_hidden:
        text_ = SYSTR(wg_uncharted)->c_str();
        break;

    case WorldGraph::Node::Type::quest_marker:
        text_ = SYSTR(wg_quest_marker)->c_str();

    case WorldGraph::Node::Type::null:
        break;
    }

    if (not map_key_) {
        map_key_.emplace(pfrm, OverlayCoord{11, 18});
    }
    map_key_->assign(text_.c_str());

    // NOTE: bugfix for icons disappearing when changing text that covers icons.
    redraw_icons(pfrm);
    map_key_->assign(text_.c_str());

    update_storm_frontier(pfrm, app.world_graph(), 0);
}



bool WorldMapScene::show_tier_2_ = true;



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



ScenePtr<Scene>
WorldMapScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_keyframe_ = not cursor_keyframe_;
    }


    if (pfrm.speaker().is_music_playing("unaccompanied_wind") and
        not pfrm.speaker().is_sound_playing("creaking")) {
        pfrm.speaker().play_sound("creaking", 9);
    }


    auto to_move_state = [&] {
        state_ = State::move;
        movement_cursor_ = 0;

        movement_targets_.clear();
        auto current = app.world_graph().nodes_[cursor_];
        for (int x = current.coord_.x - 4; x < current.coord_.x + 5; ++x) {
            for (int y = current.coord_.y - 4; y < current.coord_.y + 5; ++y) {
                for (auto& node : app.world_graph().nodes_) {
                    if (node.type_ not_eq WorldGraph::Node::Type::corrupted and
                        node.type_ not_eq WorldGraph::Node::Type::null and
                        node.coord_ not_eq current.coord_ and
                        node.coord_ == Vec2<s8>{s8(x), s8(y)}) {
                        movement_targets_.insert(movement_targets_.begin(),
                                                 node.coord_);
                    }
                }
            }
        }

        render_map_key(pfrm, app);
    };


    if (app.player().key_down(pfrm, Key::start)) {
        show_tier_2_ = not show_tier_2_;
    }

    switch (state_) {
    case State::deselected:
        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::selected;
            pfrm.speaker().play_sound("button_wooden", 3);
        }
        if (app.player().key_down(pfrm, Key::right) or
            app.player().key_down(pfrm, Key::left)) {
            pfrm.speaker().play_sound("click_wooden", 2);
            to_move_state();
        }
        if (app.player().key_down(pfrm, Key::down)) {
            state_ = State::save_selected;
        }
        break;

    case State::selected:
        if (app.player().key_down(pfrm, Key::action_1)) {
            to_move_state();
        }
        if (app.player().key_down(pfrm, Key::right) or
            app.player().key_down(pfrm, Key::left)) {
            pfrm.speaker().play_sound("click_wooden", 2);
            to_move_state();
        } else if (app.player().key_down(pfrm, Key::down)) {
            state_ = State::save_selected;
        }
        if (app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::deselected;
        }
        break;

    case State::save_selected:
        if (app.player().key_down(pfrm, Key::up) or
            app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::selected;
        } else if (app.player().key_down(pfrm, Key::left)) {
            state_ = State::settings_selected;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::save_button_depressed;
            save_icon_.emplace(pfrm, 130, OverlayCoord{27, 17});
            timer_ = 0;
        }
        break;


    case State::help_selected:
        if (app.player().key_down(pfrm, Key::up) or
            app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::selected;
        } else if (app.player().key_down(pfrm, Key::right)) {
            state_ = State::settings_selected;
        } else if (app.player().key_down(pfrm, Key::left)) {
            state_ = State::logbook_selected;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::help_button_depressed;
            help_icon_.emplace(pfrm, 138, OverlayCoord{21, 17});
            timer_ = 0;
        }
        break;


    case State::help_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::help_button_released_wait;
            help_icon_.emplace(pfrm, 134, OverlayCoord{21, 17});
        }
        break;


    case State::help_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_help;
        }
        break;


    case State::logbook_selected:
        if (app.player().key_down(pfrm, Key::up) or
            app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::selected;
        } else if (app.player().key_down(pfrm, Key::right)) {
            state_ = State::help_selected;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::logbook_button_depressed;
            logbook_icon_.emplace(pfrm, 154, OverlayCoord{18, 17});
            timer_ = 0;
        }
        break;


    case State::logbook_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::logbook_button_released_wait;
            logbook_icon_.emplace(pfrm, 150, OverlayCoord{18, 17});
        }
        break;


    case State::logbook_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_logbook;
        }
        break;


    case State::settings_selected:
        if (app.player().key_down(pfrm, Key::up) or
            app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::selected;
        } else if (app.player().key_down(pfrm, Key::right)) {
            state_ = State::save_selected;
        } else if (app.player().key_down(pfrm, Key::left)) {
            state_ = State::help_selected;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::settings_button_depressed;
            settings_icon_.emplace(pfrm, 146, OverlayCoord{24, 17});
            timer_ = 0;
        }
        break;


    case State::settings_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::settings_button_released_wait;
            settings_icon_.emplace(pfrm, 142, OverlayCoord{24, 17});
        }
        break;


    case State::settings_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_settings;
        }
        break;


    case State::save_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::save_button_released_wait;
            save_icon_.emplace(pfrm, 126, OverlayCoord{27, 17});
        }
        break;


    case State::save_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_saved;
        }
        break;


    case State::move: {
        if (not tier_2_visible_) {
            tier_2_timer_ += delta;
            if (tier_2_timer_ > milliseconds(1500)) {
                tier_2_timer_ = 0;
                tier_2_visible_ = true;
            }
        }
        if (app.player().key_down(pfrm, Key::action_1)) {
            if (app.world_graph().nodes_[cursor_].type_ not_eq
                WorldGraph::Node::Type::shop) {
                app.world_graph().nodes_[cursor_].type_ =
                    WorldGraph::Node::Type::visited;
            }

            for (int i = 0; i < 19; ++i) {
                if (app.world_graph().nodes_[i].coord_ ==
                    movement_targets_[movement_cursor_]) {
                    cursor_ = i;
                }
            }
            // In case anything goes wrong: create an emergency backup!
            app.create_backup(pfrm,
                              App::BackupContext{
                                  .next_world_location_ = (s8)cursor_,
                              });

            app.current_world_location() = cursor_;
            show_map(pfrm, app.world_graph(), app.world_graph().storm_depth_);
            cmix_ = {};
            map_key_.reset();
            redraw_icons(pfrm);
            update_storm_frontier(pfrm, app.world_graph(), 0);
            ++app.world_graph().storm_depth_;

            if (app.world_graph().nodes_[cursor_].type_ ==
                WorldGraph::Node::Type::visited) {
                draw_stormcloud_background(
                    pfrm, app, app.world_graph().storm_depth_, false);
                state_ = State::storm_advance;
            } else {
                pfrm.speaker().play_sound("button_wooden", 3);
                state_ = State::wait;
                cmix_ = {ColorConstant::stil_de_grain, 200};
            }

        } else if (app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::selected;
            map_key_.reset();
            redraw_icons(pfrm);
            update_storm_frontier(pfrm, app.world_graph(), 0);
            show_map(pfrm, app.world_graph(), app.world_graph().storm_depth_);
            cmix_ = {};
        }
        // auto current = movement_targets_[movement_cursor_];
        if (app.player().key_down(pfrm, Key::left)) {
            Buffer<int, 10> left;

            tier_2_visible_ = false;
            tier_2_timer_ = 0;

            auto search = [&](int width) {
                left.clear();
                for (u32 i = 0; i < movement_targets_.size(); ++i) {
                    auto& t = movement_targets_[i];
                    if (abs(t.y - movement_targets_[movement_cursor_].y) <
                            width and
                        t.x < movement_targets_[movement_cursor_].x) {
                        left.push_back(i);
                    }
                }
                std::sort(left.begin(), left.end(), [&](auto& lhs, auto& rhs) {
                    return movement_targets_[lhs].x > movement_targets_[rhs].x;
                });
            };

            search(2);

            if (not left.empty()) {
                pfrm.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = left[0];
            } else {
                search(3);

                if (not left.empty()) {
                    pfrm.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = left[0];
                } else {
                    search(5);
                    if (not left.empty()) {
                        pfrm.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = left[0];
                    } else {
                        search(7);
                        if (not left.empty()) {
                            pfrm.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = left[0];
                        } else {
                            search(8);
                            if (not left.empty()) {
                                pfrm.speaker().play_sound("click_wooden", 2);
                                movement_cursor_ = left[0];
                            }
                        }
                    }
                }
            }
            render_map_key(pfrm, app);
        } else if (app.player().key_down(pfrm, Key::right)) {
            Buffer<int, 10> right;

            tier_2_visible_ = false;
            tier_2_timer_ = 0;

            auto search = [&](int width) {
                right.clear();
                for (u32 i = 0; i < movement_targets_.size(); ++i) {
                    auto& t = movement_targets_[i];
                    if (abs(t.y - movement_targets_[movement_cursor_].y) <
                            width and
                        t.x > movement_targets_[movement_cursor_].x) {
                        right.push_back(i);
                    }
                }
                std::sort(
                    right.begin(), right.end(), [&](auto& lhs, auto& rhs) {
                        return movement_targets_[lhs].x <
                               movement_targets_[rhs].x;
                    });
            };

            search(2);

            if (not right.empty()) {
                pfrm.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = right[0];
            } else {
                search(3);

                if (not right.empty()) {
                    pfrm.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = right[0];
                } else {
                    search(5);

                    if (not right.empty()) {
                        pfrm.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = right[0];
                    } else {
                        search(7);
                        if (not right.empty()) {
                            pfrm.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = right[0];
                        } else {
                            search(8);
                            if (not right.empty()) {
                                pfrm.speaker().play_sound("click_wooden", 2);
                                movement_cursor_ = right[0];
                            }
                        }
                    }
                }
            }
            render_map_key(pfrm, app);
        } else if (app.player().key_down(pfrm, Key::up)) {
            Buffer<int, 10> above;

            tier_2_visible_ = false;
            tier_2_timer_ = 0;

            auto search = [&](int width) {
                above.clear();
                for (u32 i = 0; i < movement_targets_.size(); ++i) {
                    auto& t = movement_targets_[i];
                    if (abs(t.x - movement_targets_[movement_cursor_].x) <
                            width and
                        t.y < movement_targets_[movement_cursor_].y) {
                        above.push_back(i);
                    }
                }
                std::sort(
                    above.begin(), above.end(), [&](auto& lhs, auto& rhs) {
                        return movement_targets_[lhs].y >
                               movement_targets_[rhs].y;
                    });
            };

            search(2);

            if (not above.empty()) {
                pfrm.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = above[0];
            } else {
                search(3);
                if (not above.empty()) {
                    pfrm.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = above[0];
                } else {
                    search(5);
                    if (not above.empty()) {
                        pfrm.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = above[0];
                    } else {
                        search(7);
                        if (not above.empty()) {
                            pfrm.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = above[0];
                        }
                    }
                }
            }
            render_map_key(pfrm, app);
        } else if (app.player().key_down(pfrm, Key::down)) {
            Buffer<int, 10> beneath;

            tier_2_visible_ = false;
            tier_2_timer_ = 0;

            auto search = [&](int width) {
                beneath.clear();
                for (u32 i = 0; i < movement_targets_.size(); ++i) {
                    auto& t = movement_targets_[i];
                    if (abs(t.x - movement_targets_[movement_cursor_].x) <
                            width and
                        t.y > movement_targets_[movement_cursor_].y) {
                        beneath.push_back(i);
                    }
                }
                std::sort(
                    beneath.begin(), beneath.end(), [&](auto& lhs, auto& rhs) {
                        return movement_targets_[lhs].y <
                               movement_targets_[rhs].y;
                    });
            };

            search(2);

            if (not beneath.empty()) {
                pfrm.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = beneath[0];
            } else {

                search(3);

                if (not beneath.empty()) {
                    pfrm.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = beneath[0];
                } else {
                    search(5);
                    if (not beneath.empty()) {
                        pfrm.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = beneath[0];
                    } else {
                        search(7);
                        if (not beneath.empty()) {
                            pfrm.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = beneath[0];
                        }
                    }
                }
            }
            render_map_key(pfrm, app);
        }
        break;
    }

    case State::wait:
        if (cmix_.amount_ > 0) {
            timer_ += delta;
            if (timer_ > 12000) {
                timer_ -= 12000;
                cmix_ = {cmix_.color_, u8(cmix_.amount_ - 5)};
            }
        } else {
            timer_ = 0;
            cmix_ = {};
            state_ = State::fade_out;
        }
        break;


    case State::fade_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(700);
        if (timer_ > fade_duration) {
            pfrm.speaker().clear_sounds();
            return scene_pool::alloc<LoadLevelScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::fade_in: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::storm_scroll_in;
            // pfrm.screen().fade(1.f, custom_color(0x6057b1), {}, false, false);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }


    case State::show_node_death_icons:
        if (dead_nodes_.empty()) {
            state_ = State::deselected;
        } else {
            timer_ += delta;
            if (app.player().key_down(pfrm, Key::action_1) or
                app.player().key_down(pfrm, Key::action_2)) {
                fast_ = true;
            }
            if (fast_) {
                timer_ += delta;
            }
            if (timer_ > node_death_sequence_time) {
                timer_ = 0;
                state_ = State::deselected;
                auto current = app.world_graph().nodes_[cursor_];
                if (current.type_ == WorldGraph::Node::Type::corrupted) {
                    state_ = State::fade_out;
                }
            }
        }
        break;


    case State::storm_advance: {
        storm_scroll_timer_ += delta;
        constexpr auto fade_duration = milliseconds(1000);
        if (storm_scroll_timer_ > fade_duration) {
            storm_scroll_timer_ = 0;
            state_ = State::show_node_death_icons;
            fast_ = false;
            timer_ = 0;
            show_map(pfrm, app.world_graph(), app.world_graph().storm_depth_);
            update_storm_frontier(pfrm, app.world_graph(), 0);
        } else {
            const auto amount =
                1.f - smoothstep(0.f, fade_duration, storm_scroll_timer_);
            pfrm.set_scroll(Layer::map_1_ext, amount * 16, 0);
        }
        break;
    }


    case State::storm_scroll_in: {
        storm_scroll_timer_ += delta;
        constexpr auto fade_duration = milliseconds(1000);
        if (storm_scroll_timer_ > fade_duration) {
            storm_scroll_timer_ = 0;
            state_ = State::show_node_death_icons;
            timer_ = 0;
            show_map(pfrm, app.world_graph(), app.world_graph().storm_depth_);
            update_storm_frontier(pfrm, app.world_graph(), 0);
        } else {
            const auto amount =
                1.f - smoothstep(0.f, fade_duration, storm_scroll_timer_);
            pfrm.set_scroll(Layer::map_1_ext, amount * 16, 0);
        }
        break;
    }


    case State::fade_out_saved: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::print_saved_text;
            save::store(pfrm, app, app.persistent_data());
            pfrm.fill_overlay(0);
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::fade_out_logbook: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            pfrm.fill_overlay(0);
            auto next = scene_pool::alloc<AdventureLogScene>();
            next->set_next_scene(
                [] { return scene_pool::alloc<WorldMapScene>(); });
            return next;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::fade_out_help: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            pfrm.fill_overlay(0);
            return scene_pool::alloc<HintScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::fade_out_settings: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            pfrm.fill_overlay(0);
            return scene_pool::alloc<AdventureModeSettingsScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::print_saved_text: {
        pfrm.load_overlay_texture("overlay");
        const auto screen_tiles = calc_screen_tiles(pfrm);
        for (int i = 0; i < screen_tiles.x; ++i) {
            pfrm.set_tile(Layer::overlay, i, 0, 112);
            pfrm.set_tile(Layer::overlay, i, 1, 112);
            pfrm.set_tile(Layer::overlay, i, 2, 112);
            pfrm.set_tile(Layer::overlay, i, 3, 112);
            pfrm.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
            pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 5, 112);
        }
        state_ = State::show_saved_text;
        pfrm.load_tile1_texture("savegame_flattened");
        __draw_image(pfrm, 1, 0, 4, 30, 13, Layer::map_1);
        heading_.emplace(pfrm, SYSTR(wg_saved)->c_str(), OverlayCoord{1, 1});
        pfrm.speaker().play_sound("button_wooden", 3);
        pfrm.speaker().stop_music();
        break;
    }


    case State::show_saved_text: {
        if (timer_ == 0) {
            pfrm.screen().schedule_fade(0.f);
            pfrm.load_tile1_texture("savegame_flattened");
        }
        const auto prev_timer = timer_;
        timer_ += delta;

        if (app.player().key_down(pfrm, Key::action_1) or
            app.player().key_down(pfrm, Key::action_2)) {
            timer_ = milliseconds(7450);
        }

        if (timer_ > milliseconds(7500)) {
            heading_.reset();
            for (int i = 0; i < 30; ++i) {
                pfrm.set_tile(Layer::overlay, i, 1, 112);
            }
        }

        if (timer_ > milliseconds(7600)) {
            const auto screen_tiles = calc_screen_tiles(pfrm);
            for (int i = 0; i < 30; ++i) {
                pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
            }
        }

        if (timer_ > milliseconds(7700)) {
            const auto screen_tiles = calc_screen_tiles(pfrm);
            for (int i = 0; i < 30; ++i) {
                pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            }
        }

        const auto t1_thresh = milliseconds(750);
        if (prev_timer < t1_thresh and timer_ > t1_thresh) {
            Text t(pfrm,
                   SYSTR(wg_saved_description)->c_str(),
                   OverlayCoord{1, 16});
            t.__detach();
        }

        const auto t2_thresh = milliseconds(1450);
        if (prev_timer < t2_thresh and timer_ > t2_thresh) {
            Text t(pfrm,
                   SYSTR(wg_saved_come_back_soon)->c_str(),
                   OverlayCoord{1, 18});
            t.__detach();
        }


        if (timer_ > milliseconds(7800)) {
            timer_ = 0;
            state_ = State::save_animate_out;
        }
        break;
    }

    case State::save_animate_out: {
        timer_ += delta;

        const auto transition_time = milliseconds(250);

        constexpr int pixels_per_tile = 8;
        const auto total_pixels = 6 * pixels_per_tile;

        const Float percentage = smoothstep(0.f, transition_time, timer_);
        const int fractional_pixels = percentage * total_pixels;

        for (int y = 4; y < 4 + 6; ++y) {
            if ((y - 4) * 8 < fractional_pixels) {
                for (int x = 0; x < 30; ++x) {
                    pfrm.set_tile(Layer::overlay, x, y, 112);
                }
                for (int x = 0; x < 30; ++x) {
                    pfrm.set_tile(Layer::overlay, x, 14 - (y - 4), 112);
                }
            } else if (((y + 1) - 4) * 8 > fractional_pixels and
                       fractional_pixels % 8) {
                for (int x = 0; x < 30; ++x) {
                    pfrm.set_tile(
                        Layer::overlay, x, y, 119 - (fractional_pixels % 8));
                }
                for (int x = 0; x < 30; ++x) {
                    pfrm.set_tile(Layer::overlay,
                                  x,
                                  14 - (y - 4),
                                  128 - (fractional_pixels % 8));
                }
                break;
            }
        }

        if (timer_ >= transition_time) {
            timer_ = 0;
            state_ = State::save_exit;
        }

        break;
    }

    case State::save_exit:
        timer_ += delta;
        if (timer_ > milliseconds(350)) {
            return scene_pool::alloc<TitleScreenScene>();
        }
        break;
    }



    return null_scene();
}



void WorldMapScene::display(Platform& pfrm, App& app)
{
    if (state_ == State::show_saved_text or state_ == State::save_animate_out or
        state_ == State::save_exit) {
        return;
    }

    Sprite cursor;
    cursor.set_priority(0);

    Vec2<s8> cursor_loc = app.world_graph().nodes_[cursor_].coord_;
    cursor_loc.x += 5;
    cursor_loc.y += 3;


    cursor.set_size(Sprite::Size::w16_h16);
    cursor.set_tidx_16x16(28, 0);
    cursor.set_position({Fixnum((int)cursor_loc.x * 8) - 8.0_fixed,
                         Fixnum((int)cursor_loc.y * 8) - 12.0_fixed});
    cursor.set_mix(cmix_);
    pfrm.screen().draw(cursor);


    cursor.set_mix({});

    cursor.set_size(Sprite::Size::w16_h16);


    if (state_ == State::selected) {
        auto current = app.world_graph().nodes_[cursor_].coord_;
        cursor.set_texture_index((15 * 2) + cursor_keyframe_);
        cursor.set_position({Fixnum((current.x + map_start_x) * Float(8) - 4),
                             Fixnum((current.y + map_start_y) * Float(8) - 4)});
        pfrm.screen().draw(cursor);
    } else if (state_ == State::move) {

        auto target = movement_targets_[movement_cursor_];
        cursor.set_texture_index((15 * 2) + cursor_keyframe_);
        cursor.set_position({Fixnum((target.x + map_start_x) * Float(8) - 4),
                             Fixnum((target.y + map_start_y) * Float(8) - 4)});
        pfrm.screen().draw(cursor);

        for (auto& t : app.world_graph().nodes_) {
            if (t.type_ not_eq WorldGraph::Node::Type::null and
                t.type_ not_eq WorldGraph::Node::Type::corrupted and
                (map_start_x + t.coord_.x) * 8 <
                    (app.world_graph().storm_depth_ + 2) * 16) {
                cursor.set_texture_index(111);
                cursor.set_position(
                    {Fixnum((t.coord_.x + map_start_x) * Float(8) - 3),
                     Fixnum((t.coord_.y + map_start_y) * Float(8) - (12))});
                cursor.set_mix({});
                cursor.set_priority(0);
                cursor.set_size(Sprite::Size::w16_h32);
                cursor.set_alpha(Sprite::Alpha::opaque);
                pfrm.screen().draw(cursor);
            }
        }

        auto current = app.world_graph().nodes_[cursor_];
        auto x = (current.coord_.x + map_start_x) - 4;
        auto y = (current.coord_.y + map_start_y) - 4;
        cursor.set_texture_index(76);
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_alpha(Sprite::Alpha::translucent);
        cursor.set_priority(2);

        auto draw_range = [&cursor, &pfrm](int x, int y) {
            for (int i = 0; i < 4; ++i) {
                cursor.set_position(
                    {Fixnum(Float(x) * 8 + i * 16), Fixnum(Float(y) * 8 + 32)});
                pfrm.screen().draw(cursor);
                cursor.set_position(
                    {Fixnum(Float(x) * 8 + i * 16), Fixnum(Float(y) * 8)});
                pfrm.screen().draw(cursor);
            }

            cursor.set_texture_index(74);
            for (int i = 0; i < 4; ++i) {
                cursor.set_position(
                    {Fixnum(Float(x) * 8 + i * 16), Fixnum(Float(y) * 8 + 64)});
                pfrm.screen().draw(cursor);
            }

            cursor.set_texture_index(75);
            cursor.set_position(
                {Fixnum(Float(x) * 8 + 64), Fixnum(Float(y) * 8)});
            pfrm.screen().draw(cursor);
            cursor.set_position(
                {Fixnum(Float(x) * 8 + 64), Fixnum(Float(y) * 8 + 32)});
            pfrm.screen().draw(cursor);
            cursor.set_position(
                {Fixnum(Float(x) * 8 + 64), Fixnum(Float(y) * 8 + 32 + 8)});
            pfrm.screen().draw(cursor);
        };

        draw_range(x, y);

        cursor.set_mix({ColorConstant::rich_black, 64});
        auto o = movement_targets_[movement_cursor_];
        cursor.set_texture_index(76);
        draw_range(o.x + map_start_x - 4, o.y + map_start_y - 4);

        Buffer<Vec2<s8>, 10> tier_2_reachable;
        for (int x = o.x - 4; x < o.x + 5; ++x) {
            for (int y = o.y - 4; y < o.y + 5; ++y) {
                for (auto& node : app.world_graph().nodes_) {
                    if (node.type_ not_eq WorldGraph::Node::Type::corrupted and
                        node.type_ not_eq WorldGraph::Node::Type::null and
                        node.coord_ not_eq o and
                        node.coord_ == Vec2<s8>{s8(x), s8(y)}) {
                        tier_2_reachable.push_back(node.coord_);
                    }
                }
            }
        }


        if (show_tier_2_ and tier_2_visible_) {
            for (auto& o : tier_2_reachable) {
                cursor.set_mix({ColorConstant::rich_black, 180});
                cursor.set_texture_index(76);
                draw_range(o.x + map_start_x - 4, o.y + map_start_y - 4);
            }
        }

    } else if (state_ == State::save_selected or
               state_ == State::save_button_depressed or
               state_ == State::save_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(208), Fixnum::from_integer(128)});
        pfrm.screen().draw(cursor);
    } else if (state_ == State::logbook_selected or
               state_ == State::logbook_button_depressed or
               state_ == State::logbook_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(136), Fixnum::from_integer(128)});
        pfrm.screen().draw(cursor);
    } else if (state_ == State::help_selected or
               state_ == State::help_button_depressed or
               state_ == State::help_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(160), Fixnum::from_integer(128)});
        pfrm.screen().draw(cursor);
    } else if (state_ == State::settings_selected or
               state_ == State::settings_button_depressed or
               state_ == State::settings_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(184), Fixnum::from_integer(128)});
        pfrm.screen().draw(cursor);
    } else if (state_ == State::show_node_death_icons) {
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_texture_index(77);
        const int offset =
            interpolate(9, 0, Float(timer_) / node_death_sequence_time);
        for (auto& node : dead_nodes_) {
            cursor.set_position({});
            cursor.set_position(
                {Fixnum::from_integer((node.x + map_start_x) * Float(8) - 1),
                 Fixnum::from_integer((node.y + map_start_y) * Float(8) -
                                      (4 + offset))});
            pfrm.screen().draw(cursor);
        }
    }
}



void WorldMapScene::enter(Platform& pfrm, App& app, Scene& prev_scene)
{
    pfrm.screen().set_shader(passthrough_shader);

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    app.swap_player<PlayerP1>();

    app.effects().clear();
    app.player_island().projectiles().clear();

    cursor_ = app.current_world_location();

    auto view = pfrm.screen().get_view();
    view.set_center({});
    pfrm.screen().set_view(view);

    app.time_stream().clear();

    auto& current = app.world_graph().nodes_[cursor_];
    if (current.type_ not_eq WorldGraph::Node::Type::shop) {
        current.type_ = WorldGraph::Node::Type::visited;
    }

    pfrm.load_overlay_texture("overlay_world_map");
    pfrm.load_tile1_texture("tilesheet_world_map_backdrop");

    pfrm.set_scroll(Layer::map_1_ext, 16, 0);
    draw_stormcloud_background(pfrm, app, app.world_graph().storm_depth_);

    pfrm.enable_glyph_mode(true);

    auto st = calc_screen_tiles(pfrm);

    for (int x = 1; x < st.x - 1; ++x) {
        pfrm.set_tile(Layer::overlay, x, 0, 89);
        pfrm.set_tile(Layer::overlay, x, st.y - 1, 90);
    }

    for (int y = 1; y < st.y - 1; ++y) {
        pfrm.set_tile(Layer::overlay, 0, y, 91);
        pfrm.set_tile(Layer::overlay, st.x - 1, y, 92);
    }

    pfrm.set_tile(Layer::overlay, 0, 0, 93);
    pfrm.set_tile(Layer::overlay, st.x - 1, 0, 95);
    pfrm.set_tile(Layer::overlay, 0, st.y - 1, 96);
    pfrm.set_tile(Layer::overlay, st.x - 1, st.y - 1, 94);

    heading_.emplace(pfrm, OverlayCoord{1, 1});

    heading_->assign(format(SYSTR(wg_title)->c_str(), app.zone()).c_str());

    warning_.emplace(pfrm, OverlayCoord{1, 18});
    warning_->assign(SYSTR(wg_storm_label)->c_str());

    show_map(pfrm, app.world_graph(), app.world_graph().storm_depth_ - 1);

    save_icon_.emplace(pfrm, 126, OverlayCoord{27, 17});
    help_icon_.emplace(pfrm, 134, OverlayCoord{21, 17});
    settings_icon_.emplace(pfrm, 142, OverlayCoord{24, 17});
    logbook_icon_.emplace(pfrm, 150, OverlayCoord{18, 17});

    for (auto& node : app.world_graph().nodes_) {
        if (node.type_ == WorldGraph::Node::Type::exit) {
            exit_label_.emplace(
                pfrm,
                SYSTR(wg_exit)->c_str(),
                OverlayCoord{u8(node.coord_.x + map_start_x),
                             u8(node.coord_.y + map_start_y - 1)});
        }
    }


    update_storm_frontier(pfrm, app.world_graph(), -1);
}



void WorldMapScene::show_map(Platform& pfrm, WorldGraph& map, int storm_depth)
{
    dead_nodes_.clear();

    for (auto& node : map.nodes_) {
        if (node.type_ == WorldGraph::Node::Type::null) {
            continue;
        }
        if ((map_start_x + node.coord_.x) * 8 < (storm_depth + 1) * 16) {

            if (node.type_ == WorldGraph::Node::Type::exit) {
                exit_label_.reset();
            }
            if (node.type_ not_eq WorldGraph::Node::Type::corrupted) {
                dead_nodes_.push_back(node.coord_);
            }
            node.type_ = WorldGraph::Node::Type::corrupted;

            pfrm.set_tile(Layer::overlay,
                          map_start_x + node.coord_.x,
                          map_start_y + node.coord_.y,
                          98);

        } else {

            pfrm.set_tile(Layer::overlay,
                          map_start_x + node.coord_.x,
                          map_start_y + node.coord_.y,
                          114 + (int)node.type_);
        }
    }
}



void WorldMapScene::exit(Platform& pfrm, App& app, Scene& next_scene)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    pfrm.fill_overlay(0);

    save_icon_.reset();
    help_icon_.reset();
    settings_icon_.reset();
    logbook_icon_.reset();
    exit_label_.reset();
    heading_.reset();
    warning_.reset();

    pfrm.load_overlay_texture("overlay");

    show_island_exterior(pfrm, app, app.opponent_island());
}



} // namespace skyland
