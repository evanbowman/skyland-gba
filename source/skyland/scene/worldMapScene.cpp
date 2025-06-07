////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "worldMapScene.hpp"
#include "adventureLogScene.hpp"
#include "fadeInScene.hpp"
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



// Number of hops to get from Node n1 to Node n2
WorldGraph::PathBuffer WorldGraph::path(Vec2<s8> n1, Vec2<s8> n2)
{
    PathBuffer result;

    struct Vertex
    {
        Vertex* prev_ = nullptr;
        u16 dist_ = std::numeric_limits<u16>::max();
        Vec2<s8> coord_;
    };

    BulkAllocator<2> vertex_memory;

    Buffer<Vertex*, node_count * 4> priority_q;

    if (n1 == n2) {
        return result;
    }

    struct Matrix
    {
        Vertex* mat_[WorldGraph::width][WorldGraph::height] = {{}};
    };

    auto mat = allocate_dynamic<Matrix>("wg-path-mat");

    Vertex* start_v = nullptr;

    for (auto& n : nodes_) {
        if (n.type_ == WorldGraph::Node::Type::null) {
            continue;
        }
        if (auto vert = vertex_memory.alloc<Vertex>()) {
            vert->coord_ = n.coord_;
            if (priority_q.push_back(vert.release())) {
                auto v = priority_q.back();
                if (v->coord_ == n1) {
                    start_v = v;
                    start_v->dist_ = 0;
                }
                mat->mat_[v->coord_.x][v->coord_.y] = v;
            }
        } else {
            error("failed to push vertex!");
            return result;
        }
    }

    if (not start_v) {
        error("missing startv");
        return result;
    }

    auto neighbors = [&](Vertex* data) {
        Buffer<Vertex*, 10> result;

        auto o = data->coord_;

        for (int x = clamp(o.x - 4, 0, (int)o.x); x < o.x + 5; ++x) {
            for (int y = clamp(o.y - 4, 0, (int)o.y); y < o.y + 5; ++y) {
                if (x < WorldGraph::width and y < WorldGraph::height) {
                    auto n = mat->mat_[x][y];
                    if (n) {
                        result.push_back(n);
                    }
                }
            }
        }
        return result;
    };

    auto sort_q = [&] {
        std::sort(priority_q.begin(),
                  priority_q.end(),
                  [](auto& lhs, auto& rhs) { return lhs->dist_ > rhs->dist_; });
    };

    sort_q();

    while (true) {
        if (not priority_q.empty()) {
            auto min = priority_q.back();
            if (min->dist_ == std::numeric_limits<u16>::max()) {
                return result;
            }
            if (min->coord_ == n2) {
                PathBuffer result;
                auto current_v = priority_q.back();
                while (current_v) {
                    result.push_back(current_v->coord_);
                    current_v = current_v->prev_;
                }
                return result;
            }
            priority_q.pop_back();

            for (auto& neighbor : neighbors(min)) {
                auto alt = min->dist_ +
                           manhattan_length(min->coord_, neighbor->coord_);
                if (alt < neighbor->dist_) {
                    neighbor->dist_ = alt;
                    neighbor->prev_ = min;
                }
            }
            sort_q();
        } else {
            return result;
        }
    }

    return result;
}



// TODO: create a worldGraph.cpp and move this function there.
void WorldGraph::generate()
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
            APP.zone() < 2) {
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
            info("failed to place quest level...");
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



static void draw_stormcloud_background(int cloud_depth, bool clear = true)
{
    if (clear) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                PLATFORM.set_tile(Layer::map_1_ext, x, y, 1);

                // In case the player's island overlaps with the map_1
                // background.
                PLATFORM.set_tile(Layer::map_0_ext, x, y, 0);
            }
        }
    }

    for (int x = 0; x < cloud_depth; ++x) {
        for (int y = 0; y < 16; ++y) {
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 2);
        }
    }

    int edge = cloud_depth;
    for (int y = 0; y < 16; ++y) {
        const auto t = 3 + (y % 4);
        if (t == 3) {
            PLATFORM.set_tile(Layer::map_1_ext, edge - 1, y, 8);
            PLATFORM.set_tile(Layer::map_1_ext, edge - 1, y + 1, 10);
            if (edge > 1) {
                PLATFORM.set_tile(Layer::map_1_ext, edge - 2, y, 12);
                PLATFORM.set_tile(Layer::map_1_ext, edge - 2, y + 1, 11);
            }
        } else if (t == 6) {
            PLATFORM.set_tile(Layer::map_1_ext, edge - 1, y, 9);
        }
        PLATFORM.set_tile(Layer::map_1_ext, edge, y, t);
    }
}



void WorldMapScene::update_storm_frontier(WorldGraph& map, int offset)
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            const auto t = PLATFORM.get_tile(Layer::overlay, x, y);
            if (t == 83) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            } else if (t == 86) {
                PLATFORM.set_tile(Layer::overlay, x, y, 89);
            } else if (t == 87) {
                PLATFORM.set_tile(Layer::overlay, x, y, 90);
            }
        }
    }
    for (int y = 0; y < 20; ++y) {
        const int x = 1 + (map.storm_depth_ + 1 + offset) * 2;
        auto t = PLATFORM.get_tile(Layer::overlay, x, y);
        if (t == 0) {
            PLATFORM.set_tile(Layer::overlay, x, y, 83);
        } else if (t == 89) {
            PLATFORM.set_tile(Layer::overlay, x, y, 86);
        } else if (t == 90) {
            PLATFORM.set_tile(Layer::overlay, x, y, 87);
        }
    }
}



static const int map_start_x = 5;
static const int map_start_y = 3;



bool is_x_behind_storm_frontier(int x, int storm_offset)
{
    return (map_start_x + x) * 8 <
           (APP.world_graph().storm_depth_ + 1 + storm_offset) * 16;
}



void WorldMapScene::redraw_icons()
{
    save_icon_.emplace(126, OverlayCoord{27, 17});
    help_icon_.emplace(134, OverlayCoord{24, 17});
    logbook_icon_.emplace(150, OverlayCoord{21, 17});
    edit_icon_.emplace(158, OverlayCoord{18, 17});
}



void WorldMapScene::render_map_key()
{
    StringBuffer<32> text_ = "error";

    WorldGraph::Node* node = nullptr;
    for (auto& n : APP.world_graph().nodes_) {
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
        map_key_.emplace(OverlayCoord{11, 18});
    }
    map_key_->assign(text_.c_str());

    // NOTE: bugfix for icons disappearing when changing text that covers icons.
    redraw_icons();
    map_key_->assign(text_.c_str());

    update_storm_frontier(APP.world_graph(), 0);
}



bool WorldMapScene::show_tier_2_ = true;



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



bool WorldMapScene::can_abort_move() const
{
    return prev_world_loc_ not_eq cursor_;
}



void update_weather_onload();



void show_saved_indicator()
{
    PLATFORM.set_tile(Layer::overlay, 27, 1, 168);
    PLATFORM.set_tile(Layer::overlay, 28, 1, 169);
}



void plot_navigation_path(const WorldMapScene::NavBuffer& nav)
{
    struct Tile16x16p
    {
        u8 data_[16][16];
    };

    static const int fb_tile_width = 12;
    static const int fb_tile_height = 7;
    static const int fb_px_width = fb_tile_width * 16;
    static const int fb_px_height = fb_tile_height * 16;

    Vector<Tile16x16p> framebuffer;
    for (int i = 0; i < fb_tile_width * fb_tile_height; ++i) {
        framebuffer.emplace_back();
    }

    auto plot_pixel = [&](u8 x, u8 y, u8 p) {
        if (x > fb_px_width or y > fb_px_height) {
            return;
        }
        u8 tx = x / 16;
        u8 ty = y / 16;
        x %= 16;
        y %= 16;
        framebuffer[ty * fb_tile_width + tx].data_[x][y] = p;
    };

    auto plot_line = [&](int x0, int y0, int x1, int y1, u8 color) {
        int dx = abs(x1 - x0);
        int sx = x0 < x1 ? 1 : -1;
        int dy = -abs(y1 - y0);
        int sy = y0 < y1 ? 1 : -1;
        int error = dx + dy;

        while (true) {
            plot_pixel(x0, y0, color + 1);
            color++;
            color %= 8;
            if (x0 == x1 && y0 == y1)
                break;
            int e2 = 2 * error;
            if (e2 >= dy) {
                if (x0 == x1)
                    break;
                error = error + dy;
                x0 = x0 + sx;
            }
            if (e2 <= dx) {
                if (y0 == y1)
                    break;
                error = error + dx;
                y0 = y0 + sy;
            }
        }
    };

    auto prev = nav.begin();
    auto it = nav.begin();
    ++it;
    for (; it not_eq nav.end();) {
        auto c1 = APP.world_graph().nodes_[*prev].coord_;
        auto c2 = APP.world_graph().nodes_[*it].coord_;
        // NOTE: Everything is shifted leftwards by eight pixels, and then 4
        // pixels are added to center the line under the map markers. The
        // framebuffer is also shifted upwards by one 16x16 tile, then we add 12
        // to shift the y coordinate down by one 8x8px tile + the centering
        // margin. It's all a bit complicated... I'm trying to prevent the lines
        // from being cropped off the edge of the framebuffer, but there's not
        // enough graphics memory to make the framebuffer the same size as the
        // screen, so there's going to be some weird offset calculations no
        // matter what I do.
        int cx1 = c1.x * 8 + 12;
        int cx2 = c2.x * 8 + 12;
        int cy1 = c1.y * 8 + 12;
        int cy2 = c2.y * 8 + 12;
        plot_line(cx1, cy1, cx2, cy2, 0);
        plot_line(cx1 - 1, cy1, cx2 - 1, cy2, 0);
        plot_line(cx1 + 1, cy1, cx2 + 1, cy2, 0);
        plot_line(cx1, cy1 - 1, cx2, cy2 - 1, 0);
        plot_line(cx1, cy1 + 1, cx2, cy2 + 1, 0);
        ++it;
        ++prev;
    }

    for (u32 i = 0; i < framebuffer.size(); ++i) {
        PLATFORM.overwrite_t0_tile(i + 1,
                                   PLATFORM.encode_tile(framebuffer[i].data_));
    }

    int tile = 1;
    for (int y = 0; y < fb_tile_height; ++y) {
        for (int x = 0; x < fb_tile_width; ++x) {
            PLATFORM.set_tile(Layer::map_0_ext, 2 + x, 1 + y, tile++);
        }
    }
}



ScenePtr WorldMapScene::update(Time delta)
{
    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_keyframe_ = not cursor_keyframe_;
    }


    if (PLATFORM.speaker().is_music_playing("unaccompanied_wind") and
        not PLATFORM.speaker().is_sound_playing("creaking")) {
        PLATFORM.speaker().play_sound("creaking", 9);
    }


    auto load_savegame_txtr = [&] {
        if (APP.faction() == Faction::goblin) {
            PLATFORM.load_tile1_texture("goblin_savegame_flattened");
        } else {
            PLATFORM.load_tile1_texture("savegame_flattened");
        }
    };

    auto exit_nav_mode = [&] {
        cursor_ = cached_cursor_;
        navigation_buffer_.clear();
        nav_mode_ = false;
        APP.world_graph() = **cached_world_graph_;
        APP.current_world_location() = cursor_;
        state_ = State::deselected;
        return make_scene<WorldMapScene>();
    };


    auto to_move_state = [&] {
        state_ = State::move;
        movement_cursor_ = 0;

        movement_targets_.clear();

        auto collect_targets = [&](int w, int h) {
            auto cur = APP.world_graph().nodes_[cursor_];
            const auto l = cur.coord_.x - (w - 1);
            const auto t = cur.coord_.y - (h - 1);
            for (int x = l; x < cur.coord_.x + w; ++x) {
                for (int y = t; y < cur.coord_.y + h; ++y) {
                    for (auto& node : APP.world_graph().nodes_) {
                        if (node.type_ not_eq
                                WorldGraph::Node::Type::corrupted and
                            node.type_ not_eq WorldGraph::Node::Type::null and
                            node.coord_ not_eq cur.coord_ and
                            node.coord_ == Vec2<s8>{s8(x), s8(y)}) {
                            movement_targets_.insert(movement_targets_.begin(),
                                                     node.coord_);
                        }
                    }
                }
            }
        };

        if (APP.player_island().has_radar()) {
            collect_targets(6, 6);
            has_radar_ = true;
        } else {
            collect_targets(5, 5);
        }

        if (not nav_mode_ and not navigation_path_.empty()) {
            for (u32 i = 0; i < movement_targets_.size(); ++i) {
                if (movement_targets_[i] ==
                    APP.world_graph().nodes_[navigation_path_[0]].coord_) {
                    movement_cursor_ = i;
                }
            }
        }

        render_map_key();
    };


    if (APP.player().key_down(Key::start)) {
        show_tier_2_ = not show_tier_2_;
    }

    switch (state_) {
    case State::deselected:
        if (APP.player().key_down(Key::select)) {
            state_ = State::plot_moves;
        }
        if (APP.player().key_down(Key::action_1)) {
            state_ = State::selected;
            PLATFORM.speaker().play_sound("button_wooden", 3);
        }
        if (APP.player().key_down(Key::right) or
            APP.player().key_down(Key::left)) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
            to_move_state();
        }
        if (APP.player().key_down(Key::down)) {
            state_ = State::save_selected;
        }
        break;

    case State::selected:
        if (APP.player().key_down(Key::select)) {
            state_ = State::plot_moves;
        }
        if (APP.player().key_down(Key::action_1)) {
            to_move_state();
        }
        if (APP.player().key_down(Key::right) or
            APP.player().key_down(Key::left)) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
            to_move_state();
        } else if (APP.player().key_down(Key::down)) {
            state_ = State::save_selected;
        }
        if (APP.player().key_down(Key::action_2)) {
            state_ = State::deselected;
        }
        break;

    case State::save_selected:
        if (APP.player().key_down(Key::up) or
            APP.player().key_down(Key::action_2)) {
            state_ = State::selected;
        } else if (APP.player().key_down(Key::left)) {
            state_ = State::help_selected;
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::save_button_depressed;
            save_icon_.emplace(130, OverlayCoord{27, 17});
            timer_ = 0;
        }
        break;


    case State::help_selected:
        if (APP.player().key_down(Key::up) or
            APP.player().key_down(Key::action_2)) {
            state_ = State::selected;
        } else if (APP.player().key_down(Key::right)) {
            state_ = State::save_selected;
        } else if (APP.player().key_down(Key::left)) {
            state_ = State::logbook_selected;
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::help_button_depressed;
            help_icon_.emplace(138, OverlayCoord{24, 17});
            timer_ = 0;
        }
        break;


    case State::help_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::help_button_released_wait;
            help_icon_.emplace(134, OverlayCoord{24, 17});
        }
        break;


    case State::help_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_help;
        }
        break;


    case State::edit_selected:
        if (APP.player().key_down(Key::up) or
            APP.player().key_down(Key::action_2)) {
            state_ = State::selected;
        } else if (APP.player().key_down(Key::right)) {
            state_ = State::logbook_selected;
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::edit_button_depressed;
            edit_icon_.emplace(162, OverlayCoord{18, 17});
            timer_ = 0;
        }
        break;


    case State::edit_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::edit_button_released_wait;
            edit_icon_.emplace(158, OverlayCoord{18, 17});
        }
        break;


    case State::edit_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_edit;
        }
        break;


    case State::logbook_selected:
        if (APP.player().key_down(Key::up) or
            APP.player().key_down(Key::action_2)) {
            state_ = State::selected;
        } else if (APP.player().key_down(Key::right)) {
            state_ = State::help_selected;
        } else if (APP.player().key_down(Key::left)) {
            state_ = State::edit_selected;
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::logbook_button_depressed;
            logbook_icon_.emplace(154, OverlayCoord{21, 17});
            timer_ = 0;
        }
        break;


    case State::logbook_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::logbook_button_released_wait;
            logbook_icon_.emplace(150, OverlayCoord{21, 17});
        }
        break;


    case State::logbook_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_logbook;
        }
        break;


    case State::save_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::save_button_released_wait;
            save_icon_.emplace(126, OverlayCoord{27, 17});
        }
        break;


    case State::save_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;

            if (not(APP.persistent_data().state_flags_.get() &
                    PersistentData::permadeath_on)) {
                state_ = State::save_options;
                auto opt1 = SYSTR(wg_save_and_continue);
                auto opt2 = SYSTR(wg_save_and_quit);
                save_opt_len_ = std::max(utf8::len(opt1->c_str()),
                                         utf8::len(opt2->c_str()));

                save_icon_.reset();
                help_icon_.reset();
                logbook_icon_.reset();
                edit_icon_.reset();

                u8 x = 31 - (save_opt_len_ + 2);
                Text::print(opt1->c_str(), OverlayCoord{x, 17});
                Text::print(opt2->c_str(), OverlayCoord{x, 18});

            } else {
                state_ = State::fade_out_saved;
            }
        }
        break;


    case State::save_options: {
        const u8 x = 31 - (save_opt_len_ + 2);

        auto clr_txt = [&] {
            for (int i = -1; i < save_opt_len_; ++i) {
                PLATFORM.set_tile(Layer::overlay, x + i, 17, 0);
                PLATFORM.set_tile(Layer::overlay, x + i, 18, 0);
            }
        };

        if (save_opt_sel_) {
            PLATFORM.set_tile(Layer::overlay, x - 1, 17, 167);
            PLATFORM.set_tile(Layer::overlay, x - 1, 18, 166);
        } else {
            PLATFORM.set_tile(Layer::overlay, x - 1, 17, 166);
            PLATFORM.set_tile(Layer::overlay, x - 1, 18, 167);
        }

        if (APP.player().key_down(Key::down)) {
            save_opt_sel_ = 1;
        }
        if (APP.player().key_down(Key::up)) {
            save_opt_sel_ = 0;
        }

        if (APP.player().key_down(Key::action_2)) {
            clr_txt();
            redraw_icons();
            state_ = State::save_selected;
        } else if (APP.player().key_down(Key::action_1)) {
            if (save_opt_sel_ == 0) {
                clr_txt();
                redraw_icons();
                save::store("", APP.persistent_data());
                PLATFORM.speaker().play_sound("button_wooden", 3);
                state_ = State::save_selected;
                show_saved_indicator();
            } else {
                clr_txt();
                redraw_icons();
                state_ = State::fade_out_saved;
            }
        }
        break;
    }


    case State::move: {
        if (not tier_2_visible_) {
            tier_2_timer_ += delta;
            if (tier_2_timer_ > milliseconds(1500)) {
                tier_2_timer_ = 0;
                tier_2_visible_ = true;
            }
        }
        if (nav_mode_) {
            auto node = APP.world_graph().nodes_[cursor_];
            if (node.type_ == WorldGraph::Node::Type::corrupted) {
                return exit_nav_mode();
            }
        }
        if (nav_mode_ and APP.player().key_down(Key::select) and
            navigation_buffer_.size() > 1) {
            state_ = State::save_plot;
            return null_scene();
        }
        if (APP.player().key_down(Key::action_1)) {
            if (nav_mode_) {
                for (int i = 0; i < 19; ++i) {
                    auto node = APP.world_graph().nodes_[i];
                    if (node.coord_ == movement_targets_[movement_cursor_]) {
                        cursor_ = i;
                        if (node.type_ == WorldGraph::Node::Type::exit) {
                            state_ = State::save_plot;
                            navigation_buffer_.push_back(cursor_);
                            plot_navigation_path(navigation_buffer_);
                            return null_scene();
                        }
                        APP.world_graph().nodes_[cursor_].type_ =
                            WorldGraph::Node::Type::visited;
                        Text::print(
                            stringify(navigation_buffer_.size()).c_str(),
                            OverlayCoord{u8(node.coord_.x + 5),
                                         u8(node.coord_.y + 3 + 1)});
                        APP.current_world_location() = cursor_;
                        update_storm_frontier(APP.world_graph(), 0);
                        ++APP.world_graph().storm_depth_;
                        show_map(APP.world_graph(), 0);
                        PLATFORM_EXTENSION(force_vsync);
                        draw_stormcloud_background(
                            APP.world_graph().storm_depth_, false);
                        auto node = APP.world_graph().nodes_[cursor_];
                        if (node.type_ == WorldGraph::Node::Type::corrupted) {
                            PLATFORM.speaker().play_sound("beep_error", 4);
                            PLATFORM.sleep(60);
                        } else {
                            PLATFORM.speaker().play_sound("button_wooden", 3);
                        }
                    }
                }
                navigation_buffer_.push_back(cursor_);
                plot_navigation_path(navigation_buffer_);
                to_move_state();
                break;
            }

            if (APP.world_graph().nodes_[cursor_].type_ not_eq
                WorldGraph::Node::Type::shop) {
                APP.world_graph().nodes_[cursor_].type_ =
                    WorldGraph::Node::Type::visited;
            }

            for (int i = 0; i < 19; ++i) {
                if (APP.world_graph().nodes_[i].coord_ ==
                    movement_targets_[movement_cursor_]) {
                    cursor_ = i;
                }
            }
            // In case anything goes wrong: create an emergency backup!
            APP.create_backup(App::BackupContext{
                .next_world_location_ = (s8)cursor_,
            });

            if (not nav_mode_ and not navigation_path_.empty()) {
                cached_navigation_path_ = navigation_path_;
                if (navigation_path_[0] == cursor_) {
                    navigation_path_.erase(navigation_path_.begin());
                } else {
                    navigation_path_.clear();
                }
            }

            prev_world_loc_ = APP.current_world_location();
            APP.current_world_location() = cursor_;
            show_map(APP.world_graph(), 0);
            cmix_ = {};
            map_key_.reset();
            redraw_icons();
            update_storm_frontier(APP.world_graph(), 0);
            ++APP.world_graph().storm_depth_;

            if (APP.world_graph().nodes_[cursor_].type_ ==
                WorldGraph::Node::Type::visited) {
                draw_stormcloud_background(APP.world_graph().storm_depth_,
                                           false);
                state_ = State::storm_advance;
            } else {
                PLATFORM.speaker().play_sound("button_wooden", 3);
                state_ = State::wait;
                cmix_ = {ColorConstant::stil_de_grain, 200};
            }

        } else if (APP.player().key_down(Key::action_2)) {
            if (nav_mode_) {
                return exit_nav_mode();
            }
            state_ = State::selected;
            map_key_.reset();
            redraw_icons();
            update_storm_frontier(APP.world_graph(), 0);
            show_map(APP.world_graph(), 0);
            cmix_ = {};
        }
        // auto current = movement_targets_[movement_cursor_];
        if (APP.player().key_down(Key::left)) {
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
                PLATFORM.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = left[0];
            } else {
                search(3);

                if (not left.empty()) {
                    PLATFORM.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = left[0];
                } else {
                    search(5);
                    if (not left.empty()) {
                        PLATFORM.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = left[0];
                    } else {
                        search(7);
                        if (not left.empty()) {
                            PLATFORM.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = left[0];
                        } else {
                            search(8);
                            if (not left.empty()) {
                                PLATFORM.speaker().play_sound("click_wooden",
                                                              2);
                                movement_cursor_ = left[0];
                            }
                        }
                    }
                }
            }
            render_map_key();
        } else if (APP.player().key_down(Key::right)) {
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
                PLATFORM.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = right[0];
            } else {
                search(3);

                if (not right.empty()) {
                    PLATFORM.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = right[0];
                } else {
                    search(5);

                    if (not right.empty()) {
                        PLATFORM.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = right[0];
                    } else {
                        search(7);
                        if (not right.empty()) {
                            PLATFORM.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = right[0];
                        } else {
                            search(8);
                            if (not right.empty()) {
                                PLATFORM.speaker().play_sound("click_wooden",
                                                              2);
                                movement_cursor_ = right[0];
                            }
                        }
                    }
                }
            }
            render_map_key();
        } else if (APP.player().key_down(Key::up)) {
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
                PLATFORM.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = above[0];
            } else {
                search(3);
                if (not above.empty()) {
                    PLATFORM.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = above[0];
                } else {
                    search(5);
                    if (not above.empty()) {
                        PLATFORM.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = above[0];
                    } else {
                        search(7);
                        if (not above.empty()) {
                            PLATFORM.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = above[0];
                        }
                    }
                }
            }
            render_map_key();
        } else if (APP.player().key_down(Key::down)) {
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
                PLATFORM.speaker().play_sound("click_wooden", 2);
                movement_cursor_ = beneath[0];
            } else {

                search(3);

                if (not beneath.empty()) {
                    PLATFORM.speaker().play_sound("click_wooden", 2);
                    movement_cursor_ = beneath[0];
                } else {
                    search(5);
                    if (not beneath.empty()) {
                        PLATFORM.speaker().play_sound("click_wooden", 2);
                        movement_cursor_ = beneath[0];
                    } else {
                        search(7);
                        if (not beneath.empty()) {
                            PLATFORM.speaker().play_sound("click_wooden", 2);
                            movement_cursor_ = beneath[0];
                        }
                    }
                }
            }
            render_map_key();
        }
        break;
    }

    case State::plot_moves: {
        nav_mode_ = true;
        heading_->assign(SYSTR(wg_nav)->c_str());
        cached_cursor_ = cursor_;
        cached_world_graph_ =
            allocate_dynamic<WorldGraph>("cached-world-graph");
        **cached_world_graph_ = APP.world_graph();
        navigation_buffer_.clear();
        navigation_buffer_.push_back(cursor_);
        draw_border();
        to_move_state();
        break;
    }

    case State::save_plot: {
        PLATFORM.speaker().play_sound("button_wooden", 3);
        PLATFORM.sleep(20);
        navigation_buffer_.erase(navigation_buffer_.begin());
        navigation_path_ = navigation_buffer_;
        return exit_nav_mode();
    }

    case State::abort_move: {
        Text::print("canceling...",
                    {9, 9},
                    Text::OptColors{
                        {ColorConstant::rich_black, custom_color(0xff8e38)}});
        exit_label_.reset();
        PLATFORM.screen().schedule_fade(
            0.5f, ColorConstant::rich_black, true, true);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.speaker().play_sound("cancel", 5);
        PLATFORM.sleep(30);
        navigation_path_ = cached_navigation_path_;
        APP.current_world_location() = prev_world_loc_;
        auto next = make_scene<WorldMapScene>();
        APP.world_graph().storm_depth_--;
        return next;
    }

    case State::wait:
        if (APP.player().key_down(Key::action_2) and can_abort_move()) {
            state_ = State::abort_move;
            break;
        }
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
        if (APP.player().key_down(Key::action_2) and can_abort_move()) {
            state_ = State::abort_move;
            break;
        }
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(700);
        if (timer_ > fade_duration) {
            PLATFORM.speaker().clear_sounds();
            return make_scene<LoadLevelScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().fade(
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
            // PLATFORM.screen().fade(1.f, custom_color(0x6057b1), {}, false, false);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }


    case State::show_node_death_icons:
        if (dead_nodes_.empty()) {
            state_ = State::deselected;
        } else {
            timer_ += delta;
            if (APP.player().key_down(Key::action_1) or
                APP.player().key_down(Key::action_2)) {
                fast_ = true;
            }
            if (fast_) {
                timer_ += delta;
            }
            if (timer_ > node_death_sequence_time) {
                timer_ = 0;
                state_ = State::deselected;
                auto current = APP.world_graph().nodes_[cursor_];
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
            show_map(APP.world_graph(), 0);
            update_storm_frontier(APP.world_graph(), 0);
        } else {
            const auto amount =
                1.f - smoothstep(0.f, fade_duration, storm_scroll_timer_);
            PLATFORM.set_scroll(Layer::map_1_ext, amount * 16, 0);
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
            show_map(APP.world_graph(), 0);
            update_storm_frontier(APP.world_graph(), 0);
        } else {
            const auto amount =
                1.f - smoothstep(0.f, fade_duration, storm_scroll_timer_);
            PLATFORM.set_scroll(Layer::map_1_ext, amount * 16, 0);
        }
        break;
    }


    case State::fade_out_saved: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::print_saved_text;
            save::store("", APP.persistent_data());
            PLATFORM.fill_overlay(0);
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 16; ++y) {
                    PLATFORM.set_tile(Layer::map_0_ext, x, y, 0);
                }
            }
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::fade_out_logbook: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            PLATFORM.fill_overlay(0);
            auto next = make_scene<AdventureLogScene>();
            next->set_next_scene([] { return make_scene<WorldMapScene>(); });
            return next;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::fade_out_help: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            PLATFORM.fill_overlay(0);
            return make_scene<HintScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::fade_out_edit: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            PLATFORM.fill_overlay(0);
            APP.reset_opponent_island();
            APP.player_island().set_position(
                {Fixnum::from_integer(10), Fixnum::from_integer(374)});
            APP.level_timer().reset(0);
            show_island_interior(&APP.player_island());
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 16; ++y) {
                    PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
                }
            }
            PLATFORM.speaker().stream_music(
                APP.environment().ambiance()->c_str(), 0);
            auto maxvol = Platform::Speaker::music_volume_max;
            PLATFORM.speaker().set_music_volume(maxvol);
            APP.invoke_script("/scripts/reset_hooks.lisp");
            update_weather_onload();
            return make_scene<FadeInScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            auto max_vol = Platform::Speaker::music_volume_max;
            u8 vol = max_vol * (1.f - amount);
            PLATFORM.speaker().set_music_volume(clamp(vol, (u8)2, max_vol));
            PLATFORM.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::print_saved_text: {
        PLATFORM.load_overlay_texture("overlay");
        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
            PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 5, 112);
        }
        state_ = State::show_saved_text;
        load_savegame_txtr();
        __draw_image(1, 0, 4, 30, 13, Layer::map_1);
        heading_.emplace(SYSTR(wg_saved)->c_str(), OverlayCoord{1, 1});
        PLATFORM.speaker().play_sound("button_wooden", 3);
        PLATFORM.speaker().stop_music();
        break;
    }


    case State::show_saved_text: {
        if (timer_ == 0) {
            PLATFORM.screen().schedule_fade(0.f);
            load_savegame_txtr();
        }
        const auto prev_timer = timer_;
        timer_ += delta;

        if (APP.player().key_down(Key::action_1) or
            APP.player().key_down(Key::action_2)) {
            timer_ = milliseconds(7450);
        }

        if (timer_ > milliseconds(7500)) {
            heading_.reset();
            for (int i = 0; i < 30; ++i) {
                PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            }
        }

        if (timer_ > milliseconds(7600)) {
            const auto screen_tiles = calc_screen_tiles();
            for (int i = 0; i < 30; ++i) {
                PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
            }
        }

        if (timer_ > milliseconds(7700)) {
            const auto screen_tiles = calc_screen_tiles();
            for (int i = 0; i < 30; ++i) {
                PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            }
        }

        const auto t1_thresh = milliseconds(750);
        if (prev_timer < t1_thresh and timer_ > t1_thresh) {
            Text t(SYSTR(wg_saved_description)->c_str(), OverlayCoord{1, 16});
            t.__detach();
        }

        const auto t2_thresh = milliseconds(1450);
        if (prev_timer < t2_thresh and timer_ > t2_thresh) {
            Text t(SYSTR(wg_saved_come_back_soon)->c_str(),
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
                    PLATFORM.set_tile(Layer::overlay, x, y, 112);
                }
                for (int x = 0; x < 30; ++x) {
                    PLATFORM.set_tile(Layer::overlay, x, 14 - (y - 4), 112);
                }
            } else if (((y + 1) - 4) * 8 > fractional_pixels and
                       fractional_pixels % 8) {
                for (int x = 0; x < 30; ++x) {
                    PLATFORM.set_tile(
                        Layer::overlay, x, y, 119 - (fractional_pixels % 8));
                }
                for (int x = 0; x < 30; ++x) {
                    PLATFORM.set_tile(Layer::overlay,
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
            return make_scene<TitleScreenScene>();
        }
        break;
    }



    return null_scene();
}



WorldMapScene::NavBuffer WorldMapScene::navigation_path_;



WorldMapScene::NavBuffer& WorldMapScene::nav_path()
{
    return navigation_path_;
}



void WorldMapScene::reset_nav_path()
{
    navigation_path_.clear();
}



void WorldMapScene::display()
{
    if (state_ == State::show_saved_text or state_ == State::save_animate_out or
        state_ == State::save_exit or state_ == State::abort_move) {
        return;
    }

    Sprite cursor;
    cursor.set_priority(0);

    if (++palette_cyc_counter_ == 7) {
        palette_cyc_counter_ = 0;
        PLATFORM_EXTENSION(rotate_palette, Layer::map_0_ext, 1, 8);
    }

    auto show_cursor = [&cursor, this](int cursor_) {
        Vec2<s8> cursor_loc = APP.world_graph().nodes_[cursor_].coord_;
        cursor_loc.x += 5;
        cursor_loc.y += 3;

        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_tidx_16x16(28, 0);
        cursor.set_position({Fixnum((int)cursor_loc.x * 8) - 8.0_fixed,
                             Fixnum((int)cursor_loc.y * 8) - 12.0_fixed});
        cursor.set_mix(cmix_);
        PLATFORM.screen().draw(cursor);
    };

    if (nav_mode_) {
        show_cursor(navigation_buffer_[0]);
        if (state_ not_eq State::save_plot) {
            show_cursor(navigation_buffer_.back());
        }
    } else {
        show_cursor(cursor_);
    }



    cursor.set_mix({});

    cursor.set_size(Sprite::Size::w16_h16);


    if (state_ == State::selected) {
        auto current = APP.world_graph().nodes_[cursor_].coord_;
        cursor.set_texture_index((15 * 2) + cursor_keyframe_);
        cursor.set_position({Fixnum((current.x + map_start_x) * Float(8) - 4),
                             Fixnum((current.y + map_start_y) * Float(8) - 4)});
        PLATFORM.screen().draw(cursor);
    } else if (state_ == State::move) {

        auto target = movement_targets_[movement_cursor_];
        cursor.set_texture_index((15 * 2) + cursor_keyframe_);
        cursor.set_position({Fixnum((target.x + map_start_x) * Float(8) - 4),
                             Fixnum((target.y + map_start_y) * Float(8) - 4)});
        PLATFORM.screen().draw(cursor);

        for (auto& t : APP.world_graph().nodes_) {
            if (t.type_ not_eq WorldGraph::Node::Type::null and
                t.type_ not_eq WorldGraph::Node::Type::exit and
                t.type_ not_eq WorldGraph::Node::Type::corrupted and
                is_x_behind_storm_frontier(t.coord_.x, 1)) {

                cursor.set_size(Sprite::Size::w16_h16);
                cursor.set_tidx_16x16(99, 1);
                cursor.set_position(
                    {Fixnum((t.coord_.x + map_start_x) * Float(8) - 3),
                     Fixnum((t.coord_.y + map_start_y) * Float(8) - (12))});
                cursor.set_mix({});
                cursor.set_priority(0);
                cursor.set_alpha(Sprite::Alpha::opaque);
                PLATFORM.screen().draw(cursor);
            }
        }

        auto current = APP.world_graph().nodes_[cursor_];
        auto x = (current.coord_.x + map_start_x) - 4;
        auto y = (current.coord_.y + map_start_y) - 4;
        cursor.set_texture_index(76);
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_alpha(Sprite::Alpha::translucent);
        cursor.set_priority(2);

        auto draw_range = [&cursor, this](int x, int y) {
            if (has_radar_) {
                for (int i = 0; i < 5; ++i) {
                    cursor.set_position({Fixnum(Float(x - 1) * 8 + i * 16),
                                         Fixnum(Float(y) * 8 + 32)});
                    PLATFORM.screen().draw(cursor);
                    cursor.set_position({Fixnum(Float(x - 1) * 8 + i * 16),
                                         Fixnum(Float(y) * 8)});
                    PLATFORM.screen().draw(cursor);
                }

                for (int i = 0; i < 5; ++i) {
                    cursor.set_texture_index(91);
                    cursor.set_position({Fixnum(Float(x - 1) * 8 + i * 16),
                                         Fixnum(Float(y) * 8 + 64)});
                    PLATFORM.screen().draw(cursor);
                    cursor.set_texture_index(74);
                    cursor.set_position({Fixnum(Float(x - 1) * 8 + i * 16),
                                         Fixnum(Float(y) * 8 - 8)});
                    PLATFORM.screen().draw(cursor);
                }


                cursor.set_texture_index(75);
                cursor.set_position(
                    {Fixnum(Float(x + 1) * 8 + 64), Fixnum(Float(y - 1) * 8)});
                PLATFORM.screen().draw(cursor);
                cursor.set_position({Fixnum(Float(x + 1) * 8 + 64),
                                     Fixnum(Float(y - 1) * 8 + 32)});
                PLATFORM.screen().draw(cursor);
                cursor.set_position({Fixnum(Float(x + 1) * 8 + 64),
                                     Fixnum(Float(y - 1) * 8 + 32 + 8)});
                PLATFORM.screen().draw(cursor);
                cursor.set_position({Fixnum(Float(x + 1) * 8 + 64),
                                     Fixnum(Float(y - 1) * 8 + 32 + 24)});
                PLATFORM.screen().draw(cursor);
                return;
            }

            for (int i = 0; i < 4; ++i) {
                cursor.set_position(
                    {Fixnum(Float(x) * 8 + i * 16), Fixnum(Float(y) * 8 + 32)});
                PLATFORM.screen().draw(cursor);
                cursor.set_position(
                    {Fixnum(Float(x) * 8 + i * 16), Fixnum(Float(y) * 8)});
                PLATFORM.screen().draw(cursor);
            }

            cursor.set_texture_index(74);
            for (int i = 0; i < 4; ++i) {
                cursor.set_position(
                    {Fixnum(Float(x) * 8 + i * 16), Fixnum(Float(y) * 8 + 64)});
                PLATFORM.screen().draw(cursor);
            }

            cursor.set_texture_index(75);
            cursor.set_position(
                {Fixnum(Float(x) * 8 + 64), Fixnum(Float(y) * 8)});
            PLATFORM.screen().draw(cursor);
            cursor.set_position(
                {Fixnum(Float(x) * 8 + 64), Fixnum(Float(y) * 8 + 32)});
            PLATFORM.screen().draw(cursor);
            cursor.set_position(
                {Fixnum(Float(x) * 8 + 64), Fixnum(Float(y) * 8 + 32 + 8)});
            PLATFORM.screen().draw(cursor);
        };

        draw_range(x, y);

        cursor.set_mix({ColorConstant::rich_black, 64});
        auto o = movement_targets_[movement_cursor_];
        cursor.set_texture_index(76);
        draw_range(o.x + map_start_x - 4, o.y + map_start_y - 4);

        Buffer<Vec2<s8>, 10> tier_2_reachable;
        for (int x = o.x - 4; x < o.x + 5; ++x) {
            for (int y = o.y - 4; y < o.y + 5; ++y) {
                for (auto& node : APP.world_graph().nodes_) {
                    if (node.type_ not_eq WorldGraph::Node::Type::corrupted and
                        node.type_ not_eq WorldGraph::Node::Type::null and
                        node.coord_ not_eq o and
                        node.coord_ == Vec2<s8>{s8(x), s8(y)}) {
                        tier_2_reachable.push_back(node.coord_);
                    }
                }
            }
        }


        if (not has_radar_ and show_tier_2_ and tier_2_visible_) {
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
        PLATFORM.screen().draw(cursor);
    } else if (state_ == State::logbook_selected or
               state_ == State::logbook_button_depressed or
               state_ == State::logbook_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(160), Fixnum::from_integer(128)});
        PLATFORM.screen().draw(cursor);
    } else if (state_ == State::edit_selected //  or
               // state_ == State::edit_button_depressed or
               // state_ == State::edit_button_released_wait
    ) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(136), Fixnum::from_integer(128)});
        PLATFORM.screen().draw(cursor);
    } else if (state_ == State::help_selected or
               state_ == State::help_button_depressed or
               state_ == State::help_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position(
            {Fixnum::from_integer(184), Fixnum::from_integer(128)});
        PLATFORM.screen().draw(cursor);
    } else if (state_ == State::show_node_death_icons) {
        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_tidx_16x16(73, 1);
        const int offset =
            interpolate(9, 0, Float(timer_) / node_death_sequence_time);
        for (auto& node : dead_nodes_) {
            cursor.set_position({});
            cursor.set_position(
                {Fixnum::from_integer((node.x + map_start_x) * Float(8) - 1),
                 Fixnum::from_integer((node.y + map_start_y) * Float(8) -
                                      (4 + offset))});
            PLATFORM.screen().draw(cursor);
        }
    }
}



void WorldMapScene::enter(Scene& prev_scene)
{
    PLATFORM.screen().set_shader(passthrough_shader);

    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    APP.swap_player<PlayerP1>();

    APP.effects().clear();
    APP.player_island().projectiles().clear();

    cursor_ = APP.current_world_location();

    auto view = PLATFORM.screen().get_view();
    view.set_center({});
    PLATFORM.screen().set_view(view);

    APP.time_stream().clear();

    auto& current = APP.world_graph().nodes_[cursor_];
    if (current.type_ not_eq WorldGraph::Node::Type::shop) {
        current.type_ = WorldGraph::Node::Type::visited;
    }

    PLATFORM.load_overlay_texture("overlay_world_map");
    PLATFORM.load_tile1_texture("tilesheet_world_map_backdrop");
    PLATFORM.load_tile0_texture("tilesheet_world_map_framebuffer");

    PLATFORM.set_scroll(Layer::map_1_ext, 16, 0);
    draw_stormcloud_background(APP.world_graph().storm_depth_);

    PLATFORM.enable_glyph_mode(true);

    draw_border();

    heading_.emplace(OverlayCoord{1, 1});

    heading_->assign(format(SYSTR(wg_title)->c_str(), APP.zone()).c_str());

    warning_.emplace(OverlayCoord{1, 18});
    warning_->assign(SYSTR(wg_storm_label)->c_str());

    show_map(APP.world_graph(), -1);
    if (not navigation_path_.empty() and not nav_mode_) {
        navigation_path_.insert(navigation_path_.begin(), cursor_);
        plot_navigation_path(navigation_path_);
        navigation_path_.erase(navigation_path_.begin());
    }

    redraw_icons();

    for (auto& node : APP.world_graph().nodes_) {
        if (node.type_ == WorldGraph::Node::Type::exit) {
            exit_label_.emplace(

                SYSTR(wg_exit)->c_str(),
                OverlayCoord{u8(node.coord_.x + map_start_x),
                             u8(node.coord_.y + map_start_y - 1)});
        }
    }

    for (int x = 0; x < 5; ++x) {
        PLATFORM.set_tile(Layer::overlay, 30 - 5 + x, 0, 179 + x);
    }

    update_storm_frontier(APP.world_graph(), -1);
}



void WorldMapScene::draw_border()
{
    auto st = calc_screen_tiles();

    for (int x = 1; x < st.x - 1; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 0, 89);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 1, 90);
    }

    for (int y = 1; y < st.y - 1; ++y) {
        PLATFORM.set_tile(Layer::overlay, 0, y, 91);
        PLATFORM.set_tile(Layer::overlay, st.x - 1, y, 92);
    }

    PLATFORM.set_tile(Layer::overlay, 0, 0, 93);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, 0, 95);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - 1, 96);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, st.y - 1, 94);
}



void WorldMapScene::show_map(WorldGraph& map, int storm_depth_offset)
{
    dead_nodes_.clear();

    for (auto& node : map.nodes_) {
        if (node.type_ == WorldGraph::Node::Type::null) {
            continue;
        }
        bool on_nav_path = false;

        if (not navigation_buffer_.empty()) {
            auto it = navigation_buffer_.begin();
            ++it;
            for (; it not_eq navigation_buffer_.end(); ++it) {
                if (map.nodes_[*it].coord_ == node.coord_) {
                    on_nav_path = true;
                }
            }
            if (map.nodes_[cursor_].coord_ == node.coord_) {
                on_nav_path = true;
            }
        }

        if (not nav_mode_) {
            for (auto idx : navigation_path_) {
                auto node = map.nodes_[idx];
                if (node.type_ == WorldGraph::Node::Type::corrupted or
                    node.type_ == WorldGraph::Node::Type::visited) {
                    continue;
                }
            }
        }

        if (is_x_behind_storm_frontier(node.coord_.x, storm_depth_offset)) {

            if (node.type_ == WorldGraph::Node::Type::exit) {
                exit_label_.reset();
            }
            if (node.type_ not_eq WorldGraph::Node::Type::corrupted) {
                dead_nodes_.push_back(node.coord_);
            }
            node.type_ = WorldGraph::Node::Type::corrupted;

            if (on_nav_path) {
                PLATFORM.set_tile(Layer::overlay,
                                  map_start_x + node.coord_.x,
                                  map_start_y + node.coord_.y,
                                  170);
            } else {
                PLATFORM.set_tile(Layer::overlay,
                                  map_start_x + node.coord_.x,
                                  map_start_y + node.coord_.y,
                                  98);
            }

        } else {

            if (on_nav_path) {
                PLATFORM.set_tile(Layer::overlay,
                                  map_start_x + node.coord_.x,
                                  map_start_y + node.coord_.y,
                                  170);
            } else {
                PLATFORM.set_tile(Layer::overlay,
                                  map_start_x + node.coord_.x,
                                  map_start_y + node.coord_.y,
                                  114 + (int)node.type_);
            }
        }
    }
}



void WorldMapScene::exit(Scene& next_scene)
{
    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    PLATFORM.fill_overlay(0);

    save_icon_.reset();
    help_icon_.reset();
    logbook_icon_.reset();
    exit_label_.reset();
    edit_icon_.reset();
    heading_.reset();
    warning_.reset();
    map_key_.reset();

    PLATFORM.load_overlay_texture("overlay");

    show_island_exterior(APP.opponent_island());
}



} // namespace skyland
