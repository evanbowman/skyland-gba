#include "worldMapScene.hpp"
#include "graphics/overlay.hpp"
#include "hintScene.hpp"
#include "loadLevelScene.hpp"
#include "platform/platform.hpp"
#include "skyland/save.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/worldMap.hpp"
#include "titleScreenScene.hpp"
#include "skyland/worldGraph.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



// TODO: create a worldGraph.cpp and move this function there.
void WorldGraph::generate()
{
    int walk_point = 0;
    nodes_[0].coord_ = {0, s8(4 + rng::choice(height - 8, rng::critical_state))};

    for (auto& node : nodes_) {
        node.type_ = WorldGraph::Node::Type::neutral;
    }

    // Random walk from left to right.
    while (true) {
        if (nodes_[walk_point].coord_.x > (width - (max_movement_distance - 3))) {
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
    }

    int hostile_levels = 6;
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

    int place_quest_levels = 1;
    while (place_quest_levels) {
        for (int i = exit_node; i < 18; ++i) {
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
        pfrm.set_tile(Layer::map_1_ext, edge, y, 3 + (y % 4));
    }
}



void WorldMapScene::update_storm_frontier(Platform& pfrm)
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
        const int x = 1 + (storm_depth_ + 1) * 2;
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



ScenePtr<Scene>
WorldMapScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_keyframe_ = not cursor_keyframe_;
    }


    // auto& node = app.world_map().matrix_[cursor_.x][cursor_.y];


    switch (state_) {
    case State::deselected:
        if (pfrm.keyboard()
                .down_transition<Key::right,
                                 Key::left,
                                 Key::up,
                                 Key::down,
                                 Key::action_1>()) {
            state_ = State::move;
            movement_cursor_ = 0;

            movement_targets_.clear();
            auto current = app.world_graph().nodes_[cursor_];
            for (int x = current.coord_.x - 4; x < current.coord_.x + 5; ++x) {
                for (int y = current.coord_.y - 4; y < current.coord_.y + 5; ++y) {
                    for (auto& node : app.world_graph().nodes_) {
                        if (node.type_ not_eq WorldGraph::Node::Type::corrupted and
                            node.coord_ not_eq current.coord_ and
                            node.coord_ == Vec2<s8>{s8(x), s8(y)}) {
                            movement_targets_.insert(movement_targets_.begin(),
                                                     node.coord_);
                        }
                    }
                }
            }
        }
        break;


    case State::explore_paths:
        if (app.player().key_down(pfrm, Key::action_1)) {
            storm_depth_ += 1;
            state_ = State::storm_advance;
        }
        break;
        // if (app.player().key_down(pfrm, Key::action_2)) {
        //     state_ = State::deselected;
        //     cursor_ = app.current_map_location();
        //     show_map(pfrm, app.world_map());
        //     break;
        // }
        // if (app.player().key_down(pfrm, Key::action_1)) {
        //     if (cursor_ == app.current_map_location()) {
        //         state_ = State::move;
        //         show_move_arrows(pfrm, app);
        //     }
        // }
        // if (app.player().key_down(pfrm, Key::right) and
        //     node.connections_.mask_ & WorldMap::Node::Connections::r) {
        //     cursor_.x += 1;
        //     show_map(pfrm, app.world_map());
        // } else if (app.player().key_down(pfrm, Key::left) and
        //            node.connections_.mask_ & WorldMap::Node::Connections::l) {
        //     cursor_.x -= 1;
        //     show_map(pfrm, app.world_map());
        // } else if (app.player().key_down(pfrm, Key::up)) {
        //     if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
        //         cursor_.x += 1;
        //         cursor_.y -= 1;
        //         show_map(pfrm, app.world_map());
        //     } else if (node.connections_.mask_ &
        //                WorldMap::Node::Connections::lu) {
        //         cursor_.x -= 1;
        //         cursor_.y -= 1;
        //         show_map(pfrm, app.world_map());
        //     }
        // } else if (app.player().key_down(pfrm, Key::down)) {
        //     if (node.connections_.mask_ & WorldMap::Node::Connections::ld) {
        //         cursor_.x -= 1;
        //         cursor_.y += 1;
        //         show_map(pfrm, app.world_map());
        //     } else if (node.connections_.mask_ &
        //                WorldMap::Node::Connections::rd) {
        //         cursor_.x += 1;
        //         cursor_.y += 1;
        //         show_map(pfrm, app.world_map());
        //     } else {
        //         state_ = State::save_selected;
        //         const auto cached_cursor = cursor_;
        //         cursor_ = app.current_map_location(); // needed for show_map
        //         show_map(pfrm, app.world_map());
        //         cursor_ = cached_cursor;
        //     }
        // }
        break;


    case State::save_selected:
        if (app.player().key_down(pfrm, Key::up) or
            app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::explore_paths;
            show_map(pfrm, app.world_graph());
        } else if (app.player().key_down(pfrm, Key::left)) {
            state_ = State::help_selected;
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
            state_ = State::explore_paths;
            show_map(pfrm, app.world_graph());
        } else if (app.player().key_down(pfrm, Key::right)) {
            state_ = State::save_selected;
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::help_button_depressed;
            help_icon_.emplace(pfrm, 138, OverlayCoord{24, 17});
            timer_ = 0;
        }
        break;


    case State::help_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::help_button_released_wait;
            help_icon_.emplace(pfrm, 134, OverlayCoord{24, 17});
        }
        break;


    case State::help_button_released_wait:
        timer_ += delta;
        if (timer_ > milliseconds(60)) {
            timer_ = 0;
            state_ = State::fade_out_help;
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
            // if (cmix_.amount_ > 0) {
            //     timer_ += delta;
            //     if (timer_ > 12000) {
            //         timer_ -= 12000;
            //         cmix_ = {cmix_.color_, u8(cmix_.amount_ - 5)};
            //     }
            // } else {
            //     timer_ = 0;
            //     cmix_ = {ColorConstant::silver_white, 200};
            // }
            if (app.player().key_down(pfrm, Key::action_1)) {
                app.world_graph().nodes_[cursor_].type_ =
                    WorldGraph::Node::Type::visited;
                for (int i = 0; i < 18; ++i) {
                    if (app.world_graph().nodes_[i].coord_ == movement_targets_[movement_cursor_]) {
                        cursor_ = i;
                    }
                }
                show_map(pfrm, app.world_graph());
                cmix_ = {};
                draw_stormcloud_background(pfrm, app, ++storm_depth_, false);
                state_ = State::storm_advance;
            } else if (app.player().key_down(pfrm, Key::action_2)) {
                state_ = State::deselected;
                show_map(pfrm, app.world_graph());
                cmix_ = {};
            }
            // auto current = movement_targets_[movement_cursor_];
            if (app.player().key_down(pfrm, Key::right)) {
                movement_cursor_++;
                if ((u32)movement_cursor_ == movement_targets_.size()) {
                    movement_cursor_ = 0;
                }
                // for (u32 i = 0; i < movement_targets_.size(); ++i) {
                //     if (movement_targets_[i].x > current.x) {
                //         movement_cursor_ = i;
                //         break;
                //     }
                // }
            } else if (app.player().key_down(pfrm, Key::left)) {
                movement_cursor_--;
                if (movement_cursor_ < 0) {
                    movement_cursor_ = movement_targets_.size() - 1;
                }
                // for (u32 i = 0; i < movement_targets_.size(); ++i) {
                //     if (movement_targets_[i].x < current.x) {
                //         movement_cursor_ = i;
                //         break;
                //     }
                // }
            }
            break;
    }


    //     if (app.player().key_down(pfrm, Key::action_2)) {
    //         state_ = State::explore_paths;
    //         cursor_ = app.current_map_location();
    //         show_map(pfrm, app.world_map());
    //         cmix_ = {};
    //         break;
    //     }
    //     if (app.player().key_down(pfrm, Key::up) and
    //         node.connections_.mask_ & WorldMap::Node::Connections::ru) {
    //         for (int i = 0; i < 3; ++i) {
    //             move_arrow_sel_[i] = false;
    //         }
    //         move_arrow_sel_[0] = true;
    //         show_move_arrows(pfrm, app);
    //     }
    //     if (app.player().key_down(pfrm, Key::right) and
    //         node.connections_.mask_ & WorldMap::Node::Connections::r) {
    //         for (int i = 0; i < 3; ++i) {
    //             move_arrow_sel_[i] = false;
    //         }
    //         move_arrow_sel_[1] = true;
    //         show_move_arrows(pfrm, app);
    //     }
    //     if (app.player().key_down(pfrm, Key::down) and
    //         node.connections_.mask_ & WorldMap::Node::Connections::rd) {
    //         for (int i = 0; i < 3; ++i) {
    //             move_arrow_sel_[i] = false;
    //         }
    //         move_arrow_sel_[2] = true;
    //         show_move_arrows(pfrm, app);
    //     }

    //     if (app.player().key_down(pfrm, Key::action_1)) {
    //         state_ = State::wait;
    //         cmix_ = {ColorConstant::stil_de_grain, 200};
    //         if (move_arrow_sel_[0]) {
    //             app.current_map_location().x += 1;
    //             app.current_map_location().y -= 1;
    //         } else if (move_arrow_sel_[1]) {
    //             app.current_map_location().x += 1;
    //         } else if (move_arrow_sel_[2]) {
    //             app.current_map_location().x += 1;
    //             app.current_map_location().y += 1;
    //         }
    //     }
    //     break;

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
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            // Create a backup before entering a level. If the game encounters
            // an unrecoverrable error, it will create a save from the backup
            // data before crashing.
            app.create_backup(pfrm);
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
            update_storm_frontier(pfrm);
            // pfrm.screen().fade(1.f, custom_color(0x6057b1), {}, false, false);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::storm_advance: {
        storm_scroll_timer_ += delta;
        constexpr auto fade_duration = milliseconds(1500);
        if (storm_scroll_timer_ > fade_duration) {
            storm_scroll_timer_ = 0;
            state_ = State::deselected;
            show_map(pfrm, app.world_graph());
            update_storm_frontier(pfrm);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, storm_scroll_timer_);
            pfrm.set_scroll(Layer::map_1_ext, amount * 16, 0);
        }
        break;
    }


    case State::storm_scroll_in: {
        storm_scroll_timer_ += delta;
        constexpr auto fade_duration = milliseconds(1500);
        if (storm_scroll_timer_ > fade_duration) {
            storm_scroll_timer_ = 0;
            state_ = State::deselected;
            update_storm_frontier(pfrm);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, storm_scroll_timer_);
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


    case State::print_saved_text:
        pfrm.load_overlay_texture("overlay");
        heading_.emplace(pfrm, "progress saved...", OverlayCoord{1, 1});
        pfrm.screen().fade(1.f);
        state_ = State::show_saved_text;
        break;


    case State::show_saved_text:
        timer_ += delta;
        if (timer_ > milliseconds(1300)) {
            return scene_pool::alloc<TitleScreenScene>();
        }
        break;
    }



    return null_scene();
}



void WorldMapScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_priority(0);

    cursor.set_size(Sprite::Size::w16_h32);

    Vec2<s8> cursor_loc = app.world_graph().nodes_[cursor_].coord_;
    cursor_loc.x += 5;
    cursor_loc.y += 3;


    cursor.set_texture_index(28);
    cursor.set_position({Float((int)cursor_loc.x * 8) - 8,
                         Float((int)cursor_loc.y * 8) - 12});
    cursor.set_mix(cmix_);
    pfrm.screen().draw(cursor);


    cursor.set_mix({});



    if (state_ == State::explore_paths) {
        cursor.set_texture_index(15 + cursor_keyframe_);

        cursor.set_position(
            {Float(28 + cursor_loc.x * 24), Float(36 + cursor_loc.y * 24)});

        pfrm.screen().draw(cursor);
    } else if (state_ == State::move) {

        auto target = movement_targets_[movement_cursor_];
        cursor.set_texture_index(15 + cursor_keyframe_);
        cursor.set_position({
                (target.x + map_start_x) * Float(8) - 4,
                (target.y + map_start_y) * Float(8) - 4
            });
        pfrm.screen().draw(cursor);

        auto current = app.world_graph().nodes_[cursor_];
        auto x = (current.coord_.x + map_start_x) - 4;
        auto y = (current.coord_.y + map_start_y) - 4;
        cursor.set_texture_index(76);
        cursor.set_alpha(Sprite::Alpha::translucent);
        cursor.set_priority(2);

        for (int i = 0; i < 4; ++i) {
            cursor.set_position({Float(x) * 8 + i * 16, Float(y) * 8 + 32});
            pfrm.screen().draw(cursor);
            cursor.set_position({Float(x) * 8 + i * 16, Float(y) * 8});
            pfrm.screen().draw(cursor);
        }

        cursor.set_texture_index(74);
        for (int i = 0; i < 4; ++i) {
            cursor.set_position({Float(x) * 8 + i * 16, Float(y) * 8 + 64});
            pfrm.screen().draw(cursor);
        }

        cursor.set_texture_index(75);
        cursor.set_position({Float(x) * 8 + 64, Float(y) * 8});
        pfrm.screen().draw(cursor);
        cursor.set_position({Float(x) * 8 + 64, Float(y) * 8 + 32});
        pfrm.screen().draw(cursor);
        cursor.set_position({Float(x) * 8 + 64, Float(y) * 8 + 32 + 8});
        pfrm.screen().draw(cursor);


    } else if (state_ == State::save_selected or
               state_ == State::save_button_depressed or
               state_ == State::save_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position({208, 134});
        pfrm.screen().draw(cursor);
    } else if (state_ == State::help_selected or
               state_ == State::help_button_depressed or
               state_ == State::help_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position({184, 134});
        pfrm.screen().draw(cursor);
    }
}



void WorldMapScene::enter(Platform& pfrm, App& app, Scene& prev_scene)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    app.swap_player<PlayerP1>();

    app.effects().clear();
    app.player_island().projectiles().clear();

    // cursor_ = app.current_map_location(); // FIXME!!!!!!!!!!!!!!!!!!!!!!!

    auto view = pfrm.screen().get_view();
    view.set_center({});
    pfrm.screen().set_view(view);

    pfrm.load_overlay_texture("overlay_world_map");
    pfrm.load_tile1_texture("tilesheet_world_map_backdrop");

    pfrm.set_scroll(Layer::map_1_ext, 16, 0);
    draw_stormcloud_background(pfrm, app, storm_depth_);

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

    heading_->assign("sky map - zone ");
    heading_->append(app.zone());

    warning_.emplace(pfrm, OverlayCoord{1, 18});
    warning_->assign("storm >>");

    show_map(pfrm, app.world_graph());

    // key_[0].emplace(pfrm, OverlayCoord{3, 13});
    // key_[0]->assign("neutral");
    // key_[1].emplace(pfrm, OverlayCoord{3, 15});
    // key_[1]->assign("hostile");
    // key_[2].emplace(pfrm, OverlayCoord{3, 17});
    // key_[2]->assign("uncharted");

    // pfrm.set_tile(Layer::overlay, 1, 13, 115);
    // pfrm.set_tile(Layer::overlay, 1, 15, 118);
    // pfrm.set_tile(Layer::overlay, 1, 17, 116);

    save_icon_.emplace(pfrm, 126, OverlayCoord{27, 17});
    help_icon_.emplace(pfrm, 134, OverlayCoord{24, 17});

    for (auto& node : app.world_graph().nodes_) {
        if (node.type_ == WorldGraph::Node::Type::exit) {
            exit_label_.emplace(pfrm, "exit", OverlayCoord{
                    u8(node.coord_.x + map_start_x),
                    u8(node.coord_.y + map_start_y - 1)
                });
        }
    }


}



void WorldMapScene::show_map(Platform& pfrm, WorldGraph& map)
{
    for (auto& node : map.nodes_) {
        if ((map_start_x + node.coord_.x) * 8 < (storm_depth_ + 1) * 16) {

            if (node.type_ == WorldGraph::Node::Type::exit) {
                exit_label_.reset();
            }
            node.type_ = WorldGraph::Node::Type::corrupted;

            pfrm.set_tile(Layer::overlay,
                      map_start_x + node.coord_.x,
                      map_start_y + node.coord_.y, 98);

        } else {

            pfrm.set_tile(Layer::overlay,
                          map_start_x + node.coord_.x,
                          map_start_y + node.coord_.y,
                          114 + (int)node.type_);
        }

    }
}



void WorldMapScene::exit(Platform& pfrm, App&, Scene& next_scene)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    pfrm.fill_overlay(0);

    save_icon_.reset();
    help_icon_.reset();
    heading_.reset();

    pfrm.load_overlay_texture("overlay");

    pfrm.load_tile1_texture("tilesheet_enemy_0");
}



} // namespace skyland
