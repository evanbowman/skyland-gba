#include "worldMapScene.hpp"
#include "graphics/overlay.hpp"
#include "loadLevelScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/worldMap.hpp"
#include "skyland/save.hpp"
#include "titleScreenScene.hpp"



namespace skyland {



ScenePtr<Scene>
WorldMapScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_keyframe_ = not cursor_keyframe_;
    }


    auto& node = app.world_map().matrix_[cursor_.x][cursor_.y];


    switch (state_) {
    case State::deselected:
        if (pfrm.keyboard()
                .down_transition<Key::right,
                                 Key::left,
                                 Key::up,
                                 Key::down,
                                 Key::action_1>()) {
            state_ = State::explore_paths;
        }
        break;


    case State::explore_paths:
        if (pfrm.keyboard().down_transition<Key::action_2>()) {
            state_ = State::deselected;
            cursor_ = app.current_map_location();
            show_map(pfrm, app.world_map());
            break;
        }
        if (pfrm.keyboard().down_transition<Key::action_1>()) {
            if (cursor_ == app.current_map_location()) {
                state_ = State::move;
                show_move_arrows(pfrm, app);
            }
        }
        if (pfrm.keyboard().down_transition<Key::right>() and
            node.connections_.mask_ & WorldMap::Node::Connections::r) {
            cursor_.x += 1;
            show_map(pfrm, app.world_map());
        } else if (pfrm.keyboard().down_transition<Key::left>() and
                   node.connections_.mask_ & WorldMap::Node::Connections::l) {
            cursor_.x -= 1;
            show_map(pfrm, app.world_map());
        } else if (pfrm.keyboard().down_transition<Key::up>()) {
            if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
                cursor_.x += 1;
                cursor_.y -= 1;
                show_map(pfrm, app.world_map());
            } else if (node.connections_.mask_ &
                       WorldMap::Node::Connections::lu) {
                cursor_.x -= 1;
                cursor_.y -= 1;
                show_map(pfrm, app.world_map());
            }
        } else if (pfrm.keyboard().down_transition<Key::down>()) {
            if (node.connections_.mask_ & WorldMap::Node::Connections::ld) {
                cursor_.x -= 1;
                cursor_.y += 1;
                show_map(pfrm, app.world_map());
            } else if (node.connections_.mask_ &
                       WorldMap::Node::Connections::rd) {
                cursor_.x += 1;
                cursor_.y += 1;
                show_map(pfrm, app.world_map());
            } else {
                state_ = State::save_selected;
                const auto cached_cursor = cursor_;
                cursor_ = app.current_map_location(); // needed for show_map
                show_map(pfrm, app.world_map());
                cursor_ = cached_cursor;
            }
        }
        break;


    case State::save_selected:
        if (pfrm.keyboard().down_transition<Key::up>() or
            pfrm.keyboard().down_transition<Key::action_2>()) {
            state_ = State::explore_paths;
            show_map(pfrm, app.world_map());
        }

        if (pfrm.keyboard().down_transition<Key::action_1>()) {
            state_ = State::save_button_depressed;
            save_icon_.emplace(pfrm, 124, OverlayCoord{26, 16});
            timer_ = 0;
        }
        break;


    case State::save_button_depressed:
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            timer_ = 0;
            state_ = State::save_button_released_wait;
            save_icon_.emplace(pfrm, 120, OverlayCoord{26, 16});
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
        if (cmix_.amount_ > 0) {
            timer_ += delta;
            if (timer_ > 12000) {
                timer_ -= 12000;
                cmix_ = {cmix_.color_, u8(cmix_.amount_ - 5)};
            }
        } else {
            timer_ = 0;
            cmix_ = {ColorConstant::silver_white, 200};
        }
    }


        if (pfrm.keyboard().down_transition<Key::action_2>()) {
            state_ = State::explore_paths;
            cursor_ = app.current_map_location();
            show_map(pfrm, app.world_map());
            cmix_ = {};
            break;
        }
        if (pfrm.keyboard().down_transition<Key::up>() and
            node.connections_.mask_ & WorldMap::Node::Connections::ru) {
            for (int i = 0; i < 3; ++i) {
                move_arrow_sel_[i] = false;
            }
            move_arrow_sel_[0] = true;
            show_move_arrows(pfrm, app);
        }
        if (pfrm.keyboard().down_transition<Key::right>() and
            node.connections_.mask_ & WorldMap::Node::Connections::r) {
            for (int i = 0; i < 3; ++i) {
                move_arrow_sel_[i] = false;
            }
            move_arrow_sel_[1] = true;
            show_move_arrows(pfrm, app);
        }
        if (pfrm.keyboard().down_transition<Key::down>() and
            node.connections_.mask_ & WorldMap::Node::Connections::rd) {
            for (int i = 0; i < 3; ++i) {
                move_arrow_sel_[i] = false;
            }
            move_arrow_sel_[2] = true;
            show_move_arrows(pfrm, app);
        }

        if (pfrm.keyboard().down_transition<Key::action_1>()) {
            state_ = State::wait;
            cmix_ = {ColorConstant::stil_de_grain, 200};
            if (move_arrow_sel_[0]) {
                app.current_map_location().x += 1;
                app.current_map_location().y -= 1;
            } else if (move_arrow_sel_[1]) {
                app.current_map_location().x += 1;
            } else if (move_arrow_sel_[2]) {
                app.current_map_location().x += 1;
                app.current_map_location().y += 1;
            }
        }
        break;

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
            state_ = State::deselected;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::fade_out_saved: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1200);
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::print_saved_text;
            save::store(pfrm, app.persistent_data());
            pfrm.fill_overlay(0);
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



void WorldMapScene::show_move_arrows(Platform& pfrm, App& app)
{
    auto& node = app.world_map().matrix_[cursor_.x][cursor_.y];

    if (move_arrow_sel_[0]) {
        cursor_ = app.current_map_location();
        cursor_.x += 1;
        cursor_.y -= 1;
        show_map(pfrm, app.world_map());
        cursor_ = app.current_map_location();

        pfrm.set_tile(Layer::overlay,
                      4 + cursor_.x * 3,
                      5 + cursor_.y * 3,
                      114 + node.type_);

        pfrm.set_tile(
            Layer::overlay, 5 + cursor_.x * 3, 4 + cursor_.y * 3, 103);
        pfrm.set_tile(
            Layer::overlay, 6 + cursor_.x * 3, 3 + cursor_.y * 3, 103);

    } else if (move_arrow_sel_[1]) {
        cursor_ = app.current_map_location();
        cursor_.x += 1;
        show_map(pfrm, app.world_map());
        cursor_ = app.current_map_location();

        pfrm.set_tile(Layer::overlay,
                      4 + cursor_.x * 3,
                      5 + cursor_.y * 3,
                      114 + node.type_);

        pfrm.set_tile(
            Layer::overlay, 5 + cursor_.x * 3, 5 + cursor_.y * 3, 102);
        pfrm.set_tile(
            Layer::overlay, 6 + cursor_.x * 3, 5 + cursor_.y * 3, 102);

    } else if (move_arrow_sel_[2]) {
        cursor_ = app.current_map_location();
        cursor_.x += 1;
        cursor_.y += 1;
        show_map(pfrm, app.world_map());
        cursor_ = app.current_map_location();

        pfrm.set_tile(Layer::overlay,
                      4 + cursor_.x * 3,
                      5 + cursor_.y * 3,
                      114 + node.type_);

        pfrm.set_tile(
            Layer::overlay, 5 + cursor_.x * 3, 6 + cursor_.y * 3, 104);
        pfrm.set_tile(
            Layer::overlay, 6 + cursor_.x * 3, 7 + cursor_.y * 3, 104);
    }
}



void WorldMapScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_priority(0);

    cursor.set_size(Sprite::Size::w16_h32);

    cursor.set_texture_index(28);
    cursor.set_position({Float(28 + app.current_map_location().x * 24) - 4,
                         Float(36 + app.current_map_location().y * 24) - 8});
    cursor.set_mix(cmix_);
    pfrm.screen().draw(cursor);


    cursor.set_mix({});


    if (state_ == State::explore_paths) {
        cursor.set_texture_index(15 + cursor_keyframe_);

        cursor.set_position(
            {Float(28 + cursor_.x * 24), Float(36 + cursor_.y * 24)});

        pfrm.screen().draw(cursor);
    } else if (state_ == State::move) {
        auto& node = app.world_map().matrix_[cursor_.x][cursor_.y];

        if (node.connections_.mask_ & WorldMap::Node::Connections::r) {
            cursor.set_texture_index(34 - 3 * move_arrow_sel_[1]);
            cursor.set_position({Float(28 + cursor_.x * 24) + 17,
                                 Float(36 + cursor_.y * 24) - 1});
            pfrm.screen().draw(cursor);
        }

        if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
            cursor.set_texture_index(32 - 3 * move_arrow_sel_[0]);
            cursor.set_position({Float(28 + cursor_.x * 24) + 11,
                                 Float(36 + cursor_.y * 24) - 13});
            pfrm.screen().draw(cursor);
        }

        if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
            cursor.set_texture_index(33 - 3 * move_arrow_sel_[2]);
            cursor.set_position({Float(28 + cursor_.x * 24) + 11,
                                 Float(36 + cursor_.y * 24) + 13});
            pfrm.screen().draw(cursor);
        }
    } else if (state_ == State::save_selected or
               state_ == State::save_button_depressed or
               state_ == State::save_button_released_wait) {
        cursor.set_size(Sprite::Size::w32_h32);
        cursor.set_texture_index(26 + cursor_keyframe_);
        cursor.set_position({200, 120});
        pfrm.screen().draw(cursor);
    }
}



void WorldMapScene::enter(Platform& pfrm, App& app, Scene& prev_scene)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    app.effects().clear();
    app.player_island().projectiles().clear();

    cursor_ = app.current_map_location();

    auto view = pfrm.screen().get_view();
    view.set_center({});
    pfrm.screen().set_view(view);

    pfrm.load_overlay_texture("overlay_world_map");

    pfrm.enable_glyph_mode(true);

    auto st = calc_screen_tiles(pfrm);

    for (int x = 0; x < st.x; ++x) {
        for (int y = 0; y < st.y; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 88);
        }
    }

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

    show_map(pfrm, app.world_map());

    key_[0].emplace(pfrm, OverlayCoord{3, 13});
    key_[0]->assign("neutral");
    key_[1].emplace(pfrm, OverlayCoord{3, 15});
    key_[1]->assign("hostile");
    key_[2].emplace(pfrm, OverlayCoord{3, 17});
    key_[2]->assign("uncharted");

    pfrm.set_tile(Layer::overlay, 1, 13, 115);
    pfrm.set_tile(Layer::overlay, 1, 15, 118);
    pfrm.set_tile(Layer::overlay, 1, 17, 116);

    auto& node = app.world_map().matrix_[cursor_.x][cursor_.y];
    if (not(node.connections_.mask_ & WorldMap::Node::Connections::r)) {
        move_arrow_sel_[1] = false;
        if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
            move_arrow_sel_[0] = true;
        } else if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
            move_arrow_sel_[2] = true;
        }
    }

    save_icon_.emplace(pfrm, 120, OverlayCoord{26, 16});

    pfrm.screen().fade(0.f);
}



static void
highlight_subtree(Platform& pfrm, WorldMap& map, const Vec2<u8>& coord)
{
    auto& node = map.matrix_[coord.x][coord.y];
    pfrm.set_tile(
        Layer::overlay, 4 + coord.x * 3, 5 + coord.y * 3, 114 + node.type_);


    if (node.connections_.mask_ & WorldMap::Node::Connections::r) {
        highlight_subtree(pfrm, map, {u8(coord.x + 1), coord.y});
        pfrm.set_tile(Layer::overlay, 5 + coord.x * 3, 5 + coord.y * 3, 102);
        pfrm.set_tile(Layer::overlay, 6 + coord.x * 3, 5 + coord.y * 3, 102);
    }

    if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
        highlight_subtree(pfrm, map, {u8(coord.x + 1), u8(coord.y - 1)});
        pfrm.set_tile(Layer::overlay, 5 + coord.x * 3, 4 + coord.y * 3, 103);
        pfrm.set_tile(Layer::overlay, 6 + coord.x * 3, 3 + coord.y * 3, 103);
    }

    if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
        highlight_subtree(pfrm, map, {u8(coord.x + 1), u8(coord.y + 1)});
        pfrm.set_tile(Layer::overlay, 5 + coord.x * 3, 6 + coord.y * 3, 104);
        pfrm.set_tile(Layer::overlay, 6 + coord.x * 3, 7 + coord.y * 3, 104);
    }
}



void WorldMapScene::update_tree(Platform& pfrm, WorldMap& map)
{
    highlight_subtree(pfrm, map, cursor_);
}



void WorldMapScene::show_map(Platform& pfrm, WorldMap& map)
{
    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 3; ++y) {
            auto& node = map.matrix_[x][y];

            pfrm.set_tile(
                Layer::overlay, 4 + x * 3, 5 + y * 3, 97 + node.type_);

            if (node.connections_.mask_ & WorldMap::Node::Connections::l) {
                pfrm.set_tile(Layer::overlay, 3 + x * 3, 5 + y * 3, 85);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::r) {
                pfrm.set_tile(Layer::overlay, 5 + x * 3, 5 + y * 3, 85);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::ru) {
                pfrm.set_tile(Layer::overlay, 5 + x * 3, 4 + y * 3, 86);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::rd) {
                pfrm.set_tile(Layer::overlay, 5 + x * 3, 6 + y * 3, 87);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::lu) {
                pfrm.set_tile(Layer::overlay, 3 + x * 3, 4 + y * 3, 87);
            }
            if (node.connections_.mask_ & WorldMap::Node::Connections::ld) {
                pfrm.set_tile(Layer::overlay, 3 + x * 3, 6 + y * 3, 86);
            }
        }
    }
    update_tree(pfrm, map);
}



void WorldMapScene::exit(Platform& pfrm, App&, Scene& next_scene)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    pfrm.fill_overlay(0);

    save_icon_.reset();
    heading_.reset();

    key_[0].reset();
    key_[1].reset();
    key_[2].reset();

    pfrm.load_overlay_texture("overlay");
}



} // namespace skyland
