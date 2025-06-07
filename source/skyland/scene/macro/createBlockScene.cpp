////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "createBlockScene.hpp"
#include "selectorScene.hpp"
#include "skyland/entity/macro/macrocosmEffect.hpp"
#include "skyland/network.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



s8 CreateBlockScene::selector_;



void CreateBlockScene::init_cursor(macro::EngineImpl& state)
{
    for (u32 i = 0; i < options_.size(); ++i) {
        if (options_[i] == state.data_->last_created_) {
            selector_ = i;
            break;
        }
    }
}



void BuildImprovementScene::init_cursor(macro::EngineImpl& state)
{
    for (u32 i = 0; i < options_.size(); ++i) {
        if (options_[i] == state.data_->last_improved_) {
            selector_ = i;
            break;
        }
    }
}



void CreateBlockScene::enter(macro::EngineImpl& state, Scene& prev)
{
    MacrocosmScene::enter(state, prev);

    collect_options(state);

    init_cursor(state);

    show_options(state);
}



void CreateBlockScene::collect_options(macro::EngineImpl& state)
{
    options_.push_back(terrain::Type::terrain);
    options_.push_back(terrain::Type::building);
    if (state.sector().cursor().z > 0) {
        auto cursor = state.sector().cursor();
        --cursor.z;
        auto& block = state.sector().get_block(cursor);
        if (block.type() == terrain::Type::building or
            block.type() == terrain::Type::workshop) {
            options_.push_back(terrain::Type::dome);
        }
    }
    options_.push_back(terrain::Type::granary);
    // options_.push_back(terrain::Type::workshop);
    options_.push_back(terrain::Type::water_source);

    if (state.data_->freebuild_mode_) {
        options_.push_back(terrain::Type::ice);
        options_.push_back(terrain::Type::shrubbery);
        options_.push_back(terrain::Type::lava_source);
    }

    // if (not state.data_->freebuild_mode_ and
    //     not state.data_->other_sectors_.empty()) {
    //     auto stats = state.sector().base_stats();
    //     if (not stats.commodities_.empty() and state.sector().exports()) {
    //         options_.push_back(terrain::Type::port);
    //     }
    // }

    if (state.sector().cursor().z > 0) {
        auto cursor = state.sector().cursor();
        --cursor.z;
        auto& block = state.sector().get_block(cursor);
        if (block.type() == terrain::Type::terrain or
            (block.type() not_eq terrain::Type::lumber and
             (terrain::categories(block.type()) & terrain::Categories::crop) and
             not(terrain::categories(block.type()) &
                 terrain::Categories::fluid_water))) {
            // options_.push_back(terrain::Type::lumber);
        }
    }

    // options_.push_back(terrain::Type::gold);
    options_.push_back(terrain::Type::crystal);
    if (state.data_->freebuild_mode_) {
        options_.push_back(terrain::Type::ocher);
        options_.push_back(terrain::Type::hematite);
    }

    options_.push_back(terrain::Type::basalt);

    options_.push_back(terrain::Type::light_source);
    options_.push_back(terrain::Type::sand);
    options_.push_back(terrain::Type::marble_top);
    options_.push_back(terrain::Type::scaffolding);
    if (state.data_->freebuild_mode_) {
        options_.push_back(terrain::Type::hull);
    }
    options_.push_back(terrain::Type::dynamite);
    options_.push_back(terrain::Type::arch);
    options_.push_back(terrain::Type::masonry);
    options_.push_back(terrain::Type::air);
}



void CreateBlockScene::exit(macro::EngineImpl& state, Scene& next)
{
    MacrocosmScene::exit(state, next);

    const auto st = calc_screen_tiles();
    for (int y = st.y - 8; y < st.y; ++y) {
        for (int x = 0; x < 32; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }
}



terrain::Cost CreateBlockScene::cost(macro::EngineImpl& state, terrain::Type t)
{
    if (state.data_->freebuild_mode_) {
        auto result = terrain::Cost{};
        result.productivity_ = 0;
        return result;
    }
    return terrain::cost(t);
}



void render_cost(macro::EngineImpl& state,
                 terrain::Type t,
                 Text& text,
                 bool harvest,
                 Text::OptColors text_colors = {},
                 Optional<terrain::Cost> inp_c = {})
{
    auto st = calc_screen_tiles();

    StringBuffer<30> message;

    text.append(message.c_str());

    if (not state.data_->freebuild_mode_) {

        auto c = terrain::cost(t);
        if (harvest) {
            c = terrain::harvest(t).first;
        }
        if (inp_c) {
            c = *inp_c;
        }

        if (c.productivity_) {
            text.append(" -", text_colors);
            text.append(c.productivity_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 415);
        }

        if (c.stone_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.stone_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 417);
        }

        if (c.lumber_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.lumber_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 423);
        }

        if (c.clay_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.clay_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 370);
        }

        if (c.water_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.water_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 371);
        }

        if (c.marble_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.marble_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 372);
        }

        if (c.crystal_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.crystal_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 424);
        }

        if (c.food_) {
            text.append(" ");
            if (not harvest) {
                text.append("-", text_colors);
            } else {
                text.append("+", text_colors);
            }
            text.append(c.food_, text_colors);
            text.append(" ");
            PLATFORM.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 414);
        }
    }

    const int count = st.x - text.len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text.len(), st.y - 1, 426);
    }
}



void CreateBlockScene::message(macro::EngineImpl& state)
{
    auto st = calc_screen_tiles();

    Text text(OverlayCoord{0, u8(st.y - 1)});
    text.append(":");
    text.append(loadstr(terrain::name(options_[selector_]))->c_str());
    render_cost(state, options_[selector_], text, false);
    text.__detach();
}



void CreateBlockScene::show_options(EngineImpl& state)
{
    auto st = calc_screen_tiles();

    message(state);

    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 3, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 5, 0);
        PLATFORM.set_tile(Layer::overlay, i, st.y - 6, 0);
    }

    for (int i = st.x - 25; i < st.x - 5; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        PLATFORM.set_tile(Layer::overlay, st.x - 26, y, 130);
        PLATFORM.set_tile(Layer::overlay, st.x - 5, y, 433);
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 26, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, st.x - 5, st.y - 2, 418);

    int opt_count = options_.size();

    {
        int index = selector_;
        if (index - 2 < -1) {
            index = opt_count - 2;
        } else if (index - 2 < 0) {
            index = opt_count - 1;
        } else {
            index = index - 2;
        }

        auto icon = terrain::icons(options_[index]).second;
        draw_image(258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = selector_;
        if (index - 1 < 0) {
            index = opt_count - 1;
        } else {
            index = index - 1;
        }

        auto icon = terrain::icons(options_[index]).second;
        draw_image(181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = terrain::icons(options_[selector_]).first;
        draw_image(197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)opt_count) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon = terrain::icons(options_[index]).second;
        ;
        draw_image(213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(213, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)opt_count) {
            index = 1;
        } else if (index + 2 >= (int)opt_count) {
            index = 0;
        } else {
            index = index + 2;
        }

        auto icon = terrain::icons(options_[index]).second;
        draw_image(274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(274, icon, 16);
    }
}



void CreateBlockScene::adjust_cursor_z(Player& player, macro::EngineImpl& state)
{
    if (player.key_down(Key::up)) {
        auto cursor = state.sector().cursor();
        if (cursor.z < state.sector().size().z - 1) {
            cursor.z++;
            while (state.sector().get_block(cursor).type() not_eq
                   terrain::Type::air) {
                if (cursor.z == state.sector().size().z - 1) {
                    PLATFORM.speaker().play_sound("beep_error", 2);
                    return;
                }
                cursor.z++;
            }
            auto& above = state.sector().get_block(cursor);
            if (above.type() == terrain::Type::air) {
                state.sector().set_cursor(cursor, false);
                PLATFORM.speaker().play_sound("click", 1);
            }
        }
    }

    if (player.key_down(Key::down)) {
        auto cursor = state.sector().cursor();
        if (cursor.z not_eq 0) {
            cursor.z--;
            while (state.sector().get_block(cursor).type() not_eq
                   terrain::Type::air) {
                if (cursor.z == 0) {
                    PLATFORM.speaker().play_sound("beep_error", 2);
                    return;
                }
                cursor.z--;
            }
            auto& beneath = state.sector().get_block(cursor);
            if (beneath.type() == terrain::Type::air) {
                state.sector().set_cursor(cursor, false);
                PLATFORM.speaker().play_sound("click", 1);
            }
        }
    }
}



ScenePtr CreateBlockScene::update(Player& player, macro::EngineImpl& state)
{
    if (auto next = MacrocosmScene::update(player, state)) {
        return next;
    }

    if (player.key_pressed(Key::alt_1)) {
        // ...
    } else {

        // adjust_cursor_z(player, state);

        if (player.key_down(Key::right)) {
            if (selector_ < (int)options_.size() - 1) {
                ++selector_;
            } else {
                selector_ = 0;
            }
            show_options(state);
            PLATFORM.speaker().play_sound("click", 1);
        }

        if (player.key_down(Key::left)) {
            if (selector_ > 0) {
                --selector_;
            } else {
                selector_ = options_.size() - 1;
            }
            show_options(state);
            PLATFORM.speaker().play_sound("click", 1);
        }
    }


    if (player.key_down(Key::action_1)) {
        return onclick(state);
    }

    if (player.key_down(Key::action_2)) {
        return make_scene<SelectorScene>();
    }

    return null_scene();
}



class InsufficentResourcesScene : public MacrocosmScene
{
public:
    InsufficentResourcesScene(macro::EngineImpl& state, terrain::Cost c)
    {
        auto& p = state.data_->p();
#define ASSIGN(NAME)                                                           \
    deficit_.NAME = c.NAME - p.NAME.get();                                     \
    if (deficit_.NAME <= 0) {                                                  \
        deficit_.NAME = 0;                                                     \
    }

        ASSIGN(stone_);
        ASSIGN(lumber_);
        ASSIGN(clay_);
        ASSIGN(crystal_);
        ASSIGN(water_);

        int prod = (c.productivity_) - state.sector().productivity();
        deficit_.productivity_ = 0;
        if (prod > 0) {
            deficit_.productivity_ = prod;
        }
    }


    void enter(macro::EngineImpl& state, Scene& prev) override
    {
        text_.emplace(OverlayCoord{0, 19});

        render_cost(state,
                    terrain::Type::air,
                    *text_,
                    false,
                    {{custom_color(0xe66428), ColorConstant::rich_black}},
                    deficit_);
    }


    void exit(macro::EngineImpl& state, Scene& next) override
    {
        text_.reset();
        PLATFORM.fill_overlay(0);
    }


    ScenePtr update(Player& player, macro::EngineImpl& state) override
    {
        if (key_down<Key::action_1>() or key_down<Key::action_2>()) {
            return make_scene<SelectorScene>();
        }

        return null_scene();
    }


private:
    terrain::Cost deficit_;
    Optional<Text> text_;
};



ScenePtr CreateBlockScene::onclick(macro::EngineImpl& state)
{
    auto cursor = state.sector().cursor();
    if (not check_z() or cursor.z < state.sector().size().z - 1) {

        auto cost = this->cost(state, options_[selector_]);

        auto& p = state.data_->p();
        if (not state.data_->freebuild_mode_ and
            (cost.stone_ > p.stone_.get() or cost.lumber_ > p.lumber_.get() or
             cost.water_ > p.water_.get() or cost.crystal_ > p.crystal_.get() or
             cost.clay_ > p.clay_.get() or cost.water_ > p.water_.get() or
             cost.productivity_ > state.sector().productivity())) {
            PLATFORM.speaker().play_sound("beep_error", 2);
            return make_scene<InsufficentResourcesScene>(state, cost);
        } else if (not state.data_->freebuild_mode_) {
            auto prod = state.sector().productivity();
            prod -= cost.productivity_;
            state.sector().set_productivity(prod);
            auto& p = state.data_->p();
            p.stone_.set(p.stone_.get() - cost.stone_);
            p.lumber_.set(p.lumber_.get() - cost.lumber_);
            p.clay_.set(p.clay_.get() - cost.clay_);
            p.crystal_.set(p.crystal_.get() - cost.crystal_);
            p.water_.set(p.water_.get() - cost.water_);
        }

        edit(state, options_[selector_]);
        state.sector().update();
        update_ui_on_exit();

        if (options_[selector_] not_eq terrain::Type::air) {
            PLATFORM.speaker().play_sound("build0", 4);

            return make_scene<SelectorScene>();

        } else {
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }
    } else {
        PLATFORM.speaker().play_sound("beep_error", 2);
    }

    return null_scene();
}



void CreateBlockScene::edit(macro::EngineImpl& state, terrain::Type t)
{
    auto cursor = state.sector().cursor();

    if (t not_eq terrain::Type::air) {
        state.sector().set_block(cursor, t);
        if (state.data_->freebuild_mode_) {
            network::packet::MacroSetBlock p;
            p.x_ = cursor.x;
            p.y_ = cursor.y;
            p.z_ = cursor.z;
            p.rot_ = (u8)state.sector().persistent().orientation_;
            p.type_ = (u8)t;

            network::transmit(p);
        }

        auto pos = screen_coord(state.sector().cursor_raster_pos());

        state.add_entity<MacrocosmEffect>(pos, 8, 13, milliseconds(80));
    }

    ++cursor.z;
    auto block = state.sector().get_block(cursor);
    while (block.type_ not_eq (u8) terrain::Type::air) {
        ++cursor.z;
        block = state.sector().get_block(cursor);
    }
    state.sector().set_cursor(cursor, false);

    state.data_->last_created_ = options_[selector_];
}



terrain::Cost BuildImprovementScene::cost(macro::EngineImpl& state,
                                          terrain::Type t)
{
    if (state.data_->freebuild_mode_) {
        return terrain::Cost{};
    }

    return terrain::cost(t);
}



void BuildImprovementScene::collect_options(macro::EngineImpl& state)
{
    auto& sector = state.sector();

    auto cursor = sector.cursor();
    if (cursor.z == 0) {
        Platform::fatal("logic error: build improvement cursor too low");
    }

    --cursor.z;
    auto improvements = sector.get_block(cursor).improvements();

    for (auto improvement : improvements) {
        options_.push_back(improvement);
    }

    selector_ = 0;
}



void BuildImprovementScene::edit(macro::EngineImpl& state, terrain::Type t)
{
    auto cursor = state.sector().cursor();

    cursor.z--;

    if (t not_eq terrain::Type::air) {
        state.sector().set_block(cursor, t);

        if (state.data_->freebuild_mode_) {
            network::packet::MacroSetBlock p;
            p.x_ = cursor.x;
            p.y_ = cursor.y;
            p.z_ = cursor.z;
            p.rot_ = (u8)state.sector().persistent().orientation_;
            p.type_ = (u8)t;

            network::transmit(p);
        }

        auto pos = screen_coord(state.sector().cursor_raster_pos());
        pos.y += 4.0_fixed;

        state.add_entity<MacrocosmEffect>(pos, 8, 13, milliseconds(80));
    }

    state.data_->last_improved_ = options_[selector_];
}



} // namespace skyland::macro
