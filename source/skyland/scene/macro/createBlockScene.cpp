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


#include "createBlockScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



s8 CreateBlockScene::selector_;
static macro::terrain::Type last_created = terrain::Type::terrain;



void CreateBlockScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    MacrocosmScene::enter(pfrm, app, prev);

    collect_options(*app.macrocosm());

    for (u32 i = 0; i < options_.size(); ++i) {
        if (options_[i] == last_created) {
            selector_ = i;
            break;
        }
    }

    show_options(pfrm, *app.macrocosm());
}



void CreateBlockScene::collect_options(macro::State& state)
{
    options_.push_back(terrain::Type::terrain);
    options_.push_back(terrain::Type::building);
    options_.push_back(terrain::Type::water);
    options_.push_back(terrain::Type::gold);
    options_.push_back(terrain::Type::light_source);
    options_.push_back(terrain::Type::masonry);
    options_.push_back(terrain::Type::air);
}



void CreateBlockScene::exit(Platform& pfrm, App& app, Scene& next)
{
    MacrocosmScene::exit(pfrm, app, next);
    pfrm.fill_overlay(0);
}



Coins CreateBlockScene::cost(macro::State& state, terrain::Type t)
{
    return terrain::cost(state.sector(), t);
}



void CreateBlockScene::show_options(Platform& pfrm, State& state)
{
    auto st = calc_screen_tiles(pfrm);

    StringBuffer<30> message = SYSTR(construction_build)->c_str();
    message += " ";
    message += loadstr(pfrm, terrain::name(options_[selector_]))->c_str();
    message += " ";
    message += stringify(cost(state, options_[selector_]));
    message += "@";

    Text text(pfrm, OverlayCoord{0, u8(st.y - 1)});
    text.assign(message.c_str());

    const int count = st.x - text.len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text.len(), st.y - 1, 426);
    }

    text.__detach();

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
        pfrm.set_tile(Layer::overlay, i, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 5, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 0);
    }

    for (int i = st.x - 25; i < st.x - 5; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        pfrm.set_tile(Layer::overlay, st.x - 26, y, 128);
        pfrm.set_tile(Layer::overlay, st.x - 5, y, 433);
    }

    pfrm.set_tile(Layer::overlay, st.x - 26, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 5, st.y - 2, 418);

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
        draw_image(pfrm, 258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = selector_;
        if (index - 1 < 0) {
            index = opt_count - 1;
        } else {
            index = index - 1;
        }

        auto icon = terrain::icons(options_[index]).second;
        draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = terrain::icons(options_[selector_]).first;
        draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(197, icon, 16);
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
        draw_image(pfrm, 213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(213, icon, 16);
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
        draw_image(pfrm, 274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(274, icon, 16);
    }
}



ScenePtr<Scene>
CreateBlockScene::update(Platform& pfrm, Player& player, macro::State& state)
{
    if (auto next = MacrocosmScene::update(pfrm, player, state)) {
        return next;
    }

    if (player.key_down(pfrm, Key::right)) {
        if (selector_ < (int)options_.size() - 1) {
            ++selector_;
        } else {
            selector_ = 0;
        }
        show_options(pfrm, state);
        pfrm.speaker().play_sound("click", 1);
    }

    if (player.key_down(pfrm, Key::left)) {
        if (selector_ > 0) {
            --selector_;
        } else {
            selector_ = options_.size() - 1;
        }
        show_options(pfrm, state);
        pfrm.speaker().play_sound("click", 1);
    }

    if (player.key_down(pfrm, Key::action_1)) {
        auto cursor = state.sector().cursor();
        if (cursor.z < macro::terrain::Sector::z_limit - 1) {
            auto cost = this->cost(state, options_[selector_]);
            if (cost > state.data_->p().coins_.get()) {
                pfrm.speaker().play_sound("beep_error", 2);
                return null_scene();
            } else {
                state.data_->p().coins_.set(state.data_->p().coins_.get() -
                                            cost);
            }

            edit(state, options_[selector_]);
            state.sector().update();

            if (options_[selector_] not_eq terrain::Type::air) {
                pfrm.speaker().play_sound("build0", 4);
                return scene_pool::alloc<SelectorScene>();
            } else {
                pfrm.speaker().play_sound("cursor_tick", 2);
            }
        }
    }

    if (player.key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<SelectorScene>();
    }

    return null_scene();
}



void CreateBlockScene::edit(macro::State& state, terrain::Type t)
{
    auto cursor = state.sector().cursor();

    if (t not_eq terrain::Type::air) {
        state.sector().set_block(cursor, t);
    }

    ++cursor.z;
    auto block = state.sector().get_block(cursor);
    while (block.type_ not_eq (u8) terrain::Type::air) {
        ++cursor.z;
        block = state.sector().get_block(cursor);
    }
    state.sector().set_cursor(cursor, false);

    last_created = options_[selector_];
}



Coins BuildImprovementScene::cost(macro::State& state, terrain::Type t)
{
    const auto base_cost = terrain::cost(state.sector(), t);

    if (t == terrain::Type::terrain) {
        // The player's just clearing land to plant something else, don't levy
        // excessive costs for doing so.
        return base_cost / 4;
    }

    if (terrain::category(t) not_eq terrain::Category::crop) {
        return base_cost;
    }

    // Cheaper to build a crop next to an existing crop of the same type.

    int cost = base_cost;

    auto cursor = state.sector().cursor();
    --cursor.z;

    if (cursor.x > 0) {
        auto temp = cursor;
        --temp.x;
        auto& block = state.sector().get_block(temp);
        if (block.type() == t) {
            cost -= base_cost / 3;
        }
    }

    if (cursor.x < 7) {
        auto temp = cursor;
        ++temp.x;
        auto& block = state.sector().get_block(temp);
        if (block.type() == t) {
            cost -= base_cost / 3;
        }
    }

    if (cursor.y > 0) {
        auto temp = cursor;
        --temp.y;
        auto& block = state.sector().get_block(temp);
        if (block.type() == t) {
            cost -= base_cost / 3;
        }
    }

    if (cursor.y < 7) {
        auto temp = cursor;
        ++temp.y;
        auto& block = state.sector().get_block(temp);
        if (block.type() == t) {
            cost -= base_cost / 3;
        }
    }

    if (cost < 0) {
        cost = 0;
    }

    return cost;
}



void BuildImprovementScene::collect_options(macro::State& state)
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



void BuildImprovementScene::edit(macro::State& state, terrain::Type t)
{
    auto cursor = state.sector().cursor();

    cursor.z--;

    if (t not_eq terrain::Type::air) {
        state.sector().set_block(cursor, t);
    }
}



} // namespace skyland::macro
