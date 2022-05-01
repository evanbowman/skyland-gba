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



void CreateBlockScene::enter(Platform& pfrm, macro::State& state, Scene& prev)
{
    MacrocosmScene::enter(pfrm, state, prev);

    collect_options(state);

    for (u32 i = 0; i < options_.size(); ++i) {
        if (options_[i] == last_created) {
            selector_ = i;
            break;
        }
    }

    show_options(pfrm, state);
}



void CreateBlockScene::collect_options(macro::State& state)
{
    options_.push_back(terrain::Type::terrain);
    options_.push_back(terrain::Type::building);
    options_.push_back(terrain::Type::water);
    options_.push_back(terrain::Type::shrubbery);

    if (not state.data_->other_sectors_.empty()) {
        auto stats = state.sector().base_stats();
        if (not stats.commodities_.empty()) {
            options_.push_back(terrain::Type::port);
        }
    }

    options_.push_back(terrain::Type::gold);
    options_.push_back(terrain::Type::lava);
    options_.push_back(terrain::Type::light_source);
    options_.push_back(terrain::Type::masonry);
    options_.push_back(terrain::Type::air);
}



void CreateBlockScene::exit(Platform& pfrm, macro::State& state, Scene& next)
{
    MacrocosmScene::exit(pfrm, state, next);

    const auto st = calc_screen_tiles(pfrm);
    for (int y = st.y - 8; y < st.y; ++y) {
        for (int x = 0; x < 32; ++x) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }
}



Coins CreateBlockScene::cost(macro::State& state, terrain::Type t)
{
    return terrain::cost(state.sector(), t);
}



void CreateBlockScene::message(Platform& pfrm, macro::State& state)
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

    auto stats = terrain::stats(options_[selector_],
                                // FIXME!
                                false);
    if (stats.food_) {
        text.append("  ");
        pfrm.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 414);
        text.append(stats.food_);
    }

    if (stats.housing_) {
        text.append("  ");
        pfrm.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 416);
        text.append(stats.housing_);
    }

    if (stats.employment_) {
        text.append("  ");
        pfrm.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 415);
        text.append(stats.employment_);
    }

    if (not stats.commodities_.empty()) {
        text.append("  ");
        pfrm.set_tile(Layer::overlay, text.len() - 1, st.y - 1, 417);
        text.append(stats.commodities_[0].supply_);
    }


    const int count = st.x - text.len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text.len(), st.y - 1, 426);
    }

    text.__detach();
}



void CreateBlockScene::show_options(Platform& pfrm, State& state)
{
    auto st = calc_screen_tiles(pfrm);

    message(pfrm, state);

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

    if (player.key_pressed(pfrm, Key::alt_1)) {
        if (player.key_down(pfrm, Key::right)) {
            pfrm.speaker().play_sound("cursor_tick", 2);
            state.advance(1);
            update_ui(state);
        }
    } else {
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
    }


    if (player.key_down(pfrm, Key::action_1)) {
        return onclick(pfrm, state);
    }

    if (player.key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<SelectorScene>();
    }

    return null_scene();
}



ScenePtr<Scene> CreateBlockScene::onclick(Platform& pfrm, macro::State& state)
{
    auto cursor = state.sector().cursor();
    if (not check_z() or cursor.z < macro::terrain::Sector::z_limit - 1) {
        auto cost = this->cost(state, options_[selector_]);
        if (cost > state.data_->p().coins_.get()) {
            pfrm.speaker().play_sound("beep_error", 2);
            return null_scene();
        } else {
            state.data_->p().coins_.set(state.data_->p().coins_.get() - cost);
        }

        edit(state, options_[selector_]);
        state.sector().update();
        update_ui_on_exit();

        if (options_[selector_] not_eq terrain::Type::air) {
            pfrm.speaker().play_sound("build0", 4);

            auto& block = state.sector().get_block(cursor);
            if (block.type() == terrain::Type::port) {
                return scene_pool::alloc<ConfigurePortScene>();
            } else {
                return scene_pool::alloc<SelectorScene>();
            }

        } else {
            pfrm.speaker().play_sound("cursor_tick", 2);
        }
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

    if (not(terrain::categories(t) & terrain::Categories::crop) and
        not(terrain::categories(t) & terrain::Categories::livestock)) {
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



void ConfigurePortScene::collect_options(macro::State& state)
{
    selector_ = 0;

    auto st = state.sector().base_stats();
    for (auto& c : st.commodities_) {
        switch (c.type_) {
        case terrain::Commodity::indigo:
            options_.push_back(terrain::Type::indigo);
            break;

        case terrain::Commodity::rose_madder:
            options_.push_back(terrain::Type::madder);
            break;

        case terrain::Commodity::shellfish:
            options_.push_back(terrain::Type::shellfish);
            break;

        case terrain::Commodity::sunflowers:
            options_.push_back(terrain::Type::sunflowers);
            break;

        case terrain::Commodity::wool:
            options_.push_back(terrain::Type::wool);
            break;

        case terrain::Commodity::saffron:
            options_.push_back(terrain::Type::saffron);
            break;

        case terrain::Commodity::bananas:
            options_.push_back(terrain::Type::bananas);
            break;

        case terrain::Commodity::food:
            Platform::fatal("Food is a special case, and no tiles should "
                            "ideally produce food as a commodity.");
            break;
        }

        commodity_types_.push_back(c.type_);
    }

    if (st.food_ > 0) {
        options_.push_back(terrain::Type::food);
        commodity_types_.push_back(terrain::Commodity::Type::food);
    }
}



ScenePtr<Scene> ConfigurePortScene::onclick(Platform& pfrm, macro::State& state)
{
    auto c = state.sector().cursor();
    --c.z;
    state.sector().remove_export(c);
    update_ui_on_exit();
    return scene_pool::alloc<ConfigurePortCountScene>(
        commodity_types_[selector_]);
}



void ConfigurePortScene::message(Platform& pfrm, macro::State& state)
{
    auto st = calc_screen_tiles(pfrm);

    StringBuffer<30> message = SYSTR(macro_export)->c_str();
    message += " ";
    message += loadstr(pfrm, terrain::name(options_[selector_]))->c_str();

    Text text(pfrm, OverlayCoord{0, u8(st.y - 1)});
    text.assign(message.c_str());

    const int count = st.x - text.len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text.len(), st.y - 1, 426);
    }

    text.__detach();
}



void ConfigurePortCountScene::enter(Platform& pfrm,
                                    macro::State& state,
                                    Scene& prev)
{
    MacrocosmScene::enter(pfrm, state, prev);
    show(pfrm, state);
}



void ConfigurePortCountScene::exit(Platform& pfrm,
                                   macro::State& state,
                                   Scene& next)
{
    MacrocosmScene::exit(pfrm, state, next);

    for (int y = text_->coord().y - 6; y < text_->coord().y + 1; ++y) {
        for (int x = 0; x < 32; ++x) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }

    text_.reset();
}



void ConfigurePortCountScene::show(Platform& pfrm, macro::State& state)
{
    auto st = calc_screen_tiles(pfrm);

    StringBuffer<30> message = SYSTR(macro_export_how_many)->c_str();
    message += " ";

    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, u8(st.y - 1)});
    }

    text_->assign(message.c_str());
    text_->append(count_);
    text_->append("/");
    text_->append(state.sector().quantity_non_exported(type_));

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < 32; ++i) {
        pfrm.set_tile(Layer::overlay, i, text_->coord().y - 1, 425);
    }
}



ScenePtr<Scene> ConfigurePortCountScene::update(Platform& pfrm,
                                                Player& player,
                                                macro::State& state)
{
    if (player.key_down(pfrm, Key::up)) {
        if (count_ < state.sector().quantity_non_exported(type_)) {
            ++count_;
            show(pfrm, state);
            pfrm.speaker().play_sound("click", 1);
        }
    }

    if (player.key_down(pfrm, Key::down)) {
        if (count_ > 0) {
            --count_;
            show(pfrm, state);
            pfrm.speaker().play_sound("click", 1);
        }
    }

    if (player.key_down(pfrm, Key::action_1)) {
        return scene_pool::alloc<ConfigurePortDestScene>(type_, count_);
    }

    return null_scene();
}



void ConfigurePortDestScene::enter(Platform& pfrm,
                                   macro::State& state,
                                   Scene& prev)
{
    MacrocosmScene::enter(pfrm, state, prev);

    auto current = state.sector().coordinate();

    auto o = state.data_->origin_sector_.coordinate();
    if (o not_eq current) {
        export_options_.push_back(o);
    }

    for (auto& s : state.data_->other_sectors_) {
        if (s->coordinate() not_eq current) {
            export_options_.push_back(s->coordinate());
        }
    }

    show(pfrm, state);
}



void ConfigurePortDestScene::exit(Platform& pfrm,
                                  macro::State& state,
                                  Scene& next)
{
    MacrocosmScene::exit(pfrm, state, next);

    for (int y = text_->coord().y - 6; y < text_->coord().y + 1; ++y) {
        for (int x = 0; x < 32; ++x) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }

    text_.reset();
}



void ConfigurePortDestScene::show(Platform& pfrm, macro::State& state)
{
    auto st = calc_screen_tiles(pfrm);

    StringBuffer<30> message = SYSTR(macro_export_where)->c_str();
    message += " ";

    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, u8(st.y - 1)});
    }


    text_->assign(message.c_str());

    if (auto s = state.load_sector(export_options_[selection_])) {
        text_->append(s->name().c_str());
    }


    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < 32; ++i) {
        pfrm.set_tile(Layer::overlay, i, text_->coord().y - 1, 425);
    }
}



ScenePtr<Scene> ConfigurePortDestScene::update(Platform& pfrm,
                                               Player& player,
                                               macro::State& state)
{
    if (player.key_down(pfrm, Key::up)) {
        if (selection_ < (int)export_options_.size() - 1) {
            ++selection_;
        } else {
            selection_ = 0;
        }
        show(pfrm, state);
        pfrm.speaker().play_sound("click", 1);
    }

    if (player.key_down(pfrm, Key::down)) {
        if (selection_ > 0) {
            --selection_;
        } else {
            selection_ = export_options_.size() - 1;
        }
        show(pfrm, state);
        pfrm.speaker().play_sound("click", 1);
    }

    if (player.key_down(pfrm, Key::action_1)) {
        auto c = state.sector().cursor();
        --c.z;

        terrain::Sector::ExportInfo info;
        info.c = type_;
        info.source_coord_ = c;
        info.destination_ =
            state.load_sector(export_options_[selection_])->coordinate();
        info.export_supply_.set((u16)export_count_);
        state.sector().set_export(info);

        update_ui_on_exit();

        return scene_pool::alloc<SelectorScene>();
    }


    return null_scene();
}



} // namespace skyland::macro
