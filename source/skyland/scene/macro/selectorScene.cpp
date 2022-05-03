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


#include "selectorScene.hpp"
#include "createBlockScene.hpp"
#include "keyComboScene.hpp"
#include "menuOptionsScene.hpp"
#include "modifiedSelectorScene.hpp"
#include "nextTurnScene.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene_pool.hpp"
#include "tileOptionsScene.hpp"


namespace skyland::macro
{



void SelectorScene::enter(Platform& pfrm, macro::State& state, Scene& prev)
{
    MacrocosmScene::enter(pfrm, state, prev);
    Text::platform_retain_alphabet(pfrm);
}



void SelectorScene::exit(Platform& pfrm, macro::State& state, Scene& next)
{
    MacrocosmScene::exit(pfrm, state, next);
    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 0);
    }
    text_.reset();
    text_2_.reset();
}



ScenePtr<Scene>
SelectorScene::update(Platform& pfrm, Player& player, macro::State& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    auto& sector = state.sector();
    auto cursor = sector.cursor();

    if (player.key_down(pfrm, Key::start)) {
        await_start_key_ = true;
    }

    if (player.key_pressed(pfrm, Key::start)) {
        if (player.key_down(pfrm, Key::alt_2)) {
            return scene_pool::alloc<KeyComboScene>(true);
        }
    } else {
        if (await_start_key_ and player.key_up(pfrm, Key::start)) {
            return scene_pool::alloc<StartMenuScene>(0);
        }
    }


    if (player.key_down(pfrm, Key::select)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        StringBuffer<48> temp;
        return scene_pool::alloc<NextTurnScene>();
    }


    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, 19});
    }
    describe_selected(pfrm, state);

    auto test_key = [&](Key k) {
        return player.test_key(pfrm, k, milliseconds(500), milliseconds(100));
    };


    if (player.key_pressed(pfrm, Key::alt_1)) {

        return scene_pool::alloc<MenuOptionsScene>();

    } else if (player.key_down(pfrm, Key::alt_2)) {

        return scene_pool::alloc<ModifiedSelectorScene>(false);

    } else {

        if (test_key(Key::up) and cursor.y > 0) {
            --cursor.y;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 2);
        }

        if (test_key(Key::down) and cursor.y < sector.size().x - 1) {
            ++cursor.y;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 2);
        }

        if (test_key(Key::right) and cursor.x > 0) {
            --cursor.x;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 2);
        }

        if (test_key(Key::left) and cursor.x < sector.size().y - 1) {
            ++cursor.x;
            sector.set_cursor(cursor);
            describe_selected(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 2);
        }
    }

    if (player.key_down(pfrm, Key::action_1)) {
        pfrm.speaker().play_sound("button_wooden", 3);
        if (cursor.z == 0) {
            return scene_pool::alloc<CreateBlockScene>();
        } else {
            return scene_pool::alloc<TileOptionsScene>();
        }
    }

    return null_scene();
}



void SelectorScene::describe_selected(Platform& pfrm, macro::State& state)
{
    auto& sector = state.sector();

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 0);
    }

    auto s = SystemString::block_air;
    auto cursor = sector.cursor();
    auto tp = terrain::Type::air;
    bool shadowed = false;
    if (cursor.z > 0) {
        --cursor.z;
        auto& block = sector.get_block(cursor);
        tp = block.type();
        s = block.name();
        shadowed = block.shadowed_;
    }

    if (tp == terrain::Type::port) {
        StringBuffer<48> b;
        for (auto& e : sector.exports()) {
            if (e.source_coord_ == cursor) {
                b += loadstr(pfrm, name(e.c))->c_str();
                b += "(";
                b += stringify(e.export_supply_.get());
                b += ")->";
                if (auto dest = state.load_sector(e.destination_)) {
                    b += dest->name();
                }
            }
        }

        if (not b.empty()) {
            text_->assign(b.c_str());

            for (int i = 0; i < text_->len(); ++i) {
                pfrm.set_tile(Layer::overlay, i, 18, 425);
            }

            return;
        }
    } else {
        text_2_->erase();
    }

    StringBuffer<48> b;
    b += "(";
    b += loadstr(pfrm, s)->c_str();
    b += ")";

    text_->assign(b.c_str());

    auto stats = terrain::stats(tp, shadowed);

    if ((terrain::categories(tp) & terrain::Categories::crop) and shadowed) {
        text_->append("  ");
        pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 411);
    } else {
        if (stats.food_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 414);
            text_->append(stats.food_);
        }

        if (stats.housing_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 416);
            text_->append(stats.housing_);
        }

        if (stats.employment_) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 415);
            text_->append(stats.employment_);
        }

        if (not stats.commodities_.empty()) {
            text_->append("  ");
            pfrm.set_tile(Layer::overlay, text_->len() - 1, 19, 417);
            text_->append(stats.commodities_[0].supply_);
        }
    }

    for (int i = 0; i < text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }
}



} // namespace skyland::macro
