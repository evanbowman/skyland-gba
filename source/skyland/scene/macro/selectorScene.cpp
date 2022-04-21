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
#include "modifiedSelectorScene.hpp"
#include "nextTurnScene.hpp"
#include "skyland/scene/startMenuScene.hpp"
#include "skyland/scene_pool.hpp"
#include "tileOptionsScene.hpp"



namespace skyland::macro
{



void SelectorScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    MacrocosmScene::enter(pfrm, app, prev);
    Text::platform_retain_alphabet(pfrm);
}



void SelectorScene::exit(Platform& pfrm, App& app, Scene& next)
{
    MacrocosmScene::exit(pfrm, app, next);
    text_.reset();
}



ScenePtr<Scene>
SelectorScene::update(Platform& pfrm, Player& player, macro::State& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    auto& sector = state.sector();
    auto cursor = sector.cursor();

    if (player.key_down(pfrm, Key::select)) {
        return scene_pool::alloc<NextTurnScene>();
    }

    if (player.key_down(pfrm, Key::start)) {
        return scene_pool::alloc<StartMenuScene>(0);
    }

    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, 19});
    }

    auto test_key = [&](Key k) {
        return player.test_key(pfrm, k, milliseconds(500), milliseconds(100));
    };

    auto msg = [&] {
        auto s = SystemString::block_air;
        auto cursor = sector.cursor();
        if (cursor.z > 0) {
            --cursor.z;
            auto& block = sector.get_block(cursor);
            s = block.name();
        }
        StringBuffer<48> b;
        b += "(";
        b += loadstr(pfrm, s)->c_str();
        b += ")";
        text_->assign(b.c_str());
    };

    if (player.key_pressed(pfrm, Key::alt_1) or
        player.key_pressed(pfrm, Key::alt_2)) {

        return scene_pool::alloc<ModifiedSelectorScene>();

    } else {

        if (test_key(Key::up) and cursor.y > 0) {
            --cursor.y;
            sector.set_cursor(cursor);
            msg();
            pfrm.speaker().play_sound("cursor_tick", 2);
        }

        if (test_key(Key::down) and cursor.y < 7) {
            ++cursor.y;
            sector.set_cursor(cursor);
            msg();
            pfrm.speaker().play_sound("cursor_tick", 2);
        }

        if (test_key(Key::right) and cursor.x > 0) {
            --cursor.x;
            sector.set_cursor(cursor);
            msg();
            pfrm.speaker().play_sound("cursor_tick", 2);
        }

        if (test_key(Key::left) and cursor.x < 7) {
            ++cursor.x;
            sector.set_cursor(cursor);
            msg();
            pfrm.speaker().play_sound("cursor_tick", 2);
        }
    }

    if (player.key_down(pfrm, Key::action_1)) {

        if (cursor.z == 0) {
            return scene_pool::alloc<CreateBlockScene>();
        } else {
            return scene_pool::alloc<TileOptionsScene>();
        }
    }

    return null_scene();
}



} // namespace skyland::macro
