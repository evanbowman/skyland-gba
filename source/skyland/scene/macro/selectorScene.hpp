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


#pragma once

#include "macrocosmScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland::macro
{



class SelectorScene : public MacrocosmScene
{
public:


    ScenePtr<Scene> update(Platform& pfrm,
                           Player& player,
                           macro::State& state) override
    {
        if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
            return scene;
        }

        auto cursor_ = state.data_->sector_.cursor();

        if (player.key_down(pfrm, Key::alt_1)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            state.data_->sector_.rotate();
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
        }

        if (player.key_down(pfrm, Key::alt_2)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            state.data_->sector_.rotate();
            state.data_->sector_.rotate();
            state.data_->sector_.rotate();
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
        }

        if (not text_) {
            text_.emplace(pfrm, OverlayCoord{0, 19});
        } else {
            // StringBuffer<48> b;
            // b += "(";
            // b += stringify(cursor_.x).c_str();
            // b += ", ";
            // b += stringify(cursor_.y).c_str();
            // b += ", ";
            // b += stringify(cursor_.z).c_str();
            // b += ")";
            // text_->assign(b.c_str());
        }

        auto test_key = [&](Key k) {
                            return player.test_key(pfrm, k, milliseconds(500), milliseconds(100));
                        };

        if (test_key(Key::up) and cursor_.y > 0) {
            --cursor_.y;
            state.data_->sector_.set_cursor(cursor_);
        }

        if (test_key(Key::down) and cursor_.y < 7) {
            ++cursor_.y;
            state.data_->sector_.set_cursor(cursor_);
        }

        if (test_key(Key::right) and cursor_.x > 0) {
            --cursor_.x;
            state.data_->sector_.set_cursor(cursor_);
        }

        if (test_key(Key::left) and cursor_.x < 7) {
            ++cursor_.x;
            state.data_->sector_.set_cursor(cursor_);
        }

        if (player.key_down(pfrm, Key::action_1)) {

            if (cursor_.z < macro::terrain::Sector::z_limit - 1) {
                state.data_->sector_.set_block(cursor_, macro::terrain::Type::masonry);
                ++cursor_.z;
                state.data_->sector_.set_cursor(cursor_);
            }
        }

        if (player.key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<TitleScreenScene>(3);
        }


        return null_scene();
    }


private:
    std::optional<Text> text_;
};



}
