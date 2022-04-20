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



void CreateBlockScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    MacrocosmScene::enter(pfrm, app, prev);

    auto c = app.macrocosm()->data_->sector_.cursor();
    if (c.z == 0) {
        // We don't allow the player to create every sort of block at layer
        // zero. Only a few specific ones:
        options_.push_back(terrain::Type::rock_stacked);
        options_.push_back(terrain::Type::masonry);
        options_.push_back(terrain::Type::wheat);
    } else {
        options_.push_back(terrain::Type::rock_stacked);
        options_.push_back(terrain::Type::building);
        options_.push_back(terrain::Type::water);
        options_.push_back(terrain::Type::masonry);
        options_.push_back(terrain::Type::wheat);
        options_.push_back(terrain::Type::air);
    }

    show_options(pfrm);
}



void CreateBlockScene::exit(Platform& pfrm, App& app, Scene& next)
{
    MacrocosmScene::exit(pfrm, app, next);
    pfrm.fill_overlay(0);
}



void CreateBlockScene::show_options(Platform& pfrm)
{
    auto st = calc_screen_tiles(pfrm);

    StringBuffer<30> message = SYSTR(construction_build)->c_str();
    message += " ";
    message += loadstr(pfrm, terrain::name(options_[selector_]))->c_str();
    // message += " ";
    // message += stringify(templates[selector_]->cost());
    // message += "@";

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

        auto icon = terrain::icons(options_[index]).second;;
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
        show_options(pfrm);
    }

    if (player.key_down(pfrm, Key::left)) {
        if (selector_ > 0) {
            --selector_;
        } else {
            selector_ = options_.size() - 1;
        }
        show_options(pfrm);
    }

    if (player.key_down(pfrm, Key::action_1)) {
        auto cursor = state.data_->sector_.cursor();
        if (cursor.z < macro::terrain::Sector::z_limit - 1) {
            state.data_->sector_.set_block(cursor,
                                           options_[selector_]);
            ++cursor.z;
            auto block = state.data_->sector_.get_block(cursor);
            while (block.type_ not_eq (u8)terrain::Type::air) {
                ++cursor.z;
                block = state.data_->sector_.get_block(cursor);
            }
            state.data_->sector_.set_cursor(cursor, false);

            if (options_[selector_] not_eq terrain::Type::air) {
                pfrm.speaker().play_sound("build0", 4);
            }

            return scene_pool::alloc<SelectorScene>();
        }
    }

    if (player.key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<SelectorScene>();
    }

    return null_scene();
}



} // namespace skyland::macro
