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


#include "tileOptionsScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void TileOptionsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    MacrocosmScene::enter(pfrm, app, prev);
    collect_options(pfrm, *app.macrocosm());
    show_options(pfrm);
}



void TileOptionsScene::exit(Platform& pfrm, App& app, Scene& next)
{
    MacrocosmScene::exit(pfrm, app, next);
    text_.reset();
    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
TileOptionsScene::update(Platform& pfrm, Player& player, macro::State& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    if (player.key_down(pfrm, Key::action_2)) {

    }

    if (player.key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<SelectorScene>();
    }

    if (player.key_down(pfrm, Key::left)) {
        if (selector_ == 0) {
            selector_ = options_.size() - 1;
        } else {
            --selector_;
        }
        show_options(pfrm);
    }

    if (player.key_down(pfrm, Key::right)) {
        ++selector_;
        selector_ %= options_.size();
        show_options(pfrm);
    }

    return null_scene();
}



struct TileOptionsScene::OptionInfo
{
    SystemString name_;
    int sel_icon_;
    int unsel_icon_;
};



void TileOptionsScene::collect_options(Platform& pfrm, macro::State& state)
{
    static const TileOptionsScene::OptionInfo options[] = {
        {
            SystemString::macro_create_block,
            776,
            760
        },
        {
            SystemString::macro_build_improvement,
            776,
            760
        },
        {
            SystemString::macro_demolish,
            504,
            504,
        }};

    options_.push_back(&options[0]);
    options_.push_back(&options[1]);
    options_.push_back(&options[2]);
}



void TileOptionsScene::show_options(Platform& pfrm)
{
    auto st = calc_screen_tiles(pfrm);

    StringBuffer<32> str = loadstr(pfrm, options_[selector_]->name_)->c_str();
    msg(pfrm, str.c_str());

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        pfrm.set_tile(Layer::overlay, st.x - 22, y, 128);
        pfrm.set_tile(Layer::overlay, st.x - 9, y, 433);
    }

    pfrm.set_tile(Layer::overlay, st.x - 22, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 9, st.y - 2, 418);

    pfrm.load_overlay_chunk(258, options_[(selector_ + 1) % options_.size()]->unsel_icon_, 16);
    pfrm.load_overlay_chunk(181, options_[(selector_ + 2) % options_.size()]->unsel_icon_, 16);
    pfrm.load_overlay_chunk(197, options_[(selector_) % options_.size()]->sel_icon_, 16);

    draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);
    draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);
    draw_image(pfrm, 258, st.x - 13, st.y - 5, 4, 4, Layer::overlay);
}



void TileOptionsScene::msg(Platform& pfrm, const char* text)
{
    auto st = calc_screen_tiles(pfrm);
    text_.emplace(pfrm, text, OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
        pfrm.set_tile(Layer::overlay, i, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 0);
        pfrm.set_tile(Layer::overlay, i, st.y - 5, 0);

        pfrm.set_tile(Layer::overlay, i, st.y - 6, 0);
    }
}



} // namespace skyland::macro