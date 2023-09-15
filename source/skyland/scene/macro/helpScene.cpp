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


#include "helpScene.hpp"
#include "macroverseScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void HelpScene::show_page(Platform& pfrm, int pg)
{
    tvs_.clear();

    pfrm.fill_overlay(0);

    Text heading(pfrm, {1, 1});
    heading.append("handbook: ");

    int margin = (calc_screen_tiles(pfrm).x - page_count * 2) / 2;
    for (int i = 0; i < page_count; ++i) {
        if (i == page_) {
            pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 101);
        } else {
            pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 100);
        }
    }

    switch (pg) {
    case 0:
        heading.append("population ");
        pfrm.set_tile(Layer::overlay, heading.len() + 1, 1, 413);
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Every island starts with one resident. Each day, your islands will gain and lose residents based on available food and housing! Build more housing to encourage villagers to settle!",
                           {1, 4},
                           {28, 7});
        break;

    case 1:
        heading.append("productivity ");
        pfrm.set_tile(Layer::overlay, heading.len() + 1, 1, 415);
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Each of your villagers produces ten productivity points per day! Spend productivity to build structures, plant or harvest crops, and more!",
                           {1, 4},
                           {28, 7});
        break;

    case 2:
        heading.append("food ");
        pfrm.set_tile(Layer::overlay, heading.len() + 1, 1, 414);
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("",
                           {1, 4},
                           {28, 7});
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Each villager consumes one unit of food per day. Be sure to plant some crops; your residents will start to leave if there's nothing to eat!",
                           {1, 4},
                           {28, 7});
        break;

    case 3:
        heading.append("food ");
        pfrm.set_tile(Layer::overlay, heading.len() + 1, 1, 414);
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Each island can store five units of food. Build granaries to increase your food storage limit!",
                           {1, 4},
                           {28, 7});
        break;

    case 4:
        heading.append("crops");
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Crops take a few days to ripen. Check them regularly! If you leave ripe crops unharvested, they'll go bad! Press the left shoulder button + A to check for ripe crops!",
                           {1, 4},
                           {28, 7});
        break;

    case 5:
        heading.append("crops");
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Crops won't grow in the dark. But you can build light sources, allowing you to layer crops vertically!",
                           {1, 4},
                           {28, 7});
        break;

    case 6:
        heading.append("crops");
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("Wheat: 2 days, yield 3, Potatoes: 6 days, yield 15, Lumber: 10 days, yields wood",
                           {1, 4},
                           {28, 7});
        break;

    case 7:
        heading.append("colonies");
        tvs_.emplace_back(pfrm);
        tvs_.back().assign("When you have a large enough population, you can settle other nearby islands! See the macrocosm menu for options.",
                           {1, 4},
                           {28, 7});
        break;
    }

    heading.__detach();

    auto st = calc_screen_tiles(pfrm);
    for (int x = 0; x < st.x; ++x) {
        pfrm.set_tile(Layer::overlay, x, 2, 107);
    }
}



void HelpScene::enter(Platform& pfrm, App&, Scene&)
{
    pfrm.screen().schedule_fade(1.f);
    pfrm.fill_overlay(0);

    show_page(pfrm, 0);
}



void HelpScene::exit(Platform& pfrm, App&, Scene&)
{
    pfrm.screen().schedule_fade(0);
    pfrm.fill_overlay(0);

    tvs_.clear();
}



ScenePtr<Scene> HelpScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    player(app).update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    if (player(app).key_down(pfrm, Key::action_1) or
        player(app).key_down(pfrm, Key::action_2)) {

        return scene_pool::alloc<SelectorScene>();
    }

    if (test_key(Key::right)) {
        if (page_ < page_count - 1) {
            show_page(pfrm, ++page_);
        }
    }

    if (test_key(Key::left)) {
        if (page_ > 0) {
            show_page(pfrm, --page_);
        }
    }

    return null_scene();
}



} // namespace skyland::macro
