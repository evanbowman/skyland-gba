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


#include "constructDroneScene.hpp"
#include "readyScene.hpp"
#include "skyland/scene/placeDroneScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void ConstructDroneScene::draw(App& app)
{
    auto st = calc_screen_tiles();


    auto [templates, template_count] = drone_metatable();

    StringBuffer<30> message = SYSTR(deploy_drone_prompt)->c_str();
    message += templates[selector_]->name();
    message += " ";
    message += stringify(templates[selector_]->cost());
    message += "@";

    if (not text_) {
        text_.emplace(OverlayCoord{0, u8(st.y - 1)});
    }
    text_->assign(message.c_str());

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

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


    {
        int index = selector_;
        if (index - 2 < -1) {
            index = template_count - 2;
        } else if (index - 2 < 0) {
            index = template_count - 1;
        } else {
            index = index - 2;
        }

        auto icon = (templates[index])->unsel_icon();
        draw_image(258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = selector_;
        if (index - 1 < 0) {
            index = template_count - 1;
        } else {
            index = index - 1;
        }

        auto icon = (templates[index])->unsel_icon();
        draw_image(181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = (templates[selector_])->icon();
        draw_image(197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)template_count) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon = (templates[index])->unsel_icon();
        draw_image(213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(213, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)template_count) {
            index = 1;
        } else if (index + 2 >= (int)template_count) {
            index = 0;
        } else {
            index = index + 2;
        }

        auto icon = (templates[index])->unsel_icon();
        draw_image(274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        PLATFORM.load_overlay_chunk(274, icon, 16);
    }
}



void ConstructDroneScene::enter(App& app, Scene& prev)
{
    ActiveWorldScene::enter(app, prev);

    persist_ui();

    draw(app);
}



void ConstructDroneScene::exit(App& app, Scene& next)
{
    ActiveWorldScene::exit(app, next);

    text_.reset();
    PLATFORM.fill_overlay(0);
}



ScenePtr<Scene> ConstructDroneScene::update(App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(app, delta)) {
        return scene;
    }

    auto [templates, template_count] = drone_metatable();

    if (app.player().key_down(Key::action_1)) {
        const auto cost = templates[selector_]->cost();
        if (app.coins() >= cost) {
            return scene_pool::alloc<PlaceDroneScene>(position_,
                                                      &templates[selector_]);
        }
    }


    auto test_key = [&](Key k) {
        return app.player().test_key(k, milliseconds(500), milliseconds(150));
    };


    if (test_key(Key::right)) {
        if (selector_ < (int)template_count - 1) {
            ++selector_;
            draw(app);
        } else {
            selector_ = 0;
            draw(app);
        }
    }

    if (test_key(Key::left)) {
        if (selector_ > 0) {
            --selector_;
            draw(app);
        } else {
            selector_ = template_count - 1;
            draw(app);
        }
    }

    if (app.player().key_down(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
