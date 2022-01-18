#include "constructDroneScene.hpp"
#include "localization.hpp"
#include "readyScene.hpp"
#include "skyland/scene/placeDroneScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void ConstructDroneScene::draw(Platform& pfrm, App& app)
{
    auto st = calc_screen_tiles(pfrm);


    auto [templates, template_count] = drone_metatable();

    StringBuffer<30> message = "deploy: ";
    message += templates[selector_]->name();
    message += " ";
    message += to_string<10>(templates[selector_]->cost());
    message += "@";

    if (not text_) {
        text_.emplace(pfrm, OverlayCoord{0, u8(st.y - 1)});
    }
    text_->assign(message.c_str());

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

    for (int i = st.x - 25; i < st.x - 5; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 6, 425);
    }

    for (int y = st.y - 5; y < st.y - 2; ++y) {
        pfrm.set_tile(Layer::overlay, st.x - 26, y, 128);
        pfrm.set_tile(Layer::overlay, st.x - 5, y, 433);
    }

    pfrm.set_tile(Layer::overlay, st.x - 26, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 5, st.y - 2, 418);


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
        draw_image(pfrm, 258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = selector_;
        if (index - 1 < 0) {
            index = template_count - 1;
        } else {
            index = index - 1;
        }

        auto icon = (templates[index])->unsel_icon();
        draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = (templates[selector_])->icon();
        draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)template_count) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon = (templates[index])->unsel_icon();
        draw_image(pfrm, 213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(213, icon, 16);
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
        draw_image(pfrm, 274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(274, icon, 16);
    }
}



void ConstructDroneScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    persist_ui();

    draw(pfrm, app);
}



void ConstructDroneScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    text_.reset();
    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
ConstructDroneScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    auto [templates, template_count] = drone_metatable();

    if (app.player().key_down(pfrm, Key::action_1)) {
        const auto cost = templates[selector_]->cost();
        if (app.coins() >= cost) {
            return scene_pool::alloc<PlaceDroneScene>(
                pfrm, position_, &templates[selector_]);
        }
    }


    auto test_key = [&](Key k) {
        return app.player().test_key(pfrm,
                                     k,
                                     milliseconds(500),
                                     milliseconds(150));
    };


    if (test_key(Key::right)) {
        if (selector_ < (int)template_count - 1) {
            ++selector_;
            draw(pfrm, app);
        } else {
            selector_ = 0;
            draw(pfrm, app);
        }
    }

    if (test_key(Key::left)) {
        if (selector_ > 0) {
            --selector_;
            draw(pfrm, app);
        } else {
            selector_ = template_count - 1;
            draw(pfrm, app);
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
