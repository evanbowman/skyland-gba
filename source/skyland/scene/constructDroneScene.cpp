#include "constructDroneScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/moveDroneScene.hpp"
#include "readyScene.hpp"



namespace skyland {



struct DroneInfo {
    const char* name_;
};



void ConstructDroneScene::draw(Platform& pfrm, App& app)
{
    auto st = calc_screen_tiles(pfrm);


    StringBuffer<30> message = "launch drone: ";

    text_.emplace(pfrm, message.c_str(), OverlayCoord{0, u8(st.y - 1)});

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
            index = available_buildings_.size() - 2;
        } else if (index - 2 < 0) {
            index = available_buildings_.size() - 1;
        } else {
            index = index - 2;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 258, st.x - 25, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(258, icon, 16);
    }

    {
        int index = selector_;
        if (index - 1 < 0) {
            index = available_buildings_.size() - 1;
        } else {
            index = index - 1;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 181, st.x - 21, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(181, icon, 16);
    }

    {
        auto icon = (*available_buildings_[selector_])->icon();
        draw_image(pfrm, 197, st.x - 17, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(197, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 1;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 213, st.x - 13, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(213, icon, 16);
    }

    {
        int index = selector_;
        if (index + 1 >= (int)available_buildings_.size()) {
            index = 1;
        } else if (index + 2 >= (int)available_buildings_.size()) {
            index = 0;
        } else {
            index = index + 2;
        }

        auto icon = (*available_buildings_[index])->unsel_icon();
        draw_image(pfrm, 274, st.x - 9, st.y - 5, 4, 4, Layer::overlay);

        pfrm.load_overlay_chunk(274, icon, 16);
    }
}



void ConstructDroneScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    available_buildings_.push_back(forcefield_mt);
    available_buildings_.push_back(cannon_mt);
    available_buildings_.push_back(missile_silo_mt);

    draw(pfrm, app);
}



void ConstructDroneScene::exit(Platform& pfrm, App&, Scene& next)
{
    text_.reset();
    pfrm.fill_overlay(0);
}



ScenePtr<Scene> ConstructDroneScene::update(Platform& pfrm,
                                            App& app,
                                            Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::action_1)) {
        return scene_pool::alloc<MoveDroneScene>(pfrm, position_);
    }

    if (app.player().key_down(pfrm, Key::right)) {
        if (selector_ < (int)available_buildings_.size() - 1) {
            ++selector_;
            draw(pfrm, app);
        } else {
            selector_ = 0;
            draw(pfrm, app);
        }
    }

    if (app.player().key_down(pfrm, Key::left)) {
        if (selector_ > 0) {
            --selector_;
            draw(pfrm, app);
        } else {
            selector_ = available_buildings_.size() - 1;
            draw(pfrm, app);
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



}
