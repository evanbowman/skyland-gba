#include "skyland.hpp"
#include "platform/platform.hpp"
#include "globals.hpp"



namespace skyland {



static void init_clouds(Platform& pfrm);



App::App(Platform& pfrm) :
    player_island_(pfrm, Layer::map_0_ext, 6),
    current_scene_(null_scene()),
    next_scene_(null_scene())
{
    pfrm.load_tile0_texture("tilesheet");

    player_island_.set_position({10, 274});


    current_scene_ = initial_scene();
    current_scene_->enter(pfrm, *this, *current_scene_);


    init_clouds(pfrm);
}



void App::update(Platform& pfrm, Microseconds delta)
{
    if (next_scene_) {
        next_scene_->enter(pfrm, *this, *current_scene_);

        current_scene_ = std::move(next_scene_);
    }

    next_scene_ = current_scene_->update(pfrm, *this, delta);

    if (next_scene_) {
        current_scene_->exit(pfrm, *this, *next_scene_);
    }
}



void App::updateParallax(Microseconds delta)
{
    cloud_scroll_1_ += 0.00002f * delta;
    cloud_scroll_2_ += 0.00004f * delta;
}



void App::render(Platform& pfrm)
{
    pfrm.enable_feature("_prlx7", (u8)cloud_scroll_1_);
    pfrm.enable_feature("_prlx8", (u8)cloud_scroll_2_);
}



static void init_clouds(Platform& pfrm)
{
    pfrm.enable_feature("parallax-clouds", true);

    for (int i = 0; i < 32; ++i) {
        for (int j = 0; j < 32; ++j) {
            pfrm.set_tile(Layer::background, i, j, 4);
        }
    }

    for (int i = 0; i < 32; ++i) {
        pfrm.set_tile(Layer::background, i, 18, 5);
        pfrm.set_tile(Layer::background, i, 19, 5);
    }

    auto put_cloud_block = [&](int x, int y, int offset)
    {
        pfrm.set_tile(Layer::background, x, y, offset++);
        pfrm.set_tile(Layer::background, x + 1, y, offset++);
        pfrm.set_tile(Layer::background, x, y + 1, offset++);
        pfrm.set_tile(Layer::background, x + 1, y + 1, offset);
    };

    auto put_fg_cloud_type_n = [&](int x, int type)
    {
        put_cloud_block(x * 2, 16, 8 + type * 4);
    };

    auto put_bg_cloud_type_n = [&](int x, int type)
    {
        put_cloud_block(x * 2, 14, 32 + type * 4);
    };

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 6;
        put_fg_cloud_type_n(offset + 0, 0);
        put_fg_cloud_type_n(offset + 1, 1);
        put_fg_cloud_type_n(offset + 2, 2);
        put_fg_cloud_type_n(offset + 3, 3);
        put_fg_cloud_type_n(offset + 4, 4);
        put_fg_cloud_type_n(offset + 5, 5);
    }

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 4;
        put_bg_cloud_type_n(offset + 0, 0);
        put_bg_cloud_type_n(offset + 1, 1);
        put_bg_cloud_type_n(offset + 2, 2);
        put_bg_cloud_type_n(offset + 3, 3);
    }

}



}
