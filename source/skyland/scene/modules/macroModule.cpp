#include "macroModule.hpp"
#include "skyland/player/player.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void MacroModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.speaker().play_music(app.environment().music(), 0);

    chunk_ = allocate_dynamic<macro::terrain::Chunk>("macro-chunk");

    scroll_ = 42;
    pfrm.set_scroll(Layer::map_0, 0, scroll_);
    pfrm.set_scroll(Layer::map_1, 0, scroll_ + 8);

    pfrm.load_background_texture("background_macro");
    // pfrm.system_call("parallax-clouds", false);

    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }


    pfrm.screen().set_view({});


    __draw_image(pfrm, 0, 0, 0, 30, 16, Layer::map_0);
    __draw_image(pfrm, 0, 0, 17, 30, 16, Layer::map_1);


    (*chunk_)->blocks_[1][1][3].type_ = 1;
    (*chunk_)->blocks_[1][1][4].type_ = 1;

    (*chunk_)->blocks_[1][0][6].type_ = 1;
    (*chunk_)->blocks_[1][0][1].type_ = 4;
    (*chunk_)->blocks_[2][0][1].type_ = 4;

    // for (int z = 0; z < 8; ++z) {
    //     for (int x = 0; x < 8; ++x) {
    //         for (int y = 0; y < 8; ++y) {
    //             (*chunk_)->blocks_[z][x][y].type_ = 2;
    //         }
    //     }
    // }

    (*chunk_)->blocks_[0][0][1].type_ = 2;
    (*chunk_)->blocks_[0][0][2].type_ = 2;
    (*chunk_)->blocks_[0][0][3].type_ = 2;
    (*chunk_)->blocks_[0][0][4].type_ = 2;
    (*chunk_)->blocks_[0][0][5].type_ = 2;
    (*chunk_)->blocks_[0][0][6].type_ = 2;
    (*chunk_)->blocks_[0][0][7].type_ = 2;

    (*chunk_)->blocks_[0][1][0].type_ = 2;
    (*chunk_)->blocks_[0][1][1].type_ = 2;
    //(*chunk_)->blocks_[0][1][2].type_ = 2;
    (*chunk_)->blocks_[0][1][3].type_ = 2;
    (*chunk_)->blocks_[0][1][4].type_ = 2;
    (*chunk_)->blocks_[0][1][5].type_ = 2;
    (*chunk_)->blocks_[0][1][6].type_ = 2;
    (*chunk_)->blocks_[0][1][7].type_ = 2;

    (*chunk_)->blocks_[0][2][0].type_ = 2;
    (*chunk_)->blocks_[0][2][1].type_ = 2;
    (*chunk_)->blocks_[0][2][2].type_ = 2;
    (*chunk_)->blocks_[0][2][3].type_ = 2;
    (*chunk_)->blocks_[0][2][4].type_ = 2;
    (*chunk_)->blocks_[0][2][5].type_ = 2;
    (*chunk_)->blocks_[0][2][6].type_ = 2;
    (*chunk_)->blocks_[0][2][7].type_ = 2;

    (*chunk_)->blocks_[0][3][0].type_ = 2;
    (*chunk_)->blocks_[0][3][1].type_ = 2;
    (*chunk_)->blocks_[0][3][2].type_ = 2;
    (*chunk_)->blocks_[0][3][3].type_ = 2;
    (*chunk_)->blocks_[0][3][4].type_ = 2;
    (*chunk_)->blocks_[0][3][5].type_ = 2;
    (*chunk_)->blocks_[0][3][6].type_ = 2;
    (*chunk_)->blocks_[0][3][7].type_ = 2;

    (*chunk_)->blocks_[0][4][0].type_ = 2;
    (*chunk_)->blocks_[0][4][1].type_ = 2;
    (*chunk_)->blocks_[0][4][2].type_ = 2;
    (*chunk_)->blocks_[0][4][3].type_ = 2;
    (*chunk_)->blocks_[0][4][4].type_ = 2;
    (*chunk_)->blocks_[0][4][5].type_ = 2;
    (*chunk_)->blocks_[0][4][6].type_ = 2;
    (*chunk_)->blocks_[0][4][7].type_ = 2;

    (*chunk_)->blocks_[0][5][0].type_ = 2;
    (*chunk_)->blocks_[0][5][1].type_ = 2;
    (*chunk_)->blocks_[0][5][2].type_ = 2;
    (*chunk_)->blocks_[0][5][3].type_ = 2;
    (*chunk_)->blocks_[0][5][4].type_ = 2;
    (*chunk_)->blocks_[0][5][5].type_ = 2;
    (*chunk_)->blocks_[0][5][6].type_ = 2;
    (*chunk_)->blocks_[0][5][7].type_ = 2;

    (*chunk_)->blocks_[0][6][0].type_ = 2;
    (*chunk_)->blocks_[0][6][1].type_ = 2;
    (*chunk_)->blocks_[0][6][2].type_ = 2;
    (*chunk_)->blocks_[0][6][3].type_ = 2;

    (*chunk_)->blocks_[0][7][0].type_ = 2;
    (*chunk_)->blocks_[0][7][1].type_ = 2;
    (*chunk_)->blocks_[0][7][2].type_ = 2;
    (*chunk_)->blocks_[0][7][3].type_ = 2;
    (*chunk_)->blocks_[0][7][4].type_ = 2;

    (*chunk_)->blocks_[5][7][1].type_ = 2;
    (*chunk_)->blocks_[5][7][2].type_ = 2;
    (*chunk_)->blocks_[1][7][2].type_ = 1;
    (*chunk_)->blocks_[5][7][3].type_ = 2;
    (*chunk_)->blocks_[5][6][1].type_ = 2;
    (*chunk_)->blocks_[5][6][2].type_ = 2;

    (*chunk_)->blocks_[1][7][0].type_ = 4;
    (*chunk_)->blocks_[2][7][0].type_ = 4;
    (*chunk_)->blocks_[3][7][0].type_ = 4;
    (*chunk_)->blocks_[4][7][0].type_ = 4;
    (*chunk_)->blocks_[5][7][0].type_ = 4;
    (*chunk_)->blocks_[6][7][0].type_ = 4;
    (*chunk_)->blocks_[7][7][0].type_ = 4;

    (*chunk_)->shadowcast();


    render(pfrm);

    pfrm.screen().schedule_fade(0.f);
}



void MacroModule::render(Platform& pfrm)
{
    macro::render(pfrm, **chunk_);
}



ScenePtr<Scene>
MacroModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    if (player(app).key_down(pfrm, Key::alt_1)) {
        pfrm.screen().schedule_fade(
            0.7f, custom_color(0x102447));
        pfrm.screen().clear();
        pfrm.screen().display();
        (*chunk_)->rotate();
        render(pfrm);
        pfrm.screen().schedule_fade(
            0.f, ColorConstant::rich_black);
    }

    if (player(app).key_down(pfrm, Key::alt_2)) {
        pfrm.screen().schedule_fade(
            0.7f, custom_color(0x102447));
        pfrm.screen().clear();
        pfrm.screen().display();
        (*chunk_)->rotate();
        (*chunk_)->rotate();
        (*chunk_)->rotate();
        render(pfrm);
        pfrm.screen().schedule_fade(
            0.f, ColorConstant::rich_black);
    }

    if (player(app).key_down(pfrm, Key::up) and cursor_.y > 0) {
        --cursor_.y;
    }

    if (player(app).key_down(pfrm, Key::down) and cursor_.y < 7) {
        ++cursor_.y;
    }

    if (player(app).key_down(pfrm, Key::right) and cursor_.x > 0) {
        --cursor_.x;
    }

    if (player(app).key_down(pfrm, Key::left) and cursor_.x < 7) {
        ++cursor_.x;
    }

    if (player(app).key_down(pfrm, Key::select) and cursor_.z > 0) {
        --cursor_.z;
    }

    if (player(app).key_down(pfrm, Key::action_1)) {
        auto& selected = (*chunk_)->blocks_[cursor_.z][cursor_.x][cursor_.y];

        ++cursor_.z;

        auto prev_type = selected.type_;
        selected.type_ = 5;

        (*chunk_)->shadowcast();

        if (prev_type == 0) {
            (*chunk_)->db_.reset();
        }

        render(pfrm);
    }

    if (player(app).key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    return null_scene();
}



MacroModule::Factory MacroModule::factory_(true);



} // namespace skyland
