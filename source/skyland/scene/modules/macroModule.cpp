#include "macroModule.hpp"
#include "skyland/player/player.hpp"
#include "skyland/skyland.hpp"



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

    if (player(app).key_down(pfrm, Key::alt_2)) {
        pfrm.screen().schedule_fade(
            0.8f, custom_color(0x102447), true, false, false);
        pfrm.screen().clear();
        pfrm.screen().display();
        (*chunk_)->rotate();
        render(pfrm);
        pfrm.screen().schedule_fade(
            0.f, ColorConstant::rich_black, true, false, false);
    }

    // if (player(app).key_pressed(pfrm, Key::down)) {
    //     scroll_ -= 0.7f;
    //     scroll_ = clamp(scroll_, 0.f, 50.f);
    //     pfrm.set_scroll(Layer::map_0, 0, scroll_);
    //     pfrm.set_scroll(Layer::map_1, 0, scroll_ + 8);
    // }

    // if (player(app).key_pressed(pfrm, Key::up)) {
    //     scroll_ += 0.7f;
    //     scroll_ = clamp(scroll_, 0.f, 50.f);
    //     pfrm.set_scroll(Layer::map_0, 0, scroll_);
    //     pfrm.set_scroll(Layer::map_1, 0, scroll_ + 8);
    // }

    if (player(app).key_pressed(pfrm, Key::left)) {
        --cursor_.x;
    }

    if (player(app).key_pressed(pfrm, Key::right)) {
        ++cursor_.x;
    }

    if (player(app).key_pressed(pfrm, Key::action_1)) {
        auto& selected = (*chunk_)->blocks_[0][cursor_.x][cursor_.y];
        auto prev_type = selected.type_;
        selected.type_ = 3;

        (*chunk_)->shadowcast();

        if (prev_type == 0) {
            (*chunk_)->db_.reset();
        }

        render(pfrm);
    }



    return null_scene();
}



MacroModule::Factory MacroModule::factory_(true);



} // namespace skyland
