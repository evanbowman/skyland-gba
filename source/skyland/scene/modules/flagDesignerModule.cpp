#include "flagDesignerModule.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "skyland/configure_island.hpp"




namespace skyland {



static const int canvas_start_x = 3;
static const int canvas_start_y = 3;
static const int view_shift = -18;


void FlagDesignerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);

    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            pfrm.set_tile(Layer::overlay, x + canvas_start_x, y + canvas_start_y, 492);
            pfrm.set_palette(Layer::overlay, x + canvas_start_x, y + canvas_start_y, 0);
        }
    }

    pfrm.load_tile0_texture("tilesheet");

    View v;
    v.set_center({0, view_shift});
    pfrm.screen().set_view(v);
    pfrm.screen().fade(0);

    // We use tiles to draw the large flag visualization, we need to re-order
    // the tiles based on the order of the player island's palette table.
    for (int i = 0; i < 16; ++i) {
        auto td = pfrm.extract_tile(Layer::overlay, 488 + i);
        const int y = 18;
        palette_[td.data_[0][0]] = 488 + i;
        if (td.data_[0][0] == 0) {
            // This is a transparent tile, so we draw an icon instead
            pfrm.set_tile(Layer::overlay, td.data_[0][0] + 1, y, 487);
        } else {
            pfrm.set_tile(Layer::overlay, td.data_[0][0] + 1, y, 488 + i);
            pfrm.set_palette(Layer::overlay, td.data_[0][0] + 1, y, 0);
        }

    }

    app.player_island().init_terrain(pfrm, 4);
    configure_island_from_codestring(pfrm, app.player_island(), "'((power-core 1 13))");

    app.player_island().repaint(pfrm);
    app.player_island().set_position({140, 370});


    auto data = pfrm.extract_tile(Layer::map_0, 105);
    for (int x = 0; x < 13; ++x) {
        for (int y = 0; y < 11; ++y) {
            flag_img_.pixels[x][y] = data.data_[x][y + 1];
        }
    }

    show(pfrm);
}



void FlagDesignerModule::show(Platform& pfrm)
{
    for (int y = 0; y < 11; ++y) {
        for (int x = 0; x < 13; ++x) {
            const auto t = palette_[flag_img_.pixels[x][y]];
            pfrm.set_tile(Layer::overlay,
                          x + canvas_start_x, y + canvas_start_y, t);
            pfrm.set_palette(Layer::overlay,
                             x + canvas_start_x, y + canvas_start_y, 0);
        }
    }

    vram_write_flag(pfrm, flag_img_);
}



ScenePtr<Scene> FlagDesignerModule::update(Platform& pfrm,
                                           App& app,
                                           Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::alt_1)) {
        color_++;
        color_ %= 16;
    }
    if (app.player().key_down(pfrm, Key::right) and cursor_.x < 12) {
        ++cursor_.x;
    }
    if (app.player().key_down(pfrm, Key::left) and cursor_.x > 0) {
        --cursor_.x;
    }
    if (app.player().key_down(pfrm, Key::up) and cursor_.y > 0) {
        --cursor_.y;
    }
    if (app.player().key_down(pfrm, Key::down) and cursor_.y < 10) {
        ++cursor_.y;
    }
    if (app.player().key_pressed(pfrm, Key::action_1)) {
        flag_img_.pixels[cursor_.x][cursor_.y] = color_;
        show(pfrm);
    }

    app.update_parallax(delta);

    app.player_island().update(pfrm, app, delta);

    return null_scene();
}



void FlagDesignerModule::display(Platform& pfrm, App&)
{
    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_position({
            -8 + cursor_.x * 8.f + 8 * canvas_start_x,
            2 + cursor_.y * 8.f + 8 * canvas_start_y + view_shift,
        });

    sprite.set_texture_index(62);
    sprite.set_priority(0);
    pfrm.screen().draw(sprite);
}



FlagDesignerModule::Factory FlagDesignerModule::factory_;



}
