#include "flagDesignerModule.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/save.hpp"




namespace skyland {



static const int canvas_start_x = 3;
static const int canvas_start_y = 3;
static const int view_shift = -18;



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void FlagDesignerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);
    pfrm.screen().fade(1.f);

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
    app.player_island().set_position({146, 370});


    show(pfrm, app);

    draw_rulers(pfrm);

    pfrm.screen().fade(0);
}



void FlagDesignerModule::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
}



void FlagDesignerModule::draw_rulers(Platform& pfrm)
{
    for (int i = 0; i < 13; ++i) {
        pfrm.set_tile(Layer::overlay, 3 + i, 2, i == cursor_.x ? 474 : 472);
    }
    for (int i = 0; i < 11; ++i) {
        pfrm.set_tile(Layer::overlay, 16, 3 + i, i == cursor_.y ? 473 : 471);
    }
}



void FlagDesignerModule::show(Platform& pfrm, App& app)
{
    for (int y = 0; y < 11; ++y) {
        for (int x = 0; x < 13; ++x) {
            const auto t = palette_[app.gp_.flag_img_.pixels[x][y]];
            pfrm.set_tile(Layer::overlay,
                          x + canvas_start_x, y + canvas_start_y, t);
            pfrm.set_palette(Layer::overlay,
                             x + canvas_start_x, y + canvas_start_y, 0);
        }
    }

    vram_write_flag(pfrm, app.gp_.flag_img_);
}



ScenePtr<Scene> FlagDesignerModule::update(Platform& pfrm,
                                           App& app,
                                           Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::alt_1)) {
        color_--;
        color_ %= 16;
        ready_ = true;
    }
    if (app.player().key_down(pfrm, Key::alt_2)) {
        color_++;
        color_ %= 16;
        ready_ = true;
    }
    bool cursor_move_ready = false;

    if (cursor_move_tic_ > 0) {
        cursor_move_tic_ -= delta;
        if (cursor_move_tic_ <= 0) {
            cursor_move_tic_ = 0;
            cursor_move_ready = true;
        }
    }
    if (cursor_move_ready) {
        if (app.player().key_pressed(pfrm, Key::right) and cursor_.x < 12) {
            ++cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers(pfrm);
        }
        if (app.player().key_pressed(pfrm, Key::left) and cursor_.x > 0) {
            --cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers(pfrm);
        }
        if (app.player().key_pressed(pfrm, Key::up) and cursor_.y > 0) {
            --cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers(pfrm);
        }
        if (app.player().key_pressed(pfrm, Key::down) and cursor_.y < 10) {
            ++cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers(pfrm);
        }
    } else {
        if (app.player().key_down(pfrm, Key::right) and cursor_.x < 12) {
            ++cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers(pfrm);
        }
        if (app.player().key_down(pfrm, Key::left) and cursor_.x > 0) {
            --cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers(pfrm);
        }
        if (app.player().key_down(pfrm, Key::up) and cursor_.y > 0) {
            --cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers(pfrm);
        }
        if (app.player().key_down(pfrm, Key::down) and cursor_.y < 10) {
            ++cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers(pfrm);
        }
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        ready_ = true;
    }
    if (app.player().key_down(pfrm, Key::action_2)) {
        save::store_global_data(pfrm, app.gp_);
        return scene_pool::alloc<TitleScreenScene>();
    }
    if (app.player().key_down(pfrm, Key::start)) {
        ready_ = true;
        u8 temp[16][16];
        for (int x = 0; x < FlagPixels::width; ++x) {
            for (int y = 0; y < FlagPixels::height; ++y) {
                temp[x][y] = app.gp_.flag_img_.pixels[x][y];
            }
        }
        flood_fill(pfrm, temp, color_, cursor_.x, cursor_.y);
        for (int x = 0; x < FlagPixels::width; ++x) {
            for (int y = 0; y < FlagPixels::height; ++y) {
                app.gp_.flag_img_.pixels[x][y] = temp[x][y];
            }
        }
        show(pfrm, app);
    }
    if (app.player().key_down(pfrm, Key::select)) {
        load_default_flag(pfrm, app);
        show(pfrm, app);
    }
    if (app.player().key_pressed(pfrm, Key::action_1) and ready_) {
        app.gp_.flag_img_.pixels[cursor_.x][cursor_.y] = color_;
        show(pfrm, app);
    }

    app.update_parallax(delta);

    app.player_island().update(pfrm, app, delta);

    update_entities(pfrm, app, delta, app.effects());
    for (auto& effect : app.effects()) {
        effect->update(pfrm, app, delta);
    }

    return null_scene();
}



void FlagDesignerModule::display(Platform& pfrm, App& app)
{
    app.player_island().display(pfrm);

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_position({
            -8 + cursor_.x * 8.f + 8 * canvas_start_x,
            2 + cursor_.y * 8.f + 8 * canvas_start_y + view_shift,
        });

    sprite.set_texture_index(62);
    sprite.set_priority(0);
    pfrm.screen().draw(sprite);


    sprite.set_texture_index(63);
    sprite.set_position({
            4 + color_ * 8.f,
            128 + view_shift
        });
    pfrm.screen().draw(sprite);
}



FlagDesignerModule::Factory FlagDesignerModule::factory_;



}
