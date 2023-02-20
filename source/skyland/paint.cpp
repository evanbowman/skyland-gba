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


#include "paint.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void Paint::init(Platform& pfrm, App& app)
{
    for (int x = 0; x < width_; ++x) {
        for (int y = 0; y < height_; ++y) {
            pfrm.set_tile(Layer::overlay, x + origin_x_, y + origin_y_, 492);
            pfrm.set_palette(Layer::overlay, x + origin_x_, y + origin_y_, 0);
        }
    }

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

    View v;
    v.set_center({0, Float(view_shift_)});
    pfrm.screen().set_view(v);

    show(pfrm, app);

    draw_rulers(pfrm);
}



void Paint::draw_rulers(Platform& pfrm)
{
    for (int i = 0; i < width_; ++i) {
        pfrm.set_tile(Layer::overlay,
                      (origin_x_) + i,
                      origin_y_ - 1,
                      i == cursor_.x ? 474 : 472);
    }
    for (int i = 0; i < height_; ++i) {
        pfrm.set_tile(Layer::overlay,
                      origin_x_ + width(),
                      origin_y_ + i,
                      i == cursor_.y ? 473 : 471);
    }
}



void Paint::show(Platform& pfrm, App& app)
{
    for (int y = 0; y < height_; ++y) {
        for (int x = 0; x < width_; ++x) {
            const auto val = get_pixel(app, x, y);
            if (val >= 16) {
                Platform::fatal("pixel is not 4bit indexed color!");
            }
            const auto t = palette_[val];
            pfrm.set_tile(Layer::overlay, x + origin_x_, y + origin_y_, t);
            pfrm.set_palette(Layer::overlay, x + origin_x_, y + origin_y_, 0);
        }
    }
}



ScenePtr<Scene> Paint::update(Platform& pfrm, App& app, Microseconds delta)
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

    update_entities(pfrm, app, delta, app.birds());

    if (cursor_move_tic_ > 0) {
        cursor_move_tic_ -= delta;
        if (cursor_move_tic_ <= 0) {
            cursor_move_tic_ = 0;
            cursor_move_ready = true;
        }
    }
    if (cursor_move_ready) {
        if (app.player().key_pressed(pfrm, Key::right) and
            cursor_.x < (width_ - 1)) {
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
        if (app.player().key_pressed(pfrm, Key::down) and
            cursor_.y < (height_ - 1)) {
            ++cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers(pfrm);
        }
    } else {
        if (app.player().key_down(pfrm, Key::right) and
            cursor_.x < (width_ - 1)) {
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
        if (app.player().key_down(pfrm, Key::down) and
            cursor_.y < (height_ - 1)) {
            ++cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers(pfrm);
        }
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        ready_ = true;
    }

    if (app.player().key_down(pfrm, Key::start)) {
        ready_ = true;
        u8 temp[16][16];
        for (int x = 0; x < width(); ++x) {
            for (int y = 0; y < height(); ++y) {
                temp[x][y] = get_pixel(app, x, y);
            }
        }
        flood_fill(pfrm, temp, color_, cursor_.x, cursor_.y);
        for (int x = 0; x < width(); ++x) {
            for (int y = 0; y < height(); ++y) {
                set_pixel(app, x, y, temp[x][y]);
            }
        }
        show(pfrm, app);
    }
    if (app.player().key_pressed(pfrm, Key::action_1) and ready_) {
        set_pixel(app, cursor_.x, cursor_.y, color_);
        show(pfrm, app);
    }

    app.update_parallax(delta);

    return null_scene();
}



void Paint::display(Platform& pfrm, App& app)
{
    app.player_island().display(pfrm);

    for (auto& effect : app.effects()) {
        pfrm.screen().draw(effect->sprite());
    }

    for (auto& bird : app.birds()) {
        pfrm.screen().draw(bird->sprite());
    }

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_position({
        Fixnum::from_integer(-8 + cursor_.x * 8.f + 8 * origin_x_),
        Fixnum::from_integer(2 + cursor_.y * 8.f + 8 * origin_y_ + view_shift_),
    });

    sprite.set_texture_index(62);
    sprite.set_priority(0);
    pfrm.screen().draw(sprite);


    sprite.set_texture_index(63);
    sprite.set_position({Fixnum::from_integer(4 + color_ * 8.f),
                         Fixnum::from_integer(128 + Float(view_shift_))});
    pfrm.screen().draw(sprite);
}



} // namespace skyland
