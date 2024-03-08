////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "paint.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y);



void Paint::init()
{
    for (int x = 0; x < width_; ++x) {
        for (int y = 0; y < height_; ++y) {
            PLATFORM.set_tile(
                Layer::overlay, x + origin_x_, y + origin_y_, 492);
            PLATFORM.set_palette(
                Layer::overlay, x + origin_x_, y + origin_y_, 0);
        }
    }

    // We use tiles to draw the large flag visualization, we need to re-order
    // the tiles based on the order of the player island's palette table.
    for (int i = 0; i < 16; ++i) {
        auto td = PLATFORM.extract_tile(Layer::overlay, 488 + i);
        const int y = 18;
        palette_[td.data_[0][0]] = 488 + i;
        if (td.data_[0][0] == 0) {
            // This is a transparent tile, so we draw an icon instead
            PLATFORM.set_tile(Layer::overlay, td.data_[0][0] + 1, y, 487);
        } else {
            PLATFORM.set_tile(Layer::overlay, td.data_[0][0] + 1, y, 488 + i);
            PLATFORM.set_palette(Layer::overlay, td.data_[0][0] + 1, y, 0);
        }
    }

    View v;
    v.set_center({0, Float(view_shift_)});
    PLATFORM.screen().set_view(v);

    show();

    draw_rulers();
}



void Paint::draw_rulers()
{
    for (int i = 0; i < width_; ++i) {
        PLATFORM.set_tile(Layer::overlay,
                          (origin_x_) + i,
                          origin_y_ - 1,
                          i == cursor_.x ? 474 : 472);
    }
    for (int i = 0; i < height_; ++i) {
        PLATFORM.set_tile(Layer::overlay,
                          origin_x_ + width(),
                          origin_y_ + i,
                          i == cursor_.y ? 473 : 471);
    }
}



void Paint::show()
{
    for (int y = 0; y < height_; ++y) {
        for (int x = 0; x < width_; ++x) {
            const auto val = get_pixel(x, y);
            if (val >= 16) {
                Platform::fatal("pixel is not 4bit indexed color!");
            }
            const auto t = palette_[val];
            PLATFORM.set_tile(Layer::overlay, x + origin_x_, y + origin_y_, t);
            PLATFORM.set_palette(
                Layer::overlay, x + origin_x_, y + origin_y_, 0);
        }
    }
}



ScenePtr Paint::update(Time delta)
{
    if (APP.player().key_down(Key::alt_1)) {
        color_--;
        color_ %= 16;
        ready_ = true;
    }
    if (APP.player().key_down(Key::alt_2)) {
        color_++;
        color_ %= 16;
        ready_ = true;
    }
    bool cursor_move_ready = false;

    update_entities(delta, APP.birds());

    if (cursor_move_tic_ > 0) {
        cursor_move_tic_ -= delta;
        if (cursor_move_tic_ <= 0) {
            cursor_move_tic_ = 0;
            cursor_move_ready = true;
        }
    }
    if (cursor_move_ready) {
        if (APP.player().key_pressed(Key::right) and cursor_.x < (width_ - 1)) {
            ++cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers();
        }
        if (APP.player().key_pressed(Key::left) and cursor_.x > 0) {
            --cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers();
        }
        if (APP.player().key_pressed(Key::up) and cursor_.y > 0) {
            --cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers();
        }
        if (APP.player().key_pressed(Key::down) and cursor_.y < (height_ - 1)) {
            ++cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(90);
            draw_rulers();
        }
    } else {
        if (APP.player().key_down(Key::right) and cursor_.x < (width_ - 1)) {
            ++cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers();
        }
        if (APP.player().key_down(Key::left) and cursor_.x > 0) {
            --cursor_.x;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers();
        }
        if (APP.player().key_down(Key::up) and cursor_.y > 0) {
            --cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers();
        }
        if (APP.player().key_down(Key::down) and cursor_.y < (height_ - 1)) {
            ++cursor_.y;
            ready_ = true;
            cursor_move_tic_ = milliseconds(400);
            draw_rulers();
        }
    }

    if (APP.player().key_down(Key::action_1)) {
        ready_ = true;
    }

    if (APP.player().key_down(Key::start)) {
        ready_ = true;
        u8 temp[16][16];
        for (int x = 0; x < width(); ++x) {
            for (int y = 0; y < height(); ++y) {
                temp[x][y] = get_pixel(x, y);
            }
        }
        flood_fill(temp, color_, cursor_.x, cursor_.y);
        for (int x = 0; x < width(); ++x) {
            for (int y = 0; y < height(); ++y) {
                set_pixel(x, y, temp[x][y]);
            }
        }
        show();
    }
    if (APP.player().key_pressed(Key::action_1) and ready_) {
        set_pixel(cursor_.x, cursor_.y, color_);
        show();
    }

    APP.update_parallax(delta);

    return null_scene();
}



void Paint::display()
{
    APP.player_island().display();

    for (auto& effect : APP.effects()) {
        PLATFORM.screen().draw(effect->sprite());
    }

    for (auto& bird : APP.birds()) {
        PLATFORM.screen().draw(bird->sprite());
    }

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);

    auto vx = PLATFORM.screen().get_view().get_center().x;

    sprite.set_position({
        Fixnum::from_integer((-8 + cursor_.x * 8.f + 8 * origin_x_) + vx),
        Fixnum::from_integer(2 + cursor_.y * 8.f + 8 * origin_y_ + view_shift_),
    });

    sprite.set_texture_index(62);
    sprite.set_priority(0);
    PLATFORM.screen().draw(sprite);


    sprite.set_texture_index(63);
    sprite.set_position({Fixnum::from_integer(4 + color_ * 8.f + vx),
                         Fixnum::from_integer(128 + Float(view_shift_))});
    PLATFORM.screen().draw(sprite);
}



} // namespace skyland
