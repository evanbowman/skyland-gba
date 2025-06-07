////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "paint.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



Vec2<u8> Paint::cursor_;
u32 Paint::color_ = 0;
Paint::Tool Paint::tool_ = Paint::Tool::pen;



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y);



void Paint::init()
{
    history_ = allocate_dynamic<HistoryBuffer>("paint-history");

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

    if (tool_ == Tool::exit or tool_ == Tool::preset) {
        tool_ = Tool::pen;
    }

    draw_rulers();
    show_color_name();
    show_toolbar();

    cursor_.x = clamp((int)cursor_.x, 0, (int)width_ - 1);
    cursor_.y = clamp((int)cursor_.y, 0, (int)height_ - 1);
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



bool Paint::undo(bool repaint)
{
    if ((*history_)->empty()) {
        return false;
    }

    auto last = (*history_)->back();
    (*history_)->pop_back();

    switch (last.type_) {
    case 0:
        set_pixel(last.pen_.x_, last.pen_.y_, last.pen_.prev_color_);
        break;

    case 1:
        switch (last.drag_.dir_) {
        case 0:
            apply_drag(-1, 0, false);
            break;

        case 1:
            apply_drag(1, 0, false);
            break;

        case 2:
            apply_drag(0, 1, false);
            break;

        case 3:
            apply_drag(0, -1, false);
            break;
        }
        break;

    case 2:
        if ((*history_)->size() < last.bucket_.pixels_filled_) {
            // Part of the PixelChanged entries associated with this bucket fill
            // were aged out of the buffer, so the bucket fill cannot be fully
            // reversed. It will never be possible to fully reverse the effect,
            // due to aged out entries, so we might as well clear the history.
            (*history_)->clear();
            return false;
        }
        for (int i = 0; i < last.bucket_.pixels_filled_; ++i) {
            undo(false);
        }
        break;
    }

    if (repaint) {
        show();
    }

    return true;
}



void Paint::push_history(HistoryEntry h)
{
    if ((*history_)->full()) {
        (*history_)->erase((*history_)->begin());
    }
    (*history_)->push_back(h);
}



void Paint::push_history(HistoryEntry::PixelChanged p)
{
    HistoryEntry h;
    h.type_ = 0;
    h.pen_ = p;
    push_history(h);
}



void Paint::push_history(HistoryEntry::CanvasDragged d)
{
    HistoryEntry h;
    h.type_ = 1;
    h.drag_ = d;
    push_history(h);
}



void Paint::push_history(HistoryEntry::BucketFill b)
{
    HistoryEntry h;
    h.type_ = 2;
    h.bucket_ = b;
    push_history(h);
}



void Paint::show_color_name()
{
    static const char* color_names[16] = {
        "transparent",
        "prussian blue",
        "blue gray",
        "pale gray",
        "gamboge",
        "vermillion",
        "indian red",
        "emerald green",
        "pale green",
        "warm white",
        "light blue",
        "electric blue",
        "cobalt blue",
        "naples yellow",
        "sage",
        "olive green",
    };
    for (int i = 2; i < 16; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 19, 0);
    }
    static const FontColors shade[16] = {
        {ColorConstant::silver_white, custom_color(0x163061)},
        {ColorConstant::silver_white, custom_color(0x163061)},
        {ColorConstant::silver_white, custom_color(0x666691)},
        {custom_color(0x163061), custom_color(0x9fb7c5)},
        {custom_color(0x163061), custom_color(0xe6b220)},
        {ColorConstant::silver_white, custom_color(0xe24920)},
        {ColorConstant::silver_white, custom_color(0x6e2d4a)},
        {ColorConstant::silver_white, custom_color(0x277b6e)},
        {custom_color(0x163061), custom_color(0xb8ea80)}, // pale green
        {custom_color(0x163061), custom_color(0xf2f5eb)}, // warm white
        {custom_color(0x163061), custom_color(0xa2dfe8)},
        {custom_color(0x163061), custom_color(0x66fff7)},
        {ColorConstant::silver_white, custom_color(0x165fce)},
        {custom_color(0x163061), custom_color(0xd9e2a3)},
        {custom_color(0x163061), custom_color(0xa9b07f)},
        {ColorConstant::silver_white, custom_color(0x6b6b39)},
    };
    StringBuffer<16> txt = " ";
    txt += color_names[color_];
    while (not txt.full()) {
        txt.push_back(' ');
    }
    FontColors c;
    c.foreground_ = shade[color_].foreground_;
    auto inp_bkg = shade[color_].background_;
    c.background_ = APP.environment().shader()(
        ShaderPalette::tile0, ColorConstant(inp_bkg), 0, color_);
    Text::print(txt.c_str(), OverlayCoord{1, 19}, c);
}



void Paint::show_toolbar()
{
    const int icon_metatile_size = 4;
    const int icon_count = (int)Tool::count;
    const int tcount = icon_metatile_size * icon_count;

    const char* txtr = "paint_icons";

    const int unsel_vram_offset = 181;
    const int sel_vram_offset = 213;
    const int sel_src_offset = 14 * icon_metatile_size;

    if (copy_tool_txtr_) {
        copy_tool_txtr_ = false;
        PLATFORM.load_overlay_chunk(unsel_vram_offset, 0, tcount, txtr);
        PLATFORM.load_overlay_chunk(
            sel_vram_offset, sel_src_offset, tcount, txtr);
        PLATFORM.load_overlay_chunk(258, icon_metatile_size * 28, 23, txtr);
    }

    MediumIcon::draw(258, OverlayCoord{26, 2});
    MediumIcon::draw(262, OverlayCoord{28, 2});
    PLATFORM.set_tile(Layer::overlay, 27, 4, 258 + 8);

    for (int x = 0; x < 19; ++x) {
        if (x < 1 or x > 16) {
            PLATFORM.set_tile(Layer::overlay, x, 18, 258 + 9);
            PLATFORM.set_tile(Layer::overlay, x, 19, 258 + 11);
        }
    }
    PLATFORM.set_tile(Layer::overlay, 18, 18, 258 + 12);
    PLATFORM.set_tile(Layer::overlay, 18, 19, 258 + 13);

    PLATFORM.set_tile(Layer::overlay, 1, 18, 487); // transparent color

    for (int i = 0; i < icon_count; ++i) {
        OverlayCoord coord;
        coord.x = 28;
        coord.y = 4 + i * 2;
        if (i == (int)tool_) {
            MediumIcon::draw(sel_vram_offset + i * icon_metatile_size, coord);
        } else {
            MediumIcon::draw(unsel_vram_offset + i * icon_metatile_size, coord);
        }
    }
    show_tool_name();
}



void Paint::show_preview()
{
    u8 buffer[16][16];
    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            buffer[x][y] = get_pixel(x, y);
        }
    }

    auto enc = PLATFORM.encode_tile(buffer);
    PLATFORM.overwrite_overlay_tile(131, enc);


    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            buffer[x][y] = get_pixel(x + 8, y);
        }
    }

    enc = PLATFORM.encode_tile(buffer);
    PLATFORM.overwrite_overlay_tile(132, enc);


    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            buffer[x][y] = get_pixel(x, y + 8);
        }
    }

    enc = PLATFORM.encode_tile(buffer);
    PLATFORM.overwrite_overlay_tile(133, enc);


    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            buffer[x][y] = get_pixel(x + 8, y + 8);
        }
    }

    enc = PLATFORM.encode_tile(buffer);
    PLATFORM.overwrite_overlay_tile(134, enc);

    PLATFORM.set_tile(Layer::overlay, 21, 2, 131, 0);
    PLATFORM.set_tile(Layer::overlay, 22, 2, 132, 0);
    PLATFORM.set_tile(Layer::overlay, 21, 3, 133, 0);
    PLATFORM.set_tile(Layer::overlay, 22, 3, 134, 0);

    PLATFORM.set_tile(Layer::overlay, 19, 0, 258 + 15);
    PLATFORM.set_tile(Layer::overlay, 24, 0, 258 + 16);
    PLATFORM.set_tile(Layer::overlay, 19, 5, 258 + 17);
    PLATFORM.set_tile(Layer::overlay, 24, 5, 258 + 18);

    for (int x = 20; x < 24; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 0, 258 + 19);
        PLATFORM.set_tile(Layer::overlay, x, 5, 258 + 22);
    }

    for (int y = 1; y < 5; ++y) {
        PLATFORM.set_tile(Layer::overlay, 19, y, 258 + 20);
        PLATFORM.set_tile(Layer::overlay, 24, y, 258 + 21);
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

    if (preview_) {
        show_preview();
    }
}



void Paint::apply_drag(int xo, int yo, bool record_history)
{
    if (xo > 0) {
        u8 last_col[16];
        for (int y = 0; y < height(); ++y) {
            last_col[y] = get_pixel(width() - 1, y);
        }
        for (int x = width() - 1; x > 0; --x) {
            for (int y = 0; y < height(); ++y) {
                set_pixel(x, y, get_pixel(x - 1, y));
            }
        }
        for (int y = 0; y < height(); ++y) {
            set_pixel(0, y, last_col[y]);
        }
        if (record_history) {
            show();
            push_history(HistoryEntry::CanvasDragged{.dir_ = 0});
        }
    }
    if (xo < 0) {
        u8 first_col[16];
        for (int y = 0; y < height(); ++y) {
            first_col[y] = get_pixel(0, y);
        }
        for (int x = 0; x < width() - 1; ++x) {
            for (int y = 0; y < height(); ++y) {
                set_pixel(x, y, get_pixel(x + 1, y));
            }
        }
        for (int y = 0; y < height(); ++y) {
            set_pixel(width() - 1, y, first_col[y]);
        }
        if (record_history) {
            show();
            push_history(HistoryEntry::CanvasDragged{.dir_ = 1});
        }
    }
    if (yo < 0) {
        u8 first_row[16];
        for (int x = 0; x < width(); ++x) {
            first_row[x] = get_pixel(x, 0);
        }
        for (int x = 0; x < width(); ++x) {
            for (int y = 0; y < height() - 1; ++y) {
                set_pixel(x, y, get_pixel(x, y + 1));
            }
        }
        for (int x = 0; x < width(); ++x) {
            set_pixel(x, height() - 1, first_row[x]);
        }
        if (record_history) {
            show();
            push_history(HistoryEntry::CanvasDragged{.dir_ = 2});
        }
    }
    if (yo > 0) {
        u8 last_row[16];
        for (int x = 0; x < width(); ++x) {
            last_row[x] = get_pixel(x, height() - 1);
        }
        for (int y = height() - 1; y > 0; --y) {
            for (int x = 0; x < width(); ++x) {
                set_pixel(x, y, get_pixel(x, y - 1));
            }
        }
        for (int x = 0; x < width(); ++x) {
            set_pixel(x, 0, last_row[x]);
        }
        if (record_history) {
            show();
            push_history(HistoryEntry::CanvasDragged{.dir_ = 3});
        }
    }
}



ScenePtr Paint::update(Time delta)
{
    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::alt_1)) {
        color_--;
        color_ %= 16;
        ready_ = true;
        show_color_name();
        flicker_on_ = false;
        cursor_flicker_ = 0;
    }
    if (test_key(Key::alt_2)) {
        color_++;
        color_ %= 16;
        ready_ = true;
        show_color_name();
        flicker_on_ = false;
        cursor_flicker_ = 0;
    }
    bool cursor_move_ready = false;

    update_entities(delta, APP.birds());

    if (not flicker_on_) {
        if (++cursor_flicker_ == 96) {
            flicker_on_ = true;
            cursor_flicker_ = 0;
        }
    } else {
        if (++cursor_flicker_ == 32) {
            cursor_flicker_ = 0;
        }
    }

    switch (mode_) {
    case Mode::draw: {
        if (APP.player().key_down(Key::action_2)) {
            std::swap(tool_, last_tool_);
            show_toolbar();
            return null_scene();
        }

        if (APP.player().key_down(Key::select)) {
            tool_ = (Tool)((int)tool_ + 1);
            if ((int)tool_ > (int)Tool::drag) {
                tool_ = Tool::pen;
            }
            show_toolbar();
        }

        if (cursor_move_tic_ > 0) {
            cursor_move_tic_ -= delta;
            if (cursor_move_tic_ <= 0) {
                cursor_move_tic_ = 0;
                cursor_move_ready = true;
            }
        }

        auto on_move = [&](int xo, int yo) {
            flicker_on_ = false;
            cursor_flicker_ = 0;
            if (tool_ == Tool::drag) {
                apply_drag(xo, yo);
            }
            show_tool_name();
        };

        if (cursor_move_ready) {
            if (APP.player().key_pressed(Key::right) and
                cursor_.x < (width_ - 1)) {
                ++cursor_.x;
                ready_ = true;
                cursor_move_tic_ = milliseconds(90);
                draw_rulers();
                on_move(1, 0);
            }
            if (APP.player().key_pressed(Key::left) and cursor_.x > 0) {
                --cursor_.x;
                ready_ = true;
                cursor_move_tic_ = milliseconds(90);
                draw_rulers();
                on_move(-1, 0);
            }
            if (APP.player().key_pressed(Key::up) and cursor_.y > 0) {
                --cursor_.y;
                ready_ = true;
                cursor_move_tic_ = milliseconds(90);
                draw_rulers();
                on_move(0, -1);
            }
            if (APP.player().key_pressed(Key::down) and
                cursor_.y < (height_ - 1)) {
                ++cursor_.y;
                ready_ = true;
                cursor_move_tic_ = milliseconds(90);
                draw_rulers();
                on_move(0, 1);
            }
        } else {
            if (APP.player().key_down(Key::right) and
                cursor_.x < (width_ - 1)) {
                ++cursor_.x;
                ready_ = true;
                cursor_move_tic_ = milliseconds(400);
                draw_rulers();
                on_move(1, 0);
            }
            if (APP.player().key_down(Key::left) and cursor_.x > 0) {
                --cursor_.x;
                ready_ = true;
                cursor_move_tic_ = milliseconds(400);
                draw_rulers();
                on_move(-1, 0);
            }
            if (APP.player().key_down(Key::up) and cursor_.y > 0) {
                --cursor_.y;
                ready_ = true;
                cursor_move_tic_ = milliseconds(400);
                draw_rulers();
                on_move(0, -1);
            }
            if (APP.player().key_down(Key::down) and
                cursor_.y < (height_ - 1)) {
                ++cursor_.y;
                ready_ = true;
                cursor_move_tic_ = milliseconds(400);
                draw_rulers();
                on_move(0, 1);
            }
        }

        if (APP.player().key_down(Key::action_1)) {
            ready_ = true;
        }

        if (APP.player().key_down(Key::start)) {
            mode_ = Mode::tool_select;
            last_tool_ = tool_;
            flicker_on_ = true;
            cursor_flicker_ = 0;
        }

        auto do_set_pixel = [this](u8 x, u8 y, u8 v) {
            auto current_color = get_pixel(x, y);
            if (current_color not_eq v) {
                push_history(HistoryEntry::PixelChanged{
                    .x_ = x, .y_ = y, .prev_color_ = current_color});
            }
            set_pixel(x, y, v);
        };

        if (APP.player().key_pressed(Key::action_1) and ready_) {
            switch (tool_) {
            case Tool::drag:
                break;

            case Tool::undo:
                break;

            case Tool::exit:
                break;

            case Tool::preset:
                break;

            case Tool::count:
            case Tool::pen: {
                do_set_pixel(cursor_.x, cursor_.y, color_);
                show();
                break;
            }

            case Tool::bucket: {
                ready_ = false;
                u8 temp[16][16];
                for (int x = 0; x < width(); ++x) {
                    for (int y = 0; y < height(); ++y) {
                        temp[x][y] = get_pixel(x, y);
                    }
                }
                flood_fill(temp, color_, cursor_.x, cursor_.y);
                u8 change_count = 0;
                for (int x = 0; x < width(); ++x) {
                    for (int y = 0; y < height(); ++y) {
                        if (get_pixel(x, y) not_eq temp[x][y]) {
                            ++change_count;
                        }
                        do_set_pixel(x, y, temp[x][y]);
                    }
                }
                if (change_count) {
                    push_history(HistoryEntry::BucketFill{.pixels_filled_ =
                                                              change_count});
                }
                show();
                break;
            }
            }
        }
        break;
    }

    case Mode::tool_select: {
        if (APP.player().key_down(Key::up)) {
            if (tool_ == (Tool)0) {
                tool_ = (Tool)(((int)Tool::count) - 1);
            } else {
                tool_ = (Tool)((int)tool_ - 1);
            }
            show_toolbar();
        }
        if (APP.player().key_down(Key::down)) {
            if (tool_ == (Tool)((int)Tool::count - 1)) {
                tool_ = (Tool)0;
            } else {
                tool_ = (Tool)((int)tool_ + 1);
            }
            show_toolbar();
        }

        if ((tool_ == Tool::exit or tool_ == Tool::preset) and
            APP.player().key_down(Key::action_1)) {
            break;
        }

        if (tool_ == Tool::undo and APP.player().key_down(Key::action_1)) {
            if (not undo()) {
                PLATFORM.speaker().play_sound("beep_error", 4);
            }
            break;
        }

        if ((APP.player().key_down(Key::action_1) or
             APP.player().key_down(Key::action_2) or
             APP.player().key_down(Key::start))) {
            if (APP.player().key_down(Key::action_2) or tool_ == Tool::undo or
                tool_ == Tool::exit) {
                tool_ = last_tool_;
                show_toolbar();
            }
            mode_ = Mode::draw;
            ready_ = false;
        }
        break;
    }
    }

    APP.update_parallax(delta);

    return null_scene();
}



void Paint::display()
{
    if (draw_world_) {
        APP.player_island().display();

        for (auto& effect : APP.effects()) {
            PLATFORM.screen().draw(effect->sprite());
        }

        for (auto& bird : APP.birds()) {
            PLATFORM.screen().draw(bird->sprite());
        }
    }

    Sprite sprite;

    if (mode_ == Mode::tool_select) {
        sprite.set_alpha(Sprite::Alpha::transparent);
    }

    auto vx = PLATFORM.screen().get_view().get_center().x;

    auto dark_cursor = [this] {
        if (flicker_on_) {
            return cursor_flicker_ < 16;
        }
        auto p = get_pixel(cursor_.x, cursor_.y);
        return p == 3 or p == 4 or p == 8 or p == 9 or p == 10 or p == 11 or
               p == 13 or p == 14;
    };

    sprite.set_priority(0);

    sprite.set_position({
        Fixnum::from_integer((-8 + cursor_.x * 8.f + 8 * origin_x_) + vx),
        Fixnum::from_integer(1 + cursor_.y * 8.f + 8 * origin_y_ + view_shift_),
    });

    switch (tool_) {
    case Tool::count:
        break;

    case Tool::preset:
    case Tool::exit:
    case Tool::undo:
        break;

    case Tool::pen:
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(62, 0);
        PLATFORM.screen().draw(sprite);
        break;

    case Tool::bucket:
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(34, 1);
        PLATFORM.screen().draw(sprite);
        break;

    case Tool::drag:
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(99, 0);
        PLATFORM.screen().draw(sprite);
        break;
    }

    sprite.set_position({
        Fixnum::from_integer((cursor_.x * 8.f + 8 * origin_x_) + vx),
        Fixnum::from_integer(cursor_.y * 8.f + 8 * origin_y_ + view_shift_),
    });

    sprite.set_size(Sprite::Size::w8_h8);
    sprite.set_tidx_8x8(51, dark_cursor() ? 4 : 5);

    PLATFORM.screen().draw(sprite);

    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_alpha(Sprite::Alpha::opaque);

    sprite.set_texture_index(63);
    sprite.set_position({Fixnum::from_integer(4 + color_ * 8.f + vx),
                         Fixnum::from_integer(128 + Float(view_shift_))});
    PLATFORM.screen().draw(sprite);

    if (mode_ == Mode::tool_select) {
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_tidx_16x16(59, 0);
        auto offset = sine8(tool_selector_anim_) >> 6;
        tool_selector_anim_ += 4;
        sprite.set_position(
            {Fixnum::from_integer(vx + 28 * 8 - 13 + offset),
             Fixnum::from_integer(2 + view_shift_ + (4 + (int)tool_ * 2) * 8)});
        PLATFORM.screen().draw(sprite);
    }
}



void Paint::show_tool_name()
{
    const char* names[(int)Tool::count] = {
        "pen",
        "fill",
        "drag",
        "undo",
        "presets",
        "exit",
    };

    for (int x = 0; x < 9; ++x) {
        PLATFORM.set_tile(Layer::overlay, 21 + x, 19, 0);
        PLATFORM.set_tile(Layer::overlay, 21 + x, 18, 0);
    }

    StringBuffer<9> temp_str = names[(int)tool_];

    if (tool_ == Tool::pen or tool_ == Tool::bucket) {
        temp_str += ":";
        temp_str += stringify(cursor_.x);
        temp_str += ",";
        temp_str += stringify(cursor_.y);
    }

    u8 name_x = 30 - temp_str.length();
    Text::print(temp_str.c_str(), OverlayCoord{name_x, 19});

    for (int x = name_x; x < 30; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 18, 425);
    }
}



} // namespace skyland
