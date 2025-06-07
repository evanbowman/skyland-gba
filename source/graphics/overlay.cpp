////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "overlay.hpp"
#include "localization.hpp"
#include "string.hpp"



void UIMetric::set_value(u32 value)
{
    auto value_len = integer_text_length(value);

    if (format_ == Format::fraction or format_ == Format::fraction_p_m) {
        auto v1 = value & 0x0000ffff;
        auto v2 = (value & 0xffff0000) >> 16;
        value_len = integer_text_length(v1) + 1 + integer_text_length(v2);
        if (format_ == Format::fraction_p_m) {
            value_len += integer_text_length(v1 - v2);
            value_len += 1;
            value_len += 2;
        }
    } else if (format_ == Format::integer_with_rate) {
        auto v1 = value & 0x0000ffff;
        int v2 = (value & 0xffff0000) >> 16;
        value_len = integer_text_length(v1) + 1 + integer_text_length(v2);
        if (v2 > 0) {
            ++value_len;
        } else if (v2 == 0) {
            value_len += 2;
        }
        if (large_numerator_) {
            value_len += 1;
        }

        if (text_ and text_->len() > value_len) {
            ++value_len;
        }
    } else if (format_ == Format::signed_integer) {
        s32 value = value_;
        value_len = integer_text_length(value);
    }

    anim_.init(value_len);
    value_ = value;


    switch (align_) {
    case Align::left:
        anim_.set_position({u8(pos_.x + 1), pos_.y});
        break;

    case Align::right: {
        anim_.set_position({u8(pos_.x - value_len), pos_.y});
        break;
    }
    }
}



void UIMetric::display()

{
    switch (align_) {
    case Align::left: {
        icon_.emplace(icon_tile_, pos_);
        text_.emplace(OverlayCoord{u8(pos_.x + 1), pos_.y});
        break;
    }

    case Align::right: {
        const auto len = integer_text_length(value_);
        icon_.emplace(icon_tile_, pos_);
        text_.emplace(OverlayCoord{u8(pos_.x - len), pos_.y});
        break;
    }
    }


    if (anim_.width() < text_->len()) {
        for (int i = anim_.width(); i < text_->len(); ++i) {
            PLATFORM.set_tile(
                Layer::overlay, text_->coord().x + i, text_->coord().y, 0);
        }
    }


    if (format_ == Format::integer_with_rate) {
        auto v1 = value_ & 0x0000ffff;
        int v2 = (value_ & 0xffff0000) >> 16;
        text_->assign(v1);
        auto clr = Text::OptColors{
            {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

        if (large_numerator_) {
            text_->append("k");
        }

        text_->append(",", clr);

        if (v2 > 0) {
            text_->append("+", clr);
            text_->append(v2, clr);
        } else if (v2 == 0) {
            text_->append("+<1", clr);
        } else {
            text_->append(v2, clr);
        }


    } else if (format_ == Format::fraction or format_ == Format::fraction_p_m) {
        auto v1 = value_ & 0x0000ffff;
        auto v2 = (value_ & 0xffff0000) >> 16;

        Text::OptColors main_clr;

        auto clr = [&]() -> Text::OptColors {
            if (v1 < v2) {
                main_clr = Text::OptColors{{ColorConstant::rich_black,
                                            ColorConstant::aerospace_orange}};
                if (format_ == Format::fraction_p_m) {
                    return Text::OptColors{{ColorConstant::med_blue_gray,
                                            ColorConstant::rich_black}};
                } else {
                    return main_clr;
                }
            } else {
                if (format_ == Format::fraction_p_m) {
                    return Text::OptColors{{ColorConstant::med_blue_gray,
                                            ColorConstant::rich_black}};
                } else {
                    return std::nullopt;
                }
            }
        }();

        if (format_ == Format::fraction_p_m) {
            text_->append(v1 - v2, main_clr);
            text_->append(",", clr);
            text_->append("-", clr);
        }
        text_->append(v2, clr);

        if (format_ == Format::fraction_p_m) {
            text_->append(",", clr);
        } else {
            text_->append("/", clr);
        }

        if (format_ == Format::fraction_p_m) {
            text_->append("+", clr);
        }
        text_->append(v1, clr);


    } else if (format_ == Format::signed_integer) {
        text_->assign((s32)value_);
    } else {
        text_->assign(value_);
    }
}



u32 integer_text_length(int n)
{
    std::array<char, 40> buffer = {0};

    locale_num2str(n, buffer.data(), 10);

    return strlen(buffer.data());
}


Text::Text(const char* str,
           const OverlayCoord& coord,
           const FontConfiguration& config)
    : coord_(coord), len_(0), config_(config)
{
    this->assign(str);
}


Text::Text(const OverlayCoord& coord, const FontConfiguration& config)
    : coord_(coord), len_(0), config_(config)
{
}


void Text::erase()
{
    if (not config_.double_size_) {
        for (int i = 0; i < len_; ++i) {
            PLATFORM.set_tile(Layer::overlay, coord_.x + i, coord_.y, 0);
        }
    } else {
        for (int i = 0; i < len_ * 2; ++i) {
            PLATFORM.set_tile(Layer::overlay, coord_.x + i, coord_.y, 0);
        }
        for (int i = 0; i < len_ * 2; ++i) {
            PLATFORM.set_tile(Layer::overlay, coord_.x + i, coord_.y + 1, 0);
        }
    }

    len_ = 0;
}


Text::Text(Text&& from)
    : coord_(from.coord_), len_(from.len_), config_(from.config_)
{
    from.len_ = 0;
}


Text::~Text()
{
    this->erase();
}



void Text::print(const char* msg,
                 const OverlayCoord& coord,
                 const OptColors& colors)
{
    Text t(coord);
    t.append(msg, colors);
    t.__detach();
}



void Text::assign(int val, const OptColors& colors)
{
    std::array<char, 40> buffer = {0};

    locale_num2str(val, buffer.data(), 10);
    this->assign(buffer.data(), colors);
}


Platform::TextureCpMapper locale_texture_map();
Platform::TextureCpMapper locale_doublesize_texture_map();
Platform::TextureCpMapper locale_texture_map_transparent();


void print_double_char(utf8::Codepoint c,
                       const OverlayCoord& coord,
                       const Optional<FontColors>& colors = {})
{
    if (c not_eq 0) {
        const auto mapping_info = locale_doublesize_texture_map()(c);

        u16 t0 = 495;
        u16 t1 = 495;
        u16 t2 = 495;
        u16 t3 = 495;

        if (mapping_info) {
            auto info = *mapping_info;

            // FIXME: these special cases should be handled in the texture map
            // lookup.
            if (info.offset_ == 72) {
                t0 = PLATFORM.map_glyph(c, info);
                // Special case for space character
                t1 = t0;
                t2 = t0;
                t3 = t0;
            } else if (info.offset_ == 65) {
                // Special case for enlarged quote character
                info.offset_ = 523;
                t0 = PLATFORM.map_glyph(c, info);
                info.offset_ = 524;
                t1 = PLATFORM.map_glyph(c, info);
                info.offset_ = 72; // space
                t2 = PLATFORM.map_glyph(c, info);
                t3 = PLATFORM.map_glyph(c, info);
            } else if (info.offset_ == 38 or info.offset_ == 37) {
                // Special case for period, comma
                t2 = PLATFORM.map_glyph(c, info);
                info.offset_ = 72; // space
                t0 = PLATFORM.map_glyph(c, info);
                t1 = PLATFORM.map_glyph(c, info);
                t3 = PLATFORM.map_glyph(c, info);
            } else {
                t0 = PLATFORM.map_glyph(c, info);
                info.offset_++;
                t1 = PLATFORM.map_glyph(c, info);
                info.offset_++;
                t2 = PLATFORM.map_glyph(c, info);
                info.offset_++;
                t3 = PLATFORM.map_glyph(c, info);
            }
        }

        if (not colors) {
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, t0);
            PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y, t1);
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y + 1, t2);
            PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y + 1, t3);
        } else {
            PLATFORM.set_tile(coord.x, coord.y, t0, *colors);
            PLATFORM.set_tile(coord.x + 1, coord.y, t1, *colors);
            PLATFORM.set_tile(coord.x, coord.y + 1, t2, *colors);
            PLATFORM.set_tile(coord.x + 1, coord.y + 1, t3, *colors);
        }
    } else {
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, 0);
        PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y, 0);
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y + 1, 0);
        PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y + 1, 0);
    }
}



static bool text_icon_glyphs_enabled = true;



void enable_text_icon_glyphs(bool enable)
{
    text_icon_glyphs_enabled = enable;
}



void print_char(utf8::Codepoint c,
                const OverlayCoord& coord,
                const Optional<FontColors>& colors = {})
{
    if (text_icon_glyphs_enabled) {
        if (c == '@') {
            // Really bad hack, to show a full color coin icon in place of the
            // '@' char.
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, 146);
            return;
        }
        if (c == '`') {
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, 147);
            return;
        }
        if (c == (char)17) { // (device control 1 ascii char)
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, 422);
            return;
        }
        if (c == (char)18) { // (device control 2 ascii char)
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, 148);
            return;
        }
    }

    if (c not_eq 0) {
        auto mapping_info = locale_texture_map()(c);

        u16 t = 495;

        if (mapping_info) {

            t = PLATFORM.map_glyph(c, *mapping_info);
        }

        if (not colors) {
            PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, t);
        } else {
            PLATFORM.set_tile(coord.x, coord.y, t, *colors);
        }
    } else {
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, 0);
    }
}


void Text::resize(u32 len)
{
    for (int i = len - 1; i < this->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, coord_.x + i, coord_.y, 0);
    }
    len_ = 0;
}


void Text::assign(const char* str, const OptColors& colors)
{
    const auto new_len = utf8::len(str);
    if (len_ > new_len) {
        if (not config_.double_size_) {
            for (int i = new_len; i < len_; ++i) {
                PLATFORM.set_tile(Layer::overlay, coord_.x + i, coord_.y, 0);
            }
        } else {
            for (int i = new_len; i < len_ * 2; ++i) {
                PLATFORM.set_tile(Layer::overlay, coord_.x + i, coord_.y, 0);
            }
            for (int i = new_len; i < len_ * 2; ++i) {
                PLATFORM.set_tile(
                    Layer::overlay, coord_.x + i, coord_.y + 1, 0);
            }
        }
    }
    len_ = 0;


    this->append(str, colors);
}


void Text::append(const char* str, const OptColors& colors)
{
    if (str == nullptr) {
        return;
    }

    if (config_.double_size_) {
        auto write_pos = static_cast<u8>(coord_.x + len_ * 2);

        utf8::scan(
            [&](const utf8::Codepoint& cp, const char* raw, int) {
                print_double_char(cp, {write_pos, coord_.y}, colors);
                write_pos += 2;
                ++len_;
                return true;
            },
            str,
            strlen(str));

    } else {
        auto write_pos = static_cast<u8>(coord_.x + len_);

        utf8::scan(
            [&](const utf8::Codepoint& cp, const char* raw, int) {
                print_char(cp, {write_pos, coord_.y}, colors);
                ++write_pos;
                ++len_;
                return true;
            },
            str,
            strlen(str));
    }
}


void Text::append(int num, const OptColors& colors)
{
    std::array<char, 40> buffer = {0};

    locale_num2str(num, buffer.data(), 10);
    this->append(buffer.data(), colors);
}



SmallIcon::SmallIcon(int tile, const OverlayCoord& coord) : coord_(coord)
{
    PLATFORM.set_tile(Layer::overlay, coord_.x, coord_.y, tile);
}


SmallIcon::~SmallIcon()
{
    PLATFORM.set_tile(Layer::overlay, coord_.x, coord_.y, 0);
}


void MediumIcon::draw(int tile, const OverlayCoord& coord)
{
    PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, tile);
    PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y, tile + 1);
    PLATFORM.set_tile(Layer::overlay, coord.x, coord.y + 1, tile + 2);
    PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y + 1, tile + 3);
}


MediumIcon::MediumIcon(int tile, const OverlayCoord& coord) : coord_(coord)
{
    draw(tile, coord);
}


MediumIcon::~MediumIcon()
{
    PLATFORM.set_tile(Layer::overlay, coord_.x, coord_.y, 0);
    PLATFORM.set_tile(Layer::overlay, coord_.x + 1, coord_.y, 0);
    PLATFORM.set_tile(Layer::overlay, coord_.x, coord_.y + 1, 0);
    PLATFORM.set_tile(Layer::overlay, coord_.x + 1, coord_.y + 1, 0);
}


TextView::TextView() : parsed_(0)
{
}

TextView::~TextView()
{
    for (int i = position_.x; i < position_.x + size_.x; ++i) {
        for (int j = position_.y; j < position_.y + size_.y; ++j) {
            PLATFORM.set_tile(Layer::overlay, i, j, 0);
        }
    }
}


TextView::LineCount TextView::assign(const char* str,
                                     const OverlayCoord& coord,
                                     const OverlayCoord& size,
                                     int skiplines,
                                     const OptColors& colors)
{
    LineCount result = 1;

    position_ = coord;
    size_ = size;

    const auto len = strlen(str);
    const auto ulen = utf8::len(str);
    utf8::BufferedStr ustr(str, len);

    auto cursor = coord;

    auto newline = [&] {
        while (cursor.x < coord.x + size.x) {
            print_char(' ', cursor, colors);
            ++cursor.x;
        }
        cursor.x = coord.x;
        ++cursor.y;
    };

    size_t i;
    for (i = 0; i < ulen; ++i) {

        if (cursor.x == coord.x + size.x) {
            if (ustr.get(i) not_eq ' ') {
                // If the next character is not a space, then the current word
                // does not fit on the current line, and needs to be written
                // onto the next line instead.
                while (ustr.get(i) not_eq ' ') {
                    --i;
                    --cursor.x;
                }
                ++result;
                newline();
                if (cursor.y == (coord.y + size.y) - 1) {
                    break;
                }
                ++result;
                newline();
            } else {
                cursor.y += 1;
                cursor.x = coord.x;
                if (cursor.y == (coord.y + size.y) - 1) {
                    break;
                }
                ++result;
                newline();
            }
            if (skiplines) {
                --skiplines;
                cursor.y -= 2;
            }
        }

        if (cursor.x == coord.x and ustr.get(i) == ' ') {
            // ...
        } else {
            if (not skiplines) {
                print_char(ustr.get(i), cursor, colors);
            }

            ++cursor.x;
        }
    }

    parsed_ = i;

    while (cursor.y < (coord.y + size.y)) {
        newline();
    }

    return result;
}


Border::Border(const OverlayCoord& size,
               const OverlayCoord& position,
               bool fill,
               int tile_offset,
               TileDesc default_tile)
    : size_(size), position_(position), filled_(fill),
      default_tile_(default_tile)
{
    const auto stopx = position_.x + size_.x;
    const auto stopy = position_.y + size_.y;

    for (int x = position_.x; x < stopx; ++x) {
        for (int y = position_.y; y < stopy; ++y) {

            if (x == position_.x and y == position_.y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 86 + tile_offset);

            } else if (x == position_.x and y == stopy - 1) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 88 + tile_offset);

            } else if (x == stopx - 1 and y == position_.y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 85 + tile_offset);

            } else if (x == stopx - 1 and y == stopy - 1) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 87 + tile_offset);

            } else if (x == position_.x) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 84 + tile_offset);

            } else if (y == position_.y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 81 + tile_offset);

            } else if (x == stopx - 1) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 82 + tile_offset);

            } else if (y == stopy - 1) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 83 + tile_offset);

            } else if (fill) {
                PLATFORM.set_tile(Layer::overlay, x, y, 67 + 80);
            }
        }
    }
}


Border::~Border()
{
    const auto stopx = position_.x + size_.x;
    const auto stopy = position_.y + size_.y;

    for (int x = position_.x; x < stopx; ++x) {
        for (int y = position_.y; y < stopy; ++y) {

            // TODO: simplify this if/else
            if ((x == position_.x and y == position_.y) or
                (x == position_.x and y == stopy - 1) or
                (x == stopx - 1 and y == position_.y) or
                (x == stopx - 1 and y == stopy - 1)) {
                PLATFORM.set_tile(Layer::overlay, x, y, default_tile_);

            } else if (x == position_.x) {
                PLATFORM.set_tile(Layer::overlay, x, y, default_tile_);

            } else if (y == position_.y) {
                PLATFORM.set_tile(Layer::overlay, x, y, default_tile_);

            } else if (x == stopx - 1) {
                PLATFORM.set_tile(Layer::overlay, x, y, default_tile_);

            } else if (y == stopy - 1) {
                PLATFORM.set_tile(Layer::overlay, x, y, default_tile_);

            } else if (filled_) {
                PLATFORM.set_tile(Layer::overlay, x, y, default_tile_);
            }
        }
    }
}


BossHealthBar::BossHealthBar(u8 height, const OverlayCoord& position)
    : position_(position), height_(height)
{
    PLATFORM.set_tile(Layer::overlay, position_.x, position_.y, 82);
    PLATFORM.set_tile(
        Layer::overlay, position_.x, position_.y + height + 1, 83);
    set_health(0.f);
}


void BossHealthBar::set_health(Float percentage)
{
    constexpr int pixels_per_tile = 8;
    const auto total_pixels = height_ * pixels_per_tile;

    int fractional_pixels = percentage * total_pixels;
    int current_tile = 0;

    while (fractional_pixels >= 8) {
        PLATFORM.set_tile(
            Layer::overlay, position_.x, position_.y + 1 + current_tile, 91);
        fractional_pixels -= 8;
        ++current_tile;
    }

    if (current_tile < height_ and fractional_pixels % 8 not_eq 0) {
        PLATFORM.set_tile(Layer::overlay,
                          position_.x,
                          position_.y + 1 + current_tile,
                          83 + fractional_pixels % 8);
        ++current_tile;
    }

    while (current_tile < height_) {
        PLATFORM.set_tile(
            Layer::overlay, position_.x, position_.y + 1 + current_tile, 92);
        ++current_tile;
    }
}


BossHealthBar::~BossHealthBar()
{
    for (int y = 0; y < height_ + 2 /* +2 due to the header and footer */;
         ++y) {
        PLATFORM.set_tile(Layer::overlay, position_.x, position_.y + y, 0);
    }
}


LoadingBar::LoadingBar(u8 width, const OverlayCoord& position)
    : position_(position), width_(width)
{
    PLATFORM.set_tile(Layer::overlay, position_.x, position_.y, 401);
    PLATFORM.set_tile(
        Layer::overlay, position_.x + width + 1, position_.y, 411);

    set_progress(0.f);
}


LoadingBar::~LoadingBar()
{
    for (int x = 0; x < width_ + 5; ++x) {
        PLATFORM.set_tile(Layer::overlay, position_.x + x, position_.y, 0);
    }
}


void LoadingBar::set_progress(Float percentage)
{
    constexpr int pixels_per_tile = 8;
    const auto total_pixels = width_ * pixels_per_tile;

    int fractional_pixels = percentage * total_pixels;
    int current_tile = 0;

    while (fractional_pixels >= 8) {
        PLATFORM.set_tile(
            Layer::overlay, position_.x + 1 + current_tile, position_.y, 410);
        fractional_pixels -= 8;
        ++current_tile;
    }

    if (current_tile < width_ and fractional_pixels % 8 not_eq 0) {
        PLATFORM.set_tile(Layer::overlay,
                          position_.x + 1 + current_tile,
                          position_.y,
                          402 + fractional_pixels % 8);
        ++current_tile;
    }

    while (current_tile < width_) {
        PLATFORM.set_tile(
            Layer::overlay, position_.x + 1 + current_tile, position_.y, 402);
        ++current_tile;
    }
}


Sidebar::Sidebar(u8 width, u8 height, const OverlayCoord& pos)
    : width_(width), height_(height), pos_(pos)
{
}


void Sidebar::set_display_percentage(Float percentage)
{
    constexpr int pixels_per_tile = 8;
    const auto total_pixels = width_ * pixels_per_tile;

    const int fractional_pixels = percentage * total_pixels;

    // const auto screen_tiles = calc_screen_tiles(pfrm_);

    for (int y = pos_.y; y < pos_.y + height_; ++y) {
        int pixels = fractional_pixels;

        int current_tile = 0;

        while (pixels >= 8) {
            PLATFORM.set_tile(
                Layer::overlay, pos_.x - (1 + current_tile), y, 121);
            pixels -= 8;
            ++current_tile;
        }

        if (current_tile < width_ and pixels % 8 not_eq 0) {
            PLATFORM.set_tile(Layer::overlay,
                              pos_.x - (1 + current_tile),
                              y,
                              128 - pixels % 8);
            ++current_tile;
        }

        while (current_tile < width_) {
            PLATFORM.set_tile(
                Layer::overlay, pos_.x - (1 + current_tile), y, 0);
            ++current_tile;
        }
    }
}


Sidebar::~Sidebar()
{
    set_display_percentage(0.f);
}


LeftSidebar::LeftSidebar(u8 width, u8 height, const OverlayCoord& pos)
    : width_(width), height_(height), pos_(pos)
{
}


void LeftSidebar::set_display_percentage(Float percentage)
{
    constexpr int pixels_per_tile = 8;
    const auto total_pixels = width_ * pixels_per_tile;

    const int fractional_pixels = percentage * total_pixels;

    // const auto screen_tiles = calc_screen_tiles(pfrm_);

    for (int y = pos_.y; y < pos_.y + height_; ++y) {
        int pixels = fractional_pixels;

        int current_tile = 0;

        while (pixels >= 8) {
            PLATFORM.set_tile(Layer::overlay, current_tile + pos_.x, y, 121);
            pixels -= 8;
            ++current_tile;
        }

        if (current_tile < width_ and pixels % 8 not_eq 0) {
            PLATFORM.set_tile(
                Layer::overlay, current_tile + pos_.x, y, 433 - pixels % 8);
            ++current_tile;
        }

        while (current_tile < width_) {
            PLATFORM.set_tile(Layer::overlay, current_tile + pos_.x, y, 0);
            ++current_tile;
        }
    }
}


LeftSidebar::~LeftSidebar()
{
    set_display_percentage(0.f);
}
