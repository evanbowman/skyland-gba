////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "platform/platform.hpp"


// A collection of display routines for printing various things to the game's
// overlay drawing layer. The game draws the overlay with the highest level of
// priority, so everything else, both entities and the game map, will be drawn
// beneath. Unlike entities or the overworld map, the overlay is not
// positionable, but instead, purely tile-based. The overlay uses absolute
// coordinates, so when the screen's view scrolls to different areas of the
// overworld, the overlay stays put.
//
// The overlay uses 8x8 tiles; smaller than any other graphics element. But a UI
// sometimes demands more flexibility and precision than sprites and
// backgrounds, e.g.: displaying text.


using OverlayCoord = Vec2<u8>;


struct FontConfiguration
{

    // Double-sized text configuration added during chinese language
    // localization, when we determined that chinese glyphs would not be
    // legible at 8x8 sizes. When set to true, the text engine will use four
    // tiles per glyph, i.e. each glyph will be 16x16.
    bool double_size_ = false;
};


using OptColors = Optional<FontColors>;


class Text
{
public:
    Text(const char* str,
         const OverlayCoord& coord,
         const FontConfiguration& config = {});

    Text(const OverlayCoord& coord, const FontConfiguration& config = {});

    Text(const Text&) = delete;
    Text(Text&&);

    static void print(const char* msg,
                      const OverlayCoord& coord,
                      const OptColors& colors = {});

    ~Text();

    using OptColors = ::OptColors;

    void assign(const char* str, const OptColors& colors = {});
    void assign(int num, const OptColors& colors = {});

    void append(const char* str, const OptColors& colors = {});
    void append(int num, const OptColors& colors = {});

    void erase();

    using Length = u16;

    Length len()
    {
        return len_;
    }

    const OverlayCoord& coord() const
    {
        return coord_;
    }

    void set_coord(const OverlayCoord& coord)
    {
        erase();
        coord_ = coord;
    }

    const FontConfiguration& config() const
    {
        return config_;
    }

    // After calling __detach(), the text element will not clear its contents
    // from the screen when destroyed.
    void __detach()
    {
        len_ = 0;
    }


private:
    void resize(u32 len);

    OverlayCoord coord_;
    Length len_;
    const FontConfiguration config_;
};


// 8x8 pixels
class SmallIcon
{
public:
    SmallIcon(int tile, const OverlayCoord& coord);
    SmallIcon(const SmallIcon&) = delete;
    ~SmallIcon();

private:
    const OverlayCoord coord_;
};


// 16x16 pixels, comprised of four 8x8 tiles
class MediumIcon
{
public:
    MediumIcon(int tile, const OverlayCoord& coord);
    MediumIcon(const MediumIcon&) = delete;
    ~MediumIcon();

    static void draw(int tile, const OverlayCoord& coord);

    inline const auto& coord() const
    {
        return coord_;
    }

private:
    const OverlayCoord coord_;
};



void enable_text_icon_glyphs(bool enable);



// Unlike Text, TextView understands words, reflows words onto new lines, and is
// capable of scrolling vertically through a block of text.
class TextView
{
public:
    TextView();
    TextView(const TextView&) = delete;
    ~TextView();

    using OptColors = ::OptColors;

    using LineCount = int;

    // Use the skiplines parameter to scroll the textview vertically.
    LineCount assign(const char* str,
                     const OverlayCoord& coord,
                     const OverlayCoord& size,
                     int skiplines = 0,
                     const OptColors& opts = {});

    inline const OverlayCoord& size()
    {
        return size_;
    }

    // Returns the amount of the input string was processed by the textview
    // rendering code. Useful for determining whether adding extra skiplines to
    // the assign() function would scroll the text further.
    size_t parsed() const
    {
        return parsed_;
    }

private:
    OverlayCoord size_;
    OverlayCoord position_;
    size_t parsed_;
};


class Border
{
public:
    Border(const OverlayCoord& size,
           const OverlayCoord& position,
           bool fill = false,
           int tile_offset = 0,
           TileDesc default_tile = 0);
    Border(const Border&) = delete;
    ~Border();

private:
    OverlayCoord size_;
    OverlayCoord position_;
    bool filled_;
    TileDesc default_tile_;
};


class BossHealthBar
{
public:
    BossHealthBar(u8 height, const OverlayCoord& position);
    BossHealthBar(const BossHealthBar&) = delete;
    ~BossHealthBar();

    void set_health(Float percentage);

private:
    OverlayCoord position_;
    u8 height_;
};


class LoadingBar
{
public:
    LoadingBar(u8 width, const OverlayCoord& position);
    LoadingBar(const LoadingBar&) = delete;
    ~LoadingBar();

    void set_progress(Float percentage);

private:
    OverlayCoord position_;
    u8 width_;
};


// Swoops in/out from the right side of the screen, based on display percentage.
class Sidebar
{
public:
    Sidebar(u8 width, u8 height, const OverlayCoord& pos);
    Sidebar(const Sidebar&) = delete;
    ~Sidebar();

    void set_display_percentage(Float percentage);

private:
    const u8 width_;
    const u8 height_;
    OverlayCoord pos_;
};


class LeftSidebar
{
public:
    LeftSidebar(u8 width, u8 height, const OverlayCoord& pos);
    LeftSidebar(const Sidebar&) = delete;
    ~LeftSidebar();

    void set_display_percentage(Float percentage);

private:
    const u8 width_;
    const u8 height_;
    OverlayCoord pos_;
};


class HorizontalFlashAnimation
{
public:
    HorizontalFlashAnimation(const OverlayCoord& position)
        : position_(position), width_(0), timer_(0), index_(0)
    {
    }

    HorizontalFlashAnimation(const HorizontalFlashAnimation&) = delete;

    ~HorizontalFlashAnimation()
    {
        fill(0);
    }

    void init(int width)
    {
        width_ = width;
        timer_ = 0;
        index_ = 0;
    }

    enum { anim_len = 3 };

    u8 width()
    {
        return width_;
    }

    void update(Microseconds dt)
    {
        timer_ += dt;
        if (timer_ > milliseconds(34)) {
            timer_ = 0;
            if (done()) {
                return;
            }
            if (index_ < anim_len) {
                fill(108 + index_);
            }
            ++index_;
        }
    }

    bool done() const
    {
        return index_ == anim_len + 1;
    }

    void set_position(const OverlayCoord& position)
    {
        position_ = position;
    }

    const auto& position() const
    {
        return position_;
    }

private:
    void fill(int tile)
    {
        for (int i = position_.x; i < position_.x + width_; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, position_.y, tile);
        }
    }

    OverlayCoord position_;
    u8 width_;
    int timer_;
    int index_;
};


inline OverlayCoord calc_screen_tiles()
{
    constexpr u32 overlay_tile_size = 8;
    return (PLATFORM.screen().size() / overlay_tile_size).cast<u8>();
}


enum class Resolution {
    r16_9,
    r3_2,
    unknown,
    count,
};


inline Resolution resolution(Platform::Screen& screen)
{
    if (screen.size().x == 240 and screen.size().y == 160) {
        return Resolution::r3_2;
    } else if (screen.size().x == 240 and screen.size().y == 136) {
        return Resolution::r16_9;
    }
    return Resolution::unknown;
}


using Margin = u16;


inline Margin centered_text_margins(u16 text_length)
{
    const auto width = calc_screen_tiles().x;

    return (width - text_length) / 2;
}


inline void left_text_margin(Text& text, Margin margin)
{
    for (int i = 0; i < margin; ++i) {
        text.append(" ");
    }
}


inline void right_text_margin(Text& text, Margin margin)
{
    for (int i = 0; i < margin + 1 /* due to rounding in margin calc */; ++i) {
        text.append(" ");
    }
}


u32 integer_text_length(int n);


class UIMetric
{
public:
    enum class Align : u8 { left, right };

    enum class Format : u8 {
        plain_integer,
        fraction,
        fraction_p_m,
        integer_with_rate,
        signed_integer,
    };


    inline UIMetric(const OverlayCoord& pos,
                    int icon_tile,
                    u32 value,
                    Align align,
                    Format format = Format::plain_integer,
                    bool large_numerator = false)
        : icon_tile_(icon_tile), value_(value), anim_(pos), align_(align),
          format_(format), pos_(pos), large_numerator_(large_numerator)
    {
        display();
    }

    UIMetric(const UIMetric&) = delete;

    virtual ~UIMetric()
    {
    }

    inline void sync_value(u32 value)
    {
        if (this->value() not_eq value) {
            set_value(value);
        }
    }

    void set_value(u32 value);

    inline void update(Microseconds dt)
    {
        if (not anim_.done()) {
            anim_.update(dt);
            if (anim_.done()) {
                display();
            }
        }
    }

    const OverlayCoord& position() const
    {
        return pos_;
    }

    u32 value() const
    {
        return value_;
    }


    void use_large_numerator(bool val)
    {
        large_numerator_ = val;
    }

private:
    void display();

    const int icon_tile_;
    Optional<SmallIcon> icon_;
    Optional<Text> text_;
    u32 value_;
    HorizontalFlashAnimation anim_;
    const Align align_;
    const Format format_;
    OverlayCoord pos_;
    bool large_numerator_ = false;
};
