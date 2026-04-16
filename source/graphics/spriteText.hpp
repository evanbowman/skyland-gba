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
#include "localization.hpp"
#include "allocator.hpp"



class SpriteText
{
public:
    SpriteText(const char* str);
    ~SpriteText();

    SpriteText(SpriteText&& other);
    SpriteText& operator=(SpriteText&& other);

    SpriteText(const SpriteText&) = delete;
    SpriteText& operator=(const SpriteText&) = delete;

    void set_position(const Vec2<Fixnum>& pos);
    const Vec2<Fixnum>& position() const;

    struct GlyphOffset
    {
        s8 x_ = 0;
        s8 y_ = 0;
    };

    void set_glyph_offset(int index, GlyphOffset offset);

    void draw();

    int glyph_count() const
    {
        return entries_.size();
    }

    u16 pixel_width() const
    {
        return pixel_width_;
    }

    // Re-upload all glyphs to sprite VRAM. Useful if DynamicTexture regions
    // were overwritten by other systems, e.g. changing the underlying texture.
    void restore();

    void position_absolute()
    {
        position_absolute_ = true;
    }

    void hide()
    {
        show_chars_ = 0;
    }

    void reveal_char()
    {
        ++show_chars_;
    }

private:
    void release();

    struct GlyphEntry
    {
        u16 tile_index_;
        u16 slot_index_;
        u16 x_offset_;
        GlyphOffset anim_;
    };

    Buffer<GlyphEntry, 32> entries_;
    Vec2<Fixnum> position_;
    u8 pixel_width_ = 0;
    bool position_absolute_ = false;
    u8 show_chars_ = 0;
};


void sprite_text_clear();
