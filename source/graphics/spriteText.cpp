////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "spriteText.hpp"
#include "localization.hpp"



struct SpriteTextDictionary
{
    struct Slot
    {
        utf8::Codepoint cp_ = 0; // 0 = free
        u16 refcount_ = 0;
    };

    Slot slots_[32];
    Buffer<Platform::DynamicTexturePtr, 2> backing_textures_;
    u16 active_count_ = 0;

    static const int slots_per_dt = 16;


    int capacity() const
    {
        return backing_textures_.size() * slots_per_dt;
    }


    u16 tile_for_slot(int slot) const
    {
        auto& txtr = backing_textures_[slot / slots_per_dt];
        return u16(txtr->mapping_index() * slots_per_dt + slot % slots_per_dt);
    }


    struct AllocResult
    {
        u16 tile_index_;
        u16 slot_index_;
    };


    Optional<AllocResult> allocate_glyph(utf8::Codepoint cp,
                                         SpriteText::Configuration conf)
    {
        int cap = capacity();

        // Return existing slot if this codepoint is already loaded.
        for (int i = 0; i < cap; ++i) {
            if (slots_[i].cp_ == cp and slots_[i].refcount_ > 0) {
                slots_[i].refcount_++;
                return AllocResult{tile_for_slot(i), u16(i)};
            }
        }

        // Search for a free slot within already-allocated textures.
        int free_slot = -1;
        for (int i = 0; i < cap; ++i) {
            if (slots_[i].cp_ == 0) {
                free_slot = i;
                break;
            }
        }

        // No free slot found: try to allocate another DynamicTexture.
        if (free_slot < 0) {
            if (backing_textures_.full()) {
                return nullopt();
            }
            auto dt = PLATFORM.make_dynamic_texture();
            if (not dt) {
                return nullopt();
            }
            free_slot = cap;
            backing_textures_.emplace_back(*dt);
        }

        auto mapping = locale_texture_map()(cp);
        if (not mapping) {
            return nullopt();
        }

        auto dst_tile = tile_for_slot(free_slot);

        PLATFORM.load_sprite_chunk(dst_tile,
                                   mapping->offset_,
                                   1,
                                   mapping->texture_name_,
                                   conf.shade_bg_index_,
                                   conf.shade_fg_index_);

        slots_[free_slot].cp_ = cp;
        slots_[free_slot].refcount_ = 1;
        active_count_++;

        return AllocResult{dst_tile, u16(free_slot)};
    }


    void release_glyph(u16 slot_index)
    {
        auto& s = slots_[slot_index];
        if (s.refcount_ > 0) {
            s.refcount_--;
            if (s.refcount_ == 0) {
                s.cp_ = 0;
                active_count_--;
            }
        }
    }


    void clear()
    {
        for (auto& s : slots_) {
            s.cp_ = 0;
            s.refcount_ = 0;
        }
        active_count_ = 0;
        backing_textures_.clear();
    }
};



static Optional<DynamicMemory<SpriteTextDictionary>>
    sprite_text_mem;



static SpriteTextDictionary& dictionary()
{
    if (not sprite_text_mem) {
        sprite_text_mem =
            allocate<SpriteTextDictionary>("sprite-text-mem");
    }
    return **sprite_text_mem;
}



static void try_release_dictionary()
{
    if (sprite_text_mem and (*sprite_text_mem)->active_count_ == 0) {
        sprite_text_mem.reset();
    }
}



SpriteText::SpriteText(const char* str, const Configuration& conf)
{
    auto& dict = dictionary();

    u16 x_cursor = 0;

    utf8::scan(
        [&](const utf8::Codepoint& cp, const char*, int) {
            if (cp == ' ') {
                x_cursor += 8;
                return true;
            }
            auto result = dict.allocate_glyph(cp, conf);
            if (result) {
                entries_.push_back(
                    {result->tile_index_, result->slot_index_, x_cursor, {}});
            }
            x_cursor += 8;
            return true;
        },
        str,
        strlen(str));

    show_chars_ = entries_.size();
    pixel_width_ = x_cursor;
}



void SpriteText::release()
{
    if (not sprite_text_mem or entries_.empty()) {
        return;
    }

    auto& dict = **sprite_text_mem;

    for (auto& entry : entries_) {
        dict.release_glyph(entry.slot_index_);
    }

    entries_.clear();

    try_release_dictionary();
}



SpriteText::~SpriteText()
{
    release();
}



SpriteText::SpriteText(SpriteText&& other)
    : position_(other.position_)
    , pixel_width_(other.pixel_width_)
    , position_absolute_(other.position_absolute_)
    , show_chars_(other.show_chars_)
{
    for (auto& e : other.entries_) {
        entries_.push_back(e);
    }
    other.entries_.clear();
    other.pixel_width_ = 0;
}



SpriteText& SpriteText::operator=(SpriteText&& other)
{
    if (this != &other) {
        release();

        for (auto& e : other.entries_) {
            entries_.push_back(e);
        }
        position_ = other.position_;
        pixel_width_ = other.pixel_width_;

        other.entries_.clear();
        other.pixel_width_ = 0;
        show_chars_ = other.show_chars_;
        position_absolute_ = other.position_absolute_;
    }
    return *this;
}



void SpriteText::set_position(const Vec2<Fixnum>& pos)
{
    position_ = pos;
}



const Vec2<Fixnum>& SpriteText::position() const
{
    return position_;
}



void SpriteText::set_glyph_offset(int index, GlyphOffset offset)
{
    if (index >= 0 and index < (int)entries_.size()) {
        entries_[index].anim_ = offset;
    }
}



void SpriteText::draw()
{
    auto& screen = PLATFORM.screen();

    auto vc = PLATFORM.screen().get_view().int_center();
    Vec2<Fixnum> vc_fp {Fixnum::from_integer(vc.x), Fixnum::from_integer(vc.y)};

    for (u32 i = 0; i < entries_.size() and i < (u32)show_chars_; ++i) {
        auto& entry = entries_[i];
        Sprite spr;
        spr.set_priority(0);
        spr.set_size(Sprite::Size::w8_h8);
        spr.set_texture_index(entry.tile_index_);
        auto pos = Vec2<Fixnum>{
            position_.x + Fixnum(entry.x_offset_ + entry.anim_.x_),
            position_.y + Fixnum(entry.anim_.y_)};
        if (position_absolute_) {
            pos = pos + vc_fp;
        }
        spr.set_position(pos);
        screen.draw(spr);
    }
}



void SpriteText::restore()
{
    if (not sprite_text_mem) {
        return;
    }

    auto& dict = **sprite_text_mem;

    for (auto& entry : entries_) {
        auto& slot = dict.slots_[entry.slot_index_];
        auto mapping = locale_texture_map()(slot.cp_);
        if (mapping) {
            Configuration conf; // FIXME! Store this in SpriteText
            PLATFORM.load_sprite_chunk(entry.tile_index_,
                                       mapping->offset_,
                                       1,
                                       mapping->texture_name_,
                                       conf.shade_bg_index_,
                                       conf.shade_fg_index_);
        }
    }
}



void animate_text(SpriteText& text, TextAnimation anim, Microseconds timer)
{
    const int n = text.glyph_count();

    auto ang = [](Microseconds t) -> s16 { return (s16)(t & 0xFFFF); };
    auto scale = [](s16 s, int amp) -> s8 {
        // amp is peak-to-peak range. Map sine [-INT16_MAX, INT16_MAX]
        // to an integer in [0, amp], then recenter.
        const s32 unsigned_sine = (s32)s + INT16_MAX; // [0, 2*INT16_MAX]
        const s32 stepped = (unsigned_sine * (amp + 1)) / ((s32)INT16_MAX * 2 + 1);
        return (s8)(stepped - amp / 2);
    };
    for (int i = 0; i < n; ++i) {
        SpriteText::GlyphOffset off{};

        switch (anim) {
        case TextAnimation::none:
            break;

        case TextAnimation::wave: {
            const Microseconds phase = (timer >> 3) + i * 6000;
            off.y_ = scale(sine(ang(phase)), 1);
            break;
        }

        case TextAnimation::laugh: {
            const Microseconds py = (timer >> 2) + i * 11000;
            off.y_ = scale(sine(ang(py)), 1);
            off.x_ = 0;
            break;
        }

        case TextAnimation::shake: {
            off.x_ = scale(sine(ang(timer >> 1)), 1);
            off.y_ = scale(sine(ang((timer >> 1) + 15000)), 1);
            break;
        }

        case TextAnimation::tremble: {
            const Microseconds px = (timer >> 1) + i * 7919;
            const Microseconds py = (timer >> 1) + i * 5413;
            off.x_ = scale(sine(ang(px)), 1);
            off.y_ = scale(sine(ang(py)), 1);
            break;
        }

        case TextAnimation::bounce: {
            // Upward-only hop. -3 clears the baseline clearly on 8px glyphs.
            const Microseconds phase = (timer >> 3) + i * 8000;
            const s16 s = sine(ang(phase));
            off.y_ = (s > 0) ? (s8)(-((s32)s * 3) / INT16_MAX) : 0;
            break;
        }

        case TextAnimation::pulse: {
            // Outward push from center. Cap amplitude at 2 — beyond
            // that, adjacent glyphs visibly separate past the gap.
            const int center = n / 2;
            const s16 s = sine(ang(timer >> 3));
            const int sign = (i < center) ? -1 : 1;
            off.x_ = (s8)(scale(s, 2) * sign);
            break;
        }
        }

        text.set_glyph_offset(i, off);
    }
}
