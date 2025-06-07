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

#include "allocator.hpp"
#include "macrocosmBlock.hpp"
#include "qr.hpp"



namespace skyland
{



class App;



}



namespace skyland::macro
{



struct EngineImpl;



} // namespace skyland::macro



namespace skyland::macro::terrain
{



using Food = s32;



class FreebuildSector;



class Sector
{
public:
    enum Orientation : u8 { north, east, south, west };

    enum class Shape : u8 {
        cube,
        pancake,
        pillar,
        freebuild,
        freebuild_wide,
        freebuild_flat,
        reserved_important_never_use,
    };


    Sector(Vec2<s8> position, Shape shape, Vec3<u8> size);


    virtual ~Sector()
    {
    }


    virtual FreebuildSector* cast_freebuild_sector()
    {
        return nullptr;
    }


    void set_block(const Vec3<u8>& coord, Type type);
    virtual void rotate() = 0;
    virtual void update() = 0;


    void soft_update(EngineImpl& state);

    virtual void render_setup() = 0;
    void render();

    void set_population(Population p);
    void set_productivity(Productivity p);


    virtual void shadowcast() = 0;
    virtual void erase() = 0;


    // The qrcode will hold binary data in the form:
    // first byte: island layout, rest: rle-encoded data.
    Optional<QRCode>
    qr_encode(Function<4 * sizeof(void*), void(const char*)> msg) const;


    // For some blocks, we need to run updates even when a sector is not
    // currently selected. Iterating over every sector cube to update all blocks
    // each time would be super expensive, especially for large
    // civilizations. So each sector keeps a list of coordinates to update in
    // the background. For each Sector::update() call, the sector clears the
    // queue of background blocks, then, each block implementation calls
    // bkg_update_push to schedule the next background update.
    struct BackgroundUpdateCoord
    {
        u8 x_ : 5;
        u8 y_ : 5;
        u8 z_ : 5;
    };
    using BackgroundUpdateBlocks = Buffer<BackgroundUpdateCoord, 24>;

    virtual BackgroundUpdateBlocks* background_update_blocks()
    {
        return nullptr;
    }


    virtual const Block& get_block(const Vec3<u8>& coord) const = 0;
    virtual Block& ref_block(const Vec3<u8>& coord) = 0;


    static const int z_limit = 9;


    Population population() const;
    Productivity productivity() const;


    Vec2<s8> coordinate() const;


    Vec3<u8> cursor() const
    {
        return p_.cursor_;
    }


    Food food() const
    {
        return p_.food_.get();
    }


    void set_food(Food f)
    {
        p_.food_.set(f);
    }


    void set_cursor(const Vec3<u8>& pos, bool lock_to_floor = true);

    // Projected position of the cursor onto the frame buffer.
    u16 cursor_raster_pos() const;


    Orientation orientation() const
    {
        return p_.orientation_;
    }


    bool set_z_view(u8 z_view);
    u8 get_z_view() const
    {
        return z_view_;
    }


    static const int name_len = 12;


    // Should include almost all data that needs to be written to save memory,
    // except for the blocks themselves.
    struct Persistent
    {

        Orientation orientation_ = Orientation::north;

        Vec3<u8> cursor_;

        char name_[name_len];
        HostInteger<Population> population_;
        HostInteger<Productivity> productivity_;
        HostInteger<Food> food_;

        s8 x_;
        s8 y_;

        Shape shape_;

        u8 pad_[1];
    };
    static_assert(std::is_trivially_copyable<Persistent>());
    static_assert(alignof(Persistent) == 1);


    void set_name(const StringBuffer<name_len - 1>& name);
    StringBuffer<name_len - 1> name();


    void repaint();


    void recalc_stats();


    virtual void on_block_changed(const Vec3<u8>& coord) = 0;


    Vec3<u8> size() const
    {
        return size_;
    }



    virtual u16 project_block(int x, int y, int z) const
    {
        Platform::fatal("project block unimplemented");
    }


    // Format:
    // 1: byte containing layout.
    // 2: rle-compressed data.
    void pack(Vector<char>& result);
    void unpack(Vector<char>& input);


    void on_day_transition();


    Food food_storage() const
    {
        return 5 + granaries_ * 8;
    }


    Population housing() const
    {
        return 1 + housing_;
    }


protected:
    Persistent p_;

    u8 z_view_ = z_limit;

    Vec3<u8> size_;
    u8 housing_ = 0;
    u8 granaries_ = 0;


public:
    // Restore from a previous save.
    virtual void restore(const Persistent& p, u8 blocks[z_limit][8][8])
    {
        Platform::fatal("logic error: restore non-cube from cube data");
    }

    virtual void restore(const Persistent& p, u8 blocks[4][12][12])
    {
        Platform::fatal("logic error: restore non-pancake from pancake data");
    }

    virtual void restore(const Persistent& p, u8 blocks[16][6][6])
    {
        Platform::fatal("logic error: restore non-pillar from pillar data");
    }

    virtual void restore_fb(const Persistent& p, u8 blocks[6][12][12])
    {
        Platform::fatal("logic error: restore non-fb-wide from fb-wide data");
    }


    const Persistent& persistent() const
    {
        return p_;
    }


    void generate_terrain(int min_blocks, int buildings);

    void generate_terrain_origin(int min_blocks);

    void generate_terrain_regular(int min_blocks, int buildings);
    void generate_terrain_desert(int min_blocks, int buildings);
    void generate_terrain_tundra(int min_blocks, int buildings);
    void generate_terrain_molten(int min_blocks, int buildings);
};



} // namespace skyland::macro::terrain
