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


#pragma once

#include "allocator.hpp"
#include "macrocosmBlock.hpp"



namespace skyland::macro
{



namespace fiscal
{



struct LineItem
{
    using Label = StringBuffer<24>;

    Label label_;
    Float contribution_;
    LineItem* next_;
};



class Ledger
{
public:
    void add_entry(LineItem::Label, Float contribution);


    const LineItem* entries() const;


private:
    LineItem* entries_ = nullptr;
    ScratchBufferBulkAllocator alloc_;
};



} // namespace fiscal



} // namespace skyland::macro



namespace skyland::macro::terrain
{



class Sector
{
public:
    enum Orientation : u8 { north, east, south, west };

    enum class Shape : u8 { cube, pancake, pillar, freebuild, outpost };


    struct ExportInfo
    {
        Commodity::Type c;
        Vec3<u8> source_coord_;
        Vec2<s8> destination_;
        host_u16 export_supply_;
    };


    Sector(Vec2<s8> position, Shape shape, Vec3<u8> size);


    virtual ~Sector()
    {
    }

    void set_block(const Vec3<u8>& coord, Type type);

    virtual void rotate() = 0;
    virtual void update() = 0;
    void advance(int years);

    virtual void render_setup(Platform& pfrm) = 0;
    void render(Platform& pfrm);

    using Population = float;
    void set_population(Population p);


    virtual void shadowcast() = 0;
    virtual void erase() = 0;



    using Exports = Buffer<ExportInfo, 24>;


    virtual Exports* exports()
    {
        return nullptr;
    }

    virtual void set_export(const ExportInfo& e)
    {
    }

    virtual void remove_export(Vec3<u8> source_coord)
    {
    }


    u16 quantity_non_exported(Commodity::Type t);


    virtual const Block& get_block(const Vec3<u8>& coord) const = 0;
    virtual Block& ref_block(const Vec3<u8>& coord) = 0;


    Stats stats() const;


    static const int z_limit = 9;



    Population population() const;

    Float population_growth_rate() const;
    Coins coin_yield() const;


    fiscal::Ledger budget() const;


    Vec2<s8> coordinate() const;


    Vec3<u8> cursor() const
    {
        return p_.cursor_;
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
        u8 population_packed_[sizeof(Population)];

        s8 x_;
        s8 y_;

        Shape shape_;

        u8 pad_[1]; // FIXME: remove. Added while packing this struct.
    };
    static_assert(std::is_trivially_copyable<Persistent>());
    static_assert(alignof(Persistent) == 1);


    void set_name(const StringBuffer<name_len - 1>& name);
    StringBuffer<name_len - 1> name();



    Stats base_stats() const;


    void repaint();


    virtual void set_repaint(bool val) = 0;


    Vec3<u8> size() const
    {
        return size_;
    }


    virtual void base_stats_cache_clear() const
    {
    }


protected:
    virtual void base_stats_cache_store(const Stats& s) const
    {
    }


    virtual Stats* base_stats_cache_load() const
    {
        return nullptr;
    }



    Persistent p_;

    u8 z_view_ = z_limit;

    Vec3<u8> size_;


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

    virtual void restore(const Persistent& p, u8 blocks[4][5][5])
    {
        Platform::fatal("logic error: restore non-outpost from outpost data");
    }


    const Persistent& persistent() const
    {
        return p_;
    }
};



} // namespace skyland::macro::terrain