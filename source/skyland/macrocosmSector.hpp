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
#include "qr.hpp"



namespace skyland
{



class App;



}



namespace skyland::macro
{



struct EngineImpl;



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
    };


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


    virtual FreebuildSector* cast_freebuild_sector()
    {
        return nullptr;
    }


    void set_block(const Vec3<u8>& coord, Type type);
    virtual void rotate() = 0;
    virtual void update() = 0;

    void soft_update(EngineImpl& state);

    virtual void render_setup(Platform& pfrm) = 0;
    void render(Platform& pfrm);

    void set_population(Population p);


    void set_productivity(Productivity p);
    Productivity productivity() const;


    using Happiness = float;
    Happiness get_happiness(EngineImpl& state) const;
    fiscal::Ledger annotate_happiness(EngineImpl& state,
                                      bool skip_labels = false) const;


    virtual void shadowcast() = 0;
    virtual void erase() = 0;


    // The qrcode will hold binary data in the form:
    // first byte: island layout, rest: rle-encoded data.
    std::optional<QRCode>
    qr_encode(Platform&,
              App& app,
              Function<4 * sizeof(void*), void(const char*)> msg) const;


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

    Float population_growth_rate_from_food_supply() const;
    Float population_growth_rate_from_housing_supply() const;
    Float population_growth_rate() const;
    Coins coin_yield() const;


    fiscal::Ledger budget(bool skip_labels = false) const;


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
        u8 productivity_packed_[sizeof(Productivity)];

        s8 x_;
        s8 y_;

        Shape shape_;

        u8 pad_[1];
    };
    static_assert(std::is_trivially_copyable<Persistent>());
    static_assert(alignof(Persistent) == 1);


    void set_name(const StringBuffer<name_len - 1>& name);
    StringBuffer<name_len - 1> name();



    Stats base_stats() const;


    void repaint();


    virtual void on_block_changed(const Vec3<u8>& coord) = 0;


    Vec3<u8> size() const
    {
        return size_;
    }


    virtual void base_stats_cache_clear() const
    {
    }


    // Format:
    // 1: byte containing layout.
    // 2: rle-compressed data.
    void pack(Vector<char>& result);
    void unpack(Vector<char>& input);


protected:
    virtual void base_stats_cache_store(const Stats& s) const
    {
    }


    virtual std::optional<Stats> base_stats_cache_load() const
    {
        return {};
    }


    virtual void coin_yield_cache_store(Coins c) const
    {
    }


    virtual std::optional<Coins> coin_yield_cache_load() const
    {
        return {};
    }


    virtual void coin_yield_cache_clear() const
    {
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


    const Persistent& persistent() const
    {
        return p_;
    }


    void generate_terrain(int min_blocks, int buildings);

};



} // namespace skyland::macro::terrain
