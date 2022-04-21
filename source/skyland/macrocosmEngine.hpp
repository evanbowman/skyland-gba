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
#include "number/int.h"
#include "systemString.hpp"



class Platform;



namespace skyland::macro
{
using Coins = u32;


namespace save
{
struct Sector;
}


} // namespace skyland::macro



namespace skyland::macro::terrain
{



static const int food_consumption_factor = 2;



enum class Type {
    air,
    building,
    __invalid,
    water,
    terrain,
    masonry,
    selector,
    wheat,
    indigo,
    madder,
    gold,
    workshop,
    water_slant_a,
    water_slant_b,
    water_slant_c,
    water_slant_d,
    light_source,
    count,
};



enum class Category {
    basic,
    crop,
    fluid,
};



Category category(Type t);



struct Commodity
{
    enum Type : u8 {
        indigo,
        rose_madder,
    };
    Type type_;
    u8 supply_;

    static Coins value(Type t);
};



struct Stats
{
    int food_ = 0;
    int housing_ = 0;
    int employment_ = 0;

    Buffer<Commodity, 16> commodities_;
};



using Improvements = Buffer<Type, 10>;



Stats stats(Type t);
SystemString name(Type t);
std::pair<int, int> icons(Type t);
Improvements improvements(Type t);



struct Block
{
    u8 type_ : 6;

    u8 shadowed_ : 1;
    u8 repaint_ : 1;

    u8 data_;


    Stats stats() const;
    SystemString name() const;

    Improvements improvements() const;


    Type type() const
    {
        return (Type)type_;
    }


    Block() : shadowed_(true), repaint_(true)
    {
    }
};
static_assert(sizeof(Block) == 2);



class Sector
{
public:
    enum Orientation : u8 { north, east, south, west };


    Sector(Vec2<s8> position);


    void restore(const save::Sector&);


    void set_block(const Vec3<u8>& coord, Type type);

    const Block& get_block(const Vec3<u8>& coord) const;


    void rotate();
    void update();
    void advance(int years);

    void render(Platform& pfrm);


    Stats stats() const;


    static const int z_limit = 9;


    using Population = float;


    Population population() const;

    void set_population(Population p);


    Float population_growth_rate() const;
    Coins coin_yield() const;


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


    // Should include almost all data that needs to be written to save memory,
    // except for the blocks themselves.
    struct Persistent
    {

        Orientation orientation_ = Orientation::north;

        Vec3<u8> cursor_;

        char name_[12];
        Population population_ = 0;

        s8 x_;
        s8 y_;
    };
    static_assert(std::is_trivially_copyable<Persistent>());


    void shadowcast();

private:

    Persistent p_;

    u8 z_view_ = z_limit;

    Block blocks_[z_limit][8][8]; // (z, x, y)


public:
    const Persistent& persistent() const
    {
        return p_;
    }
};



Coins cost(Sector& s, Type t);



} // namespace skyland::macro::terrain



namespace skyland::macro
{


struct State
{
    struct Data
    {
        Data() : origin_sector_({0, 0})
        {
        }

        macro::terrain::Sector origin_sector_;

        Buffer<DynamicMemory<macro::terrain::Sector>, 19> other_sectors_;

        int current_sector_ = -1;
        Float cloud_scroll_ = 0;


        // Contents will be written to save data.
        struct Persistent
        {
            host_u16 year_;
            HostInteger<Coins> coins_;

            Persistent()
            {
                year_.set(0);
                coins_.set(0);
                static_assert(std::is_trivially_copyable<Persistent>());
            }

        } persistent_;

        Persistent& p()
        {
            return persistent_;
        }
    };


    macro::terrain::Sector& sector()
    {
        if (data_->current_sector_ == -1) {
            return data_->origin_sector_;
        } else {
            return *data_->other_sectors_[data_->current_sector_];
        }
    }


    Coins coin_yield();


    void save(Platform& pfrm);
    void load(Platform& pfrm);


    State() : data_(allocate_dynamic<Data>("macrocosm-data"))
    {
    }

    DynamicMemory<Data> data_;


    void advance(int elapsed_years);
};



} // namespace skyland::macro
