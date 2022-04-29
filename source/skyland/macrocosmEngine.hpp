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
#include "entity.hpp"
#include "number/int.h"
#include "systemString.hpp"



class Platform;



namespace skyland::macro
{
using Coins = s32;



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
    windmill,
    windmill_stone_base,
    shellfish,
    port,
    potatoes,
    sunflowers,
    food, // Must not be constructed
    shrubbery,
    wool,
    saffron,
    count,
};



enum class Category {
    basic,
    crop,
    livestock,
    fluid,
};



Category category(Type t);



struct Commodity
{
    enum Type : u8 {
        indigo,
        rose_madder,
        shellfish,
        sunflowers,
        food,
        wool,
        saffron,
    };
    Type type_;
    u16 supply_;
    bool imported_ = false;

    static Coins value(Type t);
    SystemString name() const;
};



struct Stats
{
    int food_ = 0;
    int food_exports_ = 0;
    int housing_ = 0;
    int employment_ = 0;

    Buffer<Commodity, 24> commodities_;
};



using Improvements = Buffer<Type, 10>;



Stats stats(Type t, bool shadowed);
SystemString name(Type t);
SystemString name(Commodity::Type t);
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


    struct ExportInfo
    {
        Commodity::Type c;
        Vec3<u8> source_coord_;
        Vec2<s8> destination_;
        host_u16 export_supply_;
    };


    Sector(Vec2<s8> position);


    void restore(const save::Sector&);


    void set_block(const Vec3<u8>& coord, Type type);

    void rotate();
    void update();
    void advance(int years);

    void render_setup(Platform& pfrm);
    void render(Platform& pfrm);

    using Population = float;
    void set_population(Population p);


    void shadowcast();
    void erase();


    void clear_cache();



    using Exports = Buffer<ExportInfo, 24>;


    const Exports& exports() const;
    void set_export(const ExportInfo& e);
    void remove_export(Vec3<u8> source_coord);


    u16 quantity_non_exported(Commodity::Type t);


    const Block& get_block(const Vec3<u8>& coord) const;


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
        Population population_ = 0;

        s8 x_;
        s8 y_;
    };
    static_assert(std::is_trivially_copyable<Persistent>());


    void set_name(const StringBuffer<name_len - 1>& name);
    StringBuffer<name_len - 1> name();



    Stats base_stats() const;



private:
    Persistent p_;

    // Recalculating stats for everything when we have multiple levels slows
    // down the game significantly, so we cache previous results. I mean, a
    // sector has ~512 blocks, and if you have 20 sectors, that's a lot of
    // number crunching and will definitely lag the game if done frequently.
    mutable std::optional<Stats> base_stats_cache_;

    u8 z_view_ = z_limit;

    Block blocks_[z_limit][8][8]; // (z, x, y)

    Exports exports_;


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
    static const int max_sectors = 20;

    struct Data
    {
        Data() : origin_sector_({0, 0})
        {
        }

        macro::terrain::Sector origin_sector_;

        Buffer<DynamicMemory<macro::terrain::Sector>, max_sectors - 1>
            other_sectors_;

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

        EntityList<Entity> entities_;
    };



    std::pair<Coins, terrain::Sector::Population> colony_cost() const;



    bool make_sector(Vec2<s8> coord)
    {
        if (load_sector(coord)) {
            return false;
        }

        auto s = allocate_dynamic<terrain::Sector>("macro-colony_mem", coord);
        StringBuffer<terrain::Sector::name_len - 1> n("colony_");
        n += stringify(data_->other_sectors_.size() + 1).c_str();
        s->set_name(n);
        return data_->other_sectors_.push_back(std::move(s));
    }



    macro::terrain::Sector* bind_sector(Vec2<s8> coord)
    {
        if (data_->origin_sector_.coordinate() == coord) {
            data_->current_sector_ = -1;
            return &data_->origin_sector_;
        } else {
            int i = 0;
            for (auto& s : data_->other_sectors_) {
                if (s->coordinate() == coord) {
                    data_->current_sector_ = i;
                    return &*s;
                }
                ++i;
            }

            return nullptr;
        }
    }



    macro::terrain::Sector* load_sector(Vec2<s8> coord)
    {
        if (data_->origin_sector_.coordinate() == coord) {
            return &data_->origin_sector_;
        } else {

            for (auto& s : data_->other_sectors_) {
                if (s->coordinate() == coord) {
                    return &*s;
                }
            }

            return nullptr;
        }
    }



    macro::terrain::Sector* load_sector(int id)
    {
        if (id == -1) {
            return &data_->origin_sector_;
        } else if (id < (int)data_->other_sectors_.size()) {
            return &*data_->other_sectors_[id];
        } else {
            return nullptr;
        }
    }



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
    bool load(Platform& pfrm);


    void newgame(Platform& pfrm);


    State();


    DynamicMemory<Data> data_;


    void advance(int elapsed_years);
};



} // namespace skyland::macro
