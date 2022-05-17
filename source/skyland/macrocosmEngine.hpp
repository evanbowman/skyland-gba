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
#include "containers/vector.hpp"
#include "entity.hpp"
#include "macrocosmCubeSector.hpp"
#include "macrocosmEngineOpaque.hpp"
#include "macrocosmOutpostSector.hpp"
#include "macrocosmRaster.hpp"
#include "macrocosmSector.hpp"
#include "number/int.h"
#include "systemString.hpp"



class Platform;



namespace skyland::macro
{

struct StateImpl;
extern StateImpl* _bound_state;


} // namespace skyland::macro



namespace skyland::macro::terrain
{



static const int food_consumption_factor = 2;



enum Categories : u8 {
    basic = 1 << 0,
    crop = 1 << 1,
    livestock = 1 << 2,
    fluid_water = 1 << 3,
    fluid_lava = 1 << 4,
};



Categories categories(Type t);


Stats stats(Type t, bool shadowed);
SystemString name(Type t);
SystemString name(Commodity::Type t);
std::pair<int, int> icons(Type t);
Improvements improvements(Type t);



Coins cost(Sector& s, Type t);



} // namespace skyland::macro::terrain



namespace skyland::macro
{



struct StateImpl : public State
{
    static const int max_sectors = 20;
    static const int max_outposts = 40;

    struct Data
    {
        Data() : origin_sector_({0, 0})
        {
        }


        terrain::CubeSector origin_sector_;


        using SectorArray =
            Buffer<DynamicMemory<terrain::Sector>, max_sectors - 1>;


        SectorArray other_sectors_;

        Vector<terrain::OutpostSector> outpost_sectors_;


        terrain::Sector* current_sector_ = &origin_sector_;
        Float cloud_scroll_ = 0;

        bool freebuild_mode_ = false;


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
    std::pair<Coins, terrain::Sector::Population> outpost_cost() const;


    terrain::Sector* make_sector(Vec2<s8> coord, terrain::Sector::Shape shape);



    void erase_sector(Vec2<s8> coord)
    {
        if (data_->origin_sector_.coordinate() == coord) {
            Platform::fatal("cannot erase origin sector!");
        }

        // Rebind the origin sector, in case we are erasing the currently bound
        // sector.
        bind_sector({0, 0});

        for (auto it = data_->other_sectors_.begin();
             it not_eq data_->other_sectors_.end();
             /* ... */) {

            if ((*it)->coordinate() == coord) {
                it = data_->other_sectors_.erase(it);
                return;
            } else {
                ++it;
            }
        }

        for (auto it = data_->outpost_sectors_.begin();
             it not_eq data_->outpost_sectors_.end();
             /* ... */) {

            if (it->coordinate() == coord) {
                data_->outpost_sectors_.erase(it);
                return;
            } else {
                ++it;
            }
        }
    }



    macro::terrain::Sector* bind_sector(Vec2<s8> coord)
    {
        if (data_->origin_sector_.coordinate() == coord) {
            data_->current_sector_ = &data_->origin_sector_;
            return &data_->origin_sector_;
        } else {
            for (auto& s : data_->other_sectors_) {
                if (s->coordinate() == coord) {
                    data_->current_sector_ = &*s;
                    return &*s;
                }
            }

            for (auto& s : data_->outpost_sectors_) {
                if (s.coordinate() == coord) {
                    data_->current_sector_ = &s;
                    return &s;
                }
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

            for (auto& s : data_->outpost_sectors_) {
                if (s.coordinate() == coord) {
                    return &s;
                }
            }

            return nullptr;
        }
    }



    macro::terrain::Sector* load_sector(int id)
    {
        if (id == -1) {
            return &data_->origin_sector_;
        } else if (id < -1) {
            return &data_->outpost_sectors_[-1 * (2 + id)];
        } else if (id < (int)data_->other_sectors_.size()) {
            return &*data_->other_sectors_[id];
        } else {
            return nullptr;
        }
    }



    macro::terrain::Sector& sector();


    Coins coin_yield();


    void save(Platform& pfrm);
    bool load(Platform& pfrm);


    void newgame(Platform& pfrm);


    StateImpl(Platform&);


    DynamicMemory<Data> data_;


    void advance(int elapsed_years);
};



void background_init(Platform&);



} // namespace skyland::macro



namespace skyland
{



macro::StateImpl& macrocosm(App& app);



}
