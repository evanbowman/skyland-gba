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
#include "macrocosmRaster.hpp"
#include "macrocosmSector.hpp"
#include "number/int.h"
#include "sharedVariable.hpp"
#include "systemString.hpp"



class Platform;



namespace skyland::macro
{

struct EngineImpl;
extern EngineImpl* _bound_state;



enum Keylock { nolock, buildlock, improvelock, deletelock };



struct EngineImpl : public Engine
{
    static const int max_sectors = 30;

    struct Data
    {
#define MCR_SHARED(name) SharedVariable name = #name

        struct Bindings
        {
            // Variables exposed to the script engine.
            MCR_SHARED(mcr_building_cost);
            MCR_SHARED(mcr_water_cost);
            MCR_SHARED(mcr_lava_cost);
            MCR_SHARED(mcr_terrain_cost);
            MCR_SHARED(mcr_masonry_cost);
            MCR_SHARED(mcr_wheat_cost);
            MCR_SHARED(mcr_indigo_cost);
            MCR_SHARED(mcr_madder_cost);
            MCR_SHARED(mcr_gold_cost);
            MCR_SHARED(mcr_workshop_cost);
            MCR_SHARED(mcr_light_source_cost);
            MCR_SHARED(mcr_windmill_cost);
            MCR_SHARED(mcr_windmill_stone_base_cost);
            MCR_SHARED(mcr_shellfish_cost);
            MCR_SHARED(mcr_port_cost);
            MCR_SHARED(mcr_potatoes_cost);
            MCR_SHARED(mcr_sunflowers_cost);
            MCR_SHARED(mcr_shrubbery_cost);
            MCR_SHARED(mcr_wool_cost);
            MCR_SHARED(mcr_saffron_cost);
            MCR_SHARED(mcr_ice_cost);
            MCR_SHARED(mcr_cocoa_cost);
            MCR_SHARED(mcr_tea_cost);
            MCR_SHARED(mcr_lumber_cost);
            MCR_SHARED(mcr_arch_cost);
            MCR_SHARED(mcr_sand_cost);
            MCR_SHARED(mcr_crystal_cost);
            MCR_SHARED(mcr_marble_cost);
            MCR_SHARED(mcr_scaffolding_cost);
            MCR_SHARED(mcr_tulips_cost);
            MCR_SHARED(mcr_pearls_cost);
            MCR_SHARED(mcr_road_cost);
            MCR_SHARED(mcr_honey_cost);

            MCR_SHARED(mcr_food_consumption_factor);
            MCR_SHARED(mcr_commodity_diminishing_return_percent);
            MCR_SHARED(mcr_employment_yield_percent);
            MCR_SHARED(mcr_jobless_yield_percent);
            MCR_SHARED(mcr_homelessness_penalty_percent);
            MCR_SHARED(mcr_starvation_penalty_percent);

            MCR_SHARED(mcr_pop_growth_food_surplus_percent);
            MCR_SHARED(mcr_pop_growth_food_shortage_percent);
            MCR_SHARED(mcr_pop_growth_housing_factor);

            MCR_SHARED(mcr_base_seconds_per_year);
            MCR_SHARED(mcr_added_seconds_per_year_per_island);
        };


        DynamicMemory<Bindings> bindings_;



        Data()
            : bindings_(allocate_dynamic<Bindings>("mcr-script-bindings")),
              origin_sector_({0, 0})
        {
        }


        terrain::CubeSector origin_sector_;


        using SectorArray =
            Buffer<DynamicMemory<terrain::Sector>, max_sectors - 1>;


        SectorArray other_sectors_;


        u16 singularity_count_ = 0;
        bool singularity_expand_ = false;


        terrain::Sector* current_sector_ = &origin_sector_;
        Float cloud_scroll_ = 0;

        bool freebuild_mode_ = false;
        bool checkers_mode_ = false;
        bool checkers_ai_moved_ = false;

        Microseconds year_timer_ = 0;

        // For palette animations.
        Microseconds fluid_anim_timer_ = 0;
        u16 realtime_update_index_ = 0;
        u8 water_anim_index_ = 0;
        u8 lava_anim_index_ = 128;
        u8 cropcycle_index_ = 0;

        Keylock keylock_ = nolock;

        macro::terrain::Type last_created_ = terrain::Type::terrain;
        macro::terrain::Type last_improved_ = terrain::Type::terrain;


        // Contents will be written to save data.
        struct Persistent
        {
            host_u16 year_;
            HostInteger<Food> food_;
            HostInteger<Stone> stone_;
            HostInteger<Lumber> lumber_;
            HostInteger<Marble> marble_;
            HostInteger<Crystal> crystal_;
            HostInteger<Water> water_;
            HostInteger<s32> reserved_words_[10];

            Persistent()
            {
                year_.set(0);
                food_.set(0);
                stone_.set(0);
                lumber_.set(0);
                marble_.set(0);
                crystal_.set(0);
                water_.set(0);
                static_assert(std::is_trivially_copyable<Persistent>());
            }

        } persistent_;

        Persistent& p()
        {
            return persistent_;
        }
    };


    std::pair<Coins, Population> colony_cost() const;


    terrain::Sector* make_sector(Vec2<s8> coord, terrain::Sector::Shape shape);



    static Data::Bindings& bindings();


    static int food_consumption_factor();
    static Float commodity_diminishing_return_percent();



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


    int food_storage();


    macro::terrain::Sector& sector();



    Coins coin_yield();


    void save(Platform& pfrm);
    bool load(Platform& pfrm, App& app);


    void newgame(Platform& pfrm, App& app);


    EngineImpl(Platform&, App*);


    DynamicMemory<Data> data_;
};



void background_init(Platform&);



} // namespace skyland::macro



namespace skyland
{



macro::EngineImpl& macrocosm(App& app);



}
