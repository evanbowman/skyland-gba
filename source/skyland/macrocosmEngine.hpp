////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



// NOTE: about the code:
// Yeah, this code is pretty horrible. I made the macro mode, then heavily
// revised it during beta testing, at which point, I was pretty burnt out and
// didn't really care about anything other than player experience. Code quality
// sort of went out the window.



#pragma once

#include "alloc_entity.hpp"
#include "allocator.hpp"
#include "containers/vector.hpp"
#include "entity.hpp"
#include "entity/macro/macrocosmEntity.hpp"
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


        Data();


        DynamicMemory<terrain::Sector> origin_sector_;


        using SectorArray =
            Buffer<DynamicMemory<terrain::Sector>, max_sectors - 1>;


        SectorArray other_sectors_;


        u16 singularity_count_ = 0;
        bool singularity_expand_ = false;


        terrain::Sector* current_sector_;
        Float cloud_scroll_ = 0;

        bool freebuild_mode_ = false;
        bool checkers_mode_ = false;
        bool checkers_ai_moved_ = false;

        Time year_timer_ = 0;

        // For palette animations.
        Time fluid_anim_timer_ = 0;
        u16 realtime_update_index_ = 0;
        u8 water_anim_index_ = 0;
        u8 lava_anim_index_ = 128;
        u8 cropcycle_index_ = 0;
        u32 frames_since_music_ = 0;

        Keylock keylock_ = nolock;

        macro::terrain::Type last_created_ = terrain::Type::terrain;
        macro::terrain::Type last_improved_ = terrain::Type::terrain;

#ifndef __CMD_MACRO_RAST__
        EntityList<MacrocosmEntity> entities_;
#endif


        // Contents will be written to save data.
        struct Persistent
        {
            host_u16 year_;
            host_u32 day_;

            HostInteger<s32> lumber_;
            HostInteger<s32> stone_;
            HostInteger<s32> clay_;
            HostInteger<s32> water_;
            HostInteger<s32> crystal_;

            HostInteger<s16> day_night_cyc_;
            HostInteger<s16> coins_;
            HostInteger<s32> reserved_words_[8];

            Persistent()
            {
                year_.set(0);
                day_.set(0);
                lumber_.set(0);
                stone_.set(0);
                clay_.set(0);
                water_.set(0);
                crystal_.set(0);
                for (auto& r : reserved_words_) {
                    r.set(0);
                }
                static_assert(std::is_trivially_copyable<Persistent>());
            }

        } persistent_;

        Persistent& p()
        {
            return persistent_;
        }
    };


    std::pair<Coins, Productivity> colony_cost() const;


    terrain::Sector* make_sector(Vec2<s8> coord, terrain::Sector::Shape shape);



    static int food_consumption_factor();
    static Float commodity_diminishing_return_percent();



    void erase_sector(Vec2<s8> coord)
    {
        if (data_->origin_sector_->coordinate() == coord) {
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
        if (data_->origin_sector_->coordinate() == coord) {
            data_->current_sector_ = &*data_->origin_sector_;
            return &*data_->origin_sector_;
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
        if (data_->origin_sector_->coordinate() == coord) {
            return &*data_->origin_sector_;
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
            return &*data_->origin_sector_;
        } else if (id < (int)data_->other_sectors_.size()) {
            return &*data_->other_sectors_[id];
        } else {
            return nullptr;
        }
    }


    macro::terrain::Sector& sector();


    void save();
    bool load();


    void newgame();


    EngineImpl(App*);


#ifndef __CMD_MACRO_RAST__
    template <typename T, typename... Args> T* add_entity(Args&&... args)
    {
        if (auto e = alloc_entity<T>(std::forward<Args>(args)...)) {
            auto ret = e.get();
            data_->entities_.push(std::move(e));
            return ret;
        }
        return nullptr;
    }
#endif


    DynamicMemory<Data> data_;
};



void background_init();



bool harvest_block(macro::EngineImpl& state, terrain::Sector& s, Vec3<u8> c);



EngineImpl& bound_state();



Vec2<Fixnum> screen_coord(int tile_coord);



static const int day_frames = 18000;
static const int night_frames = 2500;


} // namespace skyland::macro



namespace skyland
{



macro::EngineImpl& macrocosm();



}
