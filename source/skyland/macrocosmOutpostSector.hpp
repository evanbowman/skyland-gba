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


#include "macrocosmSector.hpp"
#include "macrocosmSectorImpl.hpp"



namespace skyland::macro::terrain
{



class OutpostSector : public MacrocosmSectorImpl<OutpostSector, 5, 5, 4, 12>
{
public:
    OutpostSector(Vec2<s8> position)
        : MacrocosmSectorImpl(position, Shape::outpost)
    {
        erase();
    }


    // Projects isometric geometry into indices in the tilemap.
    static constexpr const u16 screen_mapping_lut[5][5] = {
        {14, 45, 76, 107, 138},
        {43, 74, 105, 136, 167},
        {72, 103, 134, 165, 196},
        {101, 132, 163, 194, 225},
        {130, 161, 192, 223, 254}};


    static constexpr const Vec2<u8> winding_path[] = {
        {0, 0}, {1, 0}, {0, 1}, {2, 0}, {1, 1}, {0, 2}, {3, 0}, {2, 1}, {1, 2},
        {0, 3}, {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4}, {4, 1}, {3, 2}, {2, 3},
        {1, 4}, {4, 2}, {3, 3}, {2, 4}, {4, 3}, {3, 4}, {4, 4},
    };


    void restore(const Persistent& p, u8 blocks[4][5][5]) override;

    void update() override;


    void base_stats_cache_clear() const override
    {
        base_stats_cache_.reset();
    }


    void coin_yield_cache_clear() const override
    {
        coin_yield_cache_.reset();
    }


    enum { commodities_max = 5 };


    void base_stats_cache_store(const Stats& s) const override
    {
        base_stats_cache_.emplace();
        base_stats_cache_->food_ = s.food_;
        base_stats_cache_->housing_ = s.housing_;
        base_stats_cache_->productivity_ = s.productivity_;
        base_stats_cache_->happiness_ = s.happiness_;

        base_stats_cache_->commodity_count_ =
            std::min((int)commodities_max, (int)s.commodities_.size());

        for (u32 i = 0; i < base_stats_cache_->commodity_count_; ++i) {
            base_stats_cache_->commodities_[i] = s.commodities_[i];
        }
    }


    void coin_yield_cache_store(Coins c) const override
    {
        coin_yield_cache_ = c;
    }


    Optional<Stats> base_stats_cache_load() const override
    {
        if (not base_stats_cache_) {
            return {};
        }

        Stats result;
        result.food_ = base_stats_cache_->food_;
        result.housing_ = base_stats_cache_->housing_;
        result.productivity_ = base_stats_cache_->productivity_;
        result.food_exports_ = 0;
        result.happiness_ = base_stats_cache_->happiness_;

        for (int i = 0; i < base_stats_cache_->commodity_count_; ++i) {
            result.commodities_.push_back(base_stats_cache_->commodities_[i]);
        }

        return result;
    }


    Optional<Coins> coin_yield_cache_load() const override
    {
        return coin_yield_cache_;
    }


private:
    struct SmallStats
    {
        s16 food_ = 0;
        s16 housing_ = 0;
        s16 productivity_ = 0;
        u8 commodity_count_;
        u8 happiness_;

        Commodity commodities_[commodities_max];
    };

    mutable Optional<SmallStats> base_stats_cache_;
    mutable Optional<Coins> coin_yield_cache_;
};



} // namespace skyland::macro::terrain
