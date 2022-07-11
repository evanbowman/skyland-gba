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


    enum { commodities_max = 5 };


    void base_stats_cache_store(const Stats& s) const override
    {
        base_stats_cache_.emplace();
        base_stats_cache_->food_ = s.food_;
        base_stats_cache_->housing_ = s.housing_;
        base_stats_cache_->employment_ = s.employment_;
        base_stats_cache_->happiness_ = s.happiness_;

        base_stats_cache_->commodity_count_ =
            std::min((int)commodities_max, (int)s.commodities_.size());

        for (u32 i = 0; i < base_stats_cache_->commodity_count_; ++i) {
            base_stats_cache_->commodities_[i] = s.commodities_[i];
        }
    }


    std::optional<Stats> base_stats_cache_load() const override
    {
        if (not base_stats_cache_) {
            return {};
        }

        Stats result;
        result.food_ = base_stats_cache_->food_;
        result.housing_ = base_stats_cache_->housing_;
        result.employment_ = base_stats_cache_->employment_;
        result.food_exports_ = 0;
        result.happiness_ = base_stats_cache_->happiness_;

        for (int i = 0; i < base_stats_cache_->commodity_count_; ++i) {
            result.commodities_.push_back(base_stats_cache_->commodities_[i]);
        }

        return result;
    }


private:
    struct SmallStats
    {
        s16 food_ = 0;
        s16 housing_ = 0;
        s16 employment_ = 0;
        u8 commodity_count_;
        u8 happiness_;

        Commodity commodities_[commodities_max];
    };

    mutable std::optional<SmallStats> base_stats_cache_;
};



} // namespace skyland::macro::terrain
