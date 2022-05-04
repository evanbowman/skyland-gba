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
#include "macrocosmCubeSector.hpp"
#include "macrocosmSector.hpp"
#include "number/int.h"
#include "systemString.hpp"



class Platform;



namespace skyland::macro
{



namespace raster
{
namespace globalstate
{

// Sorry about all of these state flags. I would definitely grumble a bit if I
// had to work on anyone else's code if it were written in this way. We're
// really trying to push the GBA hardware by redrawing as little as possible,
// and we want to draw things differently depending on the specific way that the
// terrain changed.

// Recast shadows.
extern bool _recast_shadows;

// The layout of the world changed in some way.
extern bool _changed;

// The level layout got smaller. i.e. we need to do extra work to redraw (erase)
// areas even where there are now no blocks, because we removed some.
extern bool _shrunk;

// We added blocks to the level. Normally, we throw out all draw calls for tiles
// that don't overlap with the current or previous cursor block, unless the
// terrain grew, i.e. if we added a block.
extern bool _grew;

// The cursor moved. If the cursor moved, and the structure of the level did not
// grow (no blocks added), we can do all sorts of optimizations to render the
// cursor faster. Cursor rendering is generally pretty heavily optimized,
// because if the cursor movement lags, players would easily notice.
extern bool _cursor_moved;

// Repaint required, but only because the cursor toggled between light and
// dark. Even more heavily optimized than cursor movement. Redoing the
// depth-test each time that the cursor idly flickers would burn battery for no
// particular purpose. While we don't want to keep the entire depth buffer in
// memory, we keep a cache of the rendering stack expressly for redrawing the
// flickering cursor without needing to redo the depth test.
extern bool _changed_cursor_flicker_only;

// Not exclusively a rendering optimization, actually! It's simply useful to
// keep a cache of screen tiles overlapping with the cursor, for game logic that
// cares about the normalized position of the cursor with respect to the screen
// (e.g. camera movement, effects).
extern Buffer<u16, 6> _cursor_raster_tiles;

// Used exclusively for optimizing the cursor flickering animation. If the
// cursor tile is at end of the buffer, then it can be redrawn without worrying
// about anything beneath it (because the pixels for the light and the dark
// cursor are the same).
extern Buffer<u16, 6> _cursor_raster_stack[6];
} // namespace globalstate



struct DepthNode
{
    DepthNode* next_;

    // NOTE: a sector cube has dimensions 8x8x9. Bitfields sized accordingly.
    u16 tile_;
    u16 x_pos_ : 4;
    u16 y_pos_ : 4;
    u16 z_pos_ : 4;
    u16 unused_ : 4;

    void set_position(const Vec3<u8>& pos)
    {
        x_pos_ = pos.x;
        y_pos_ = pos.y;
        z_pos_ = pos.z;
    }

    Vec3<u8> position()
    {
        return {(u8)x_pos_, (u8)y_pos_, (u8)z_pos_};
    }
};
#ifdef __GBA__
static_assert(sizeof(DepthNode) == 8);
#endif


struct DepthBufferSlab
{
    // NOTE: we use two vertically-adjacent tile layers. The 2d strategy engine
    // for skyland, which I developed first, needs two tile layers, and rather
    // than redesign the hardware abstraction layer to handle tile layers of
    // varying sizes, it's simply easier to handle things this way. Not
    // convinced? The purpose of the hardware interface is to implement the
    // minimal behavior needed to port the game to a new platform. Any feature
    // that I add to the Platform class makes my life more difficult when trying
    // to port the game.
    DepthNode* visible_[480];

    DepthBufferSlab()
    {
        for (auto& node : visible_) {
            node = nullptr;
        }
    }
};

struct DepthBuffer
{
    // NOTE: the two DepthBufferSlabs won't fit in a single allocation.
    DynamicMemory<DepthBufferSlab> depth_1_;
    DynamicMemory<DepthBufferSlab> depth_2_;

    BulkAllocator<18> depth_node_allocator_;

    DepthBuffer(Platform& pfrm)
        : depth_1_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
          depth_2_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
          depth_node_allocator_(pfrm)
    {
    }

    Bitvector<480> depth_1_needs_repaint;
    Bitvector<480> depth_2_needs_repaint;

    Bitvector<480> depth_1_cursor_redraw;
    Bitvector<480> depth_2_cursor_redraw;

    Bitvector<480> depth_1_skip_clear;
    Bitvector<480> depth_2_skip_clear;

    Bitvector<480> depth_1_empty;
    Bitvector<480> depth_2_empty;
};



extern std::optional<DynamicMemory<raster::DepthBuffer>> _db;



enum TileCategory {
    irregular,
    opaque,
    top_angled_l,
    top_angled_r,
    bot_angled_l,
    bot_angled_r,
};



TileCategory tile_category(int texture_id);



} // namespace raster


struct State;
extern State* _bound_state;


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



struct State
{
    static const int max_sectors = 20;

    struct Data
    {
        Data() : origin_sector_({0, 0})
        {
        }

        Data(const Data&) = delete;

        ~Data()
        {
            erase_other_sectors();
        }

        macro::terrain::CubeSector origin_sector_;


        std::optional<BulkAllocator<max_sectors - 1>> other_sector_mem_;

        void erase_other_sectors()
        {
            // NOTE: allocated from a bulk allocator, does not need to be freed,
            // but still need to invoke destructors. Not using unique ptr with
            // deleter due to type errors from upcast. FIXME: is there a way to
            // use unique_ptr instead?
            for (auto& sector : other_sectors_) {
                sector->~Sector();
            }
            other_sectors_.clear();
        }

        Buffer<terrain::Sector*, max_sectors - 1> other_sectors_;



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



    bool make_sector(Vec2<s8> coord, terrain::Sector::Shape shape);



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


    State(Platform&);


    DynamicMemory<Data> data_;


    void advance(int elapsed_years);
};



} // namespace skyland::macro
