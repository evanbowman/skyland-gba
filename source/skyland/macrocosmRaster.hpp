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



#include "allocator.hpp"



namespace skyland::macro
{



enum {
#ifdef __GBA__
    // Sized based on the GBA's hardware vram limits.
    RASTER_CELLCOUNT = 480,
#else
    // TODO: increase this?
    RASTER_CELLCOUNT = 480,
#endif
};



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

// Day/Night
extern bool is_night;

// Specific screen entries queued for redraw. The engine will only render blocks
// overlapping with the raster entry bits in this array. Without optimizations
// to only redraw a subset of the map, the game lags significantly. Added after
// a month of work on the engine, improved cursor scrolling speed
// significantly. I did previous optimizations to only redraw changed blocks,
// but storing a persistent global variable in this way allowed me to also skip
// large parts of the depth testing, in addition to the redraw.
//
// NOTE: IMPORTANT: The engine will get confused and drop tiles if a block is
// created and destroyed within the same frame. Not relevant in most cases, but
// worth noting. i.e. I had to be careful to make sure that the cursor block
// does not scroll e.g. right then down in the same frame, would result in
// erasure of tiles under the block's first (right) position.
extern Bitvector<RASTER_CELLCOUNT * 2> _recalc_depth_test;


// Only render to the upper half of the renderwindow. Used for the title screen.
extern bool _upper_half_only;


} // namespace globalstate



struct DepthNode
{
    DepthNode* next_;

    u16 tile_;
    u16 position_;

    // NOTE: not bounds-checked! Do not specify coordinate values in excess of
    // 31!
    void set_position(const Vec3<u8>& pos)
    {
        position_ = pos.x << 10 | pos.y << 5 | pos.z;
    }

    Vec3<u8> position()
    {
        return {u8(position_ >> 10),
                u8((position_ >> 5) & 0b11111),
                u8(position_ & 0b11111)};
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
    DepthNode* visible_[RASTER_CELLCOUNT];

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

    BumpAllocator<24, DepthNode> depth_node_allocator_;

    DepthBuffer()
        : depth_1_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
          depth_2_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer"))
    {
    }

    Bitvector<RASTER_CELLCOUNT> depth_1_needs_repaint;
    Bitvector<RASTER_CELLCOUNT> depth_2_needs_repaint;

    Bitvector<RASTER_CELLCOUNT> depth_1_cursor_redraw;
    Bitvector<RASTER_CELLCOUNT> depth_2_cursor_redraw;
};



extern Optional<DynamicMemory<raster::DepthBuffer>> _db;



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
} // namespace skyland::macro
