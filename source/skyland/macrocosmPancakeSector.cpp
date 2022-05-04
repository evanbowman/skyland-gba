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


#include "macrocosmPancakeSector.hpp"
#include "macrocosmEngine.hpp"



// Ok, I really tried to do this without polymorphism. Originally, I wanted to
// just have one simple sector class. But the game lagged on the gba and by
// trial and error, I ended up with this approach. Alternatively, I suppose I
// could have defined the whole class in a header, and parameterized the side
// lengths etc. as class template parameters. TODO I guess.



namespace skyland::macro
{



void terrain::PancakeSector::restore (const Persistent& p, u8 blocks[4][12][12])
{
    erase();

    memcpy(&p_, &p, sizeof p);

    for (u8 z = 0; z < 4; ++z) {
        for (u8 x = 0; x < length; ++x) {
            for (u8 y = 0; y < length; ++y) {
                blocks_[z][x][y].type_ = blocks[z][x][y];
                blocks_[z][x][y].repaint_ = true;
                blocks_[z][x][y].data_ = 0;
            }
        }
    }
}



void terrain::PancakeSector::erase()
{
    set_name("");

    for (auto& slab : blocks_) {
        for (auto& slice : slab) {
            for (auto& block : slice) {
                block.type_ = (u8)Type::air;
                block.repaint_ = true;
                block.shadowed_ = true;
            }
        }
    }

    exports_.clear();

    base_stats_cache_.reset();
}



static Vec3<u8> rotate_coord(Vec3<u8> input)
{
    return {
        (u8)((terrain::PancakeSector::length - 1) - input.y), input.x, input.z};
}



void terrain::PancakeSector::rotate()
{
    // NOTE: I decided to implement rotation by actually rotating the level's
    // blocks and fixing up the coordinates. Simplifies the renderer,
    // basically. Somewhat, costly, but an infrequent operation, unlike
    // rendering.

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < length / 2; x++) {
            for (int y = x; y < length - x - 1; y++) {
                auto temp = blocks_[z][x][y];
                temp.repaint_ = true;
                blocks_[z][x][y] = blocks_[z][y][length - 1 - x];
                blocks_[z][y][length - 1 - x] =
                    blocks_[z][length - 1 - x][length - 1 - y];
                blocks_[z][length - 1 - x][length - 1 - y] =
                    blocks_[z][length - 1 - y][x];
                blocks_[z][length - 1 - y][x] = temp;
            }
        }
    }

    for (auto& exp : exports_) {
        exp.source_coord_ = rotate_coord(exp.source_coord_);
    }

    static_assert(z_limit == 4);

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < length; ++x) {
            for (int y = 0; y < length; ++y) {
                auto& block = blocks_[z][x][y];
                block.repaint_ = true;
                switch (block.type()) {
                case terrain::Type::selector:
                    p_.cursor_ = {(u8)x, (u8)y, (u8)z};
                    break;

                case terrain::Type::lava_spread_laterally_a:
                    block.type_ = (u8)terrain::Type::lava_spread_laterally_b;
                    break;

                case terrain::Type::lava_spread_laterally_b:
                    block.type_ = (u8)terrain::Type::lava_spread_laterally_c;
                    break;

                case terrain::Type::lava_spread_laterally_c:
                    block.type_ = (u8)terrain::Type::lava_spread_laterally_d;
                    break;

                case terrain::Type::lava_spread_laterally_d:
                    block.type_ = (u8)terrain::Type::lava_spread_laterally_a;
                    break;

                case terrain::Type::water_spread_laterally_a:
                    block.type_ = (u8)terrain::Type::water_spread_laterally_b;
                    break;

                case terrain::Type::water_spread_laterally_b:
                    block.type_ = (u8)terrain::Type::water_spread_laterally_c;
                    break;

                case terrain::Type::water_spread_laterally_c:
                    block.type_ = (u8)terrain::Type::water_spread_laterally_d;
                    break;

                case terrain::Type::water_spread_laterally_d:
                    block.type_ = (u8)terrain::Type::water_spread_laterally_a;
                    break;

                case terrain::Type::water_slant_a:
                    block.type_ = (u8)terrain::Type::water_slant_b;
                    break;

                case terrain::Type::water_slant_b:
                    block.type_ = (u8)terrain::Type::water_slant_c;
                    break;

                case terrain::Type::water_slant_c:
                    block.type_ = (u8)terrain::Type::water_slant_d;
                    break;

                case terrain::Type::water_slant_d:
                    block.type_ = (u8)terrain::Type::water_slant_a;
                    break;

                case terrain::Type::lava_slant_a:
                    block.type_ = (u8)terrain::Type::lava_slant_b;
                    break;

                case terrain::Type::lava_slant_b:
                    block.type_ = (u8)terrain::Type::lava_slant_c;
                    break;

                case terrain::Type::lava_slant_c:
                    block.type_ = (u8)terrain::Type::lava_slant_d;
                    break;

                case terrain::Type::lava_slant_d:
                    block.type_ = (u8)terrain::Type::lava_slant_a;
                    break;

                default:
                    break;
                }
            }
        }
    }

    raster::globalstate::_changed = true;

    // Technically, the level didn't shrink, but after a rotation, there may be
    // raster slots where a block previously existed but no longer does, so we
    // want to run the same logic to zero out screen entries as we do when
    // removing a block.
    raster::globalstate::_shrunk = true;

    p_.orientation_ = (Orientation)(((int)p_.orientation_ + 1) % 4);
}



static bool blocks_light(terrain::Type t)
{
    if (t == terrain::Type::air or t == terrain::Type::selector or
        (terrain::categories(t) & terrain::Categories::fluid_water) or
        (terrain::categories(t) & terrain::Categories::fluid_lava)) {
        return false;
    }

    return true;
}



void terrain::PancakeSector::shadowcast()
{
    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < length; ++x) {
            for (int y = 0; y < length; ++y) {
                blocks_[z][x][y].shadowed_ = false;
            }
        }
    }

    for (int x = 0; x < length; ++x) {
        for (int y = 0; y < length; ++y) {
            bool shadow = false;
            for (int z = z_limit - 1; z > -1; --z) {
                auto t = blocks_[z][x][y].type();
                if (shadow) {
                    blocks_[z][x][y].shadowed_ = true;
                } else if (blocks_light(t)) {
                    shadow = true;
                }
            }
        }
    }

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < length; ++x) {
            for (int y = 0; y < length; ++y) {
                auto& block = blocks_[z][x][y];
                if (block.type() == Type::light_source) {
                    // beneath:
                    for (int zz = z - 1; zz > z - 4; --zz) {
                        if (zz > -1) {
                            auto& block = blocks_[zz][x][y];
                            blocks_[zz][x][y].shadowed_ = false;
                            if (blocks_light(block.type())) {
                                break;
                            }
                        }
                    }

                    // raycast positive x:
                    for (int xx = x + 1; xx < x + 3; ++xx) {
                        if (xx < length) {
                            auto& block = blocks_[z][xx][y];
                            if (blocks_light(block.type())) {
                                break;
                            }
                            block.shadowed_ = false;
                            if (z > 0) {
                                blocks_[z - 1][xx][y].shadowed_ = false;
                            }
                        }
                    }

                    // raycast negative x:
                    for (int xx = x - 1; xx > x - 3; --xx) {
                        if (xx > -1) {
                            auto& block = blocks_[z][xx][y];
                            if (blocks_light(block.type())) {
                                break;
                            }
                            block.shadowed_ = false;
                            if (z > 0) {
                                blocks_[z - 1][xx][y].shadowed_ = false;
                            }
                        }
                    }

                    // raycast positive y:
                    for (int yy = y + 1; yy < y + 3; ++yy) {
                        if (yy < length) {
                            auto& block = blocks_[z][x][yy];
                            if (blocks_light(block.type())) {
                                break;
                            }
                            block.shadowed_ = false;
                            if (z > 0) {
                                blocks_[z - 1][x][yy].shadowed_ = false;
                            }
                        }
                    }

                    // raycast negative y:
                    for (int yy = y - 1; yy > y - 3; --yy) {
                        if (yy > -1) {
                            auto& block = blocks_[z][x][yy];
                            if (blocks_light(block.type())) {
                                break;
                            }
                            block.shadowed_ = false;
                            if (z > 0) {
                                blocks_[z - 1][x][yy].shadowed_ = false;
                            }
                        }
                    }

                    // above:
                    for (int zz = z + 1; zz < z + 3; ++zz) {
                        if (zz < z_limit) {
                            auto& block = blocks_[zz][x][y];
                            if (blocks_light(block.type())) {
                                break;
                            }
                            blocks_[zz][x][y].shadowed_ = false;
                        }
                    }
                }
            }
        }
    }
}



const terrain::Block&
terrain::PancakeSector::get_block(const Vec3<u8>& coord) const
{
    return blocks_[coord.z][coord.x][coord.y];
}



terrain::Block& terrain::PancakeSector::ref_block(const Vec3<u8>& coord)
{
    return blocks_[coord.z][coord.x][coord.y];
}



void terrain::PancakeSector::set_repaint(bool value)
{
    for (auto& slab : blocks_) {
        for (auto& slice : slab) {
            for (auto& block : slice) {
                block.repaint_ = value;
            }
        }
    }
}



static const u16 screen_mapping_lut[12][12] = {
    {14, 45, 76, 107, 138, 169, 200, 231, 262, 293, 324, 355},
    {43, 74, 105, 136, 167, 198, 229, 260, 291, 322, 353, 384},
    {72, 103, 134, 165, 196, 227, 258, 289, 320, 351, 382, 413},
    {101, 132, 163, 194, 225, 256, 287, 318, 349, 380, 411, 442},
    {130, 161, 192, 223, 254, 285, 316, 347, 378, 409, 440, 471},
    {159, 190, 221, 252, 283, 314, 345, 376, 407, 438, 469, 500},
    {188, 219, 250, 281, 312, 343, 374, 405, 436, 467, 498, 529},
    {217, 248, 279, 310, 341, 372, 403, 434, 465, 496, 527, 558},
    {246, 277, 308, 339, 370, 401, 432, 463, 494, 525, 556, 587},
    {275, 306, 337, 368, 399, 430, 461, 492, 523, 554, 585, 616},
    {304, 335, 366, 397, 428, 459, 490, 521, 552, 583, 614, 645},
    {333, 364, 395, 426, 457, 488, 519, 550, 581, 612, 643, 674},
};



void terrain::PancakeSector::render_setup(Platform& pfrm)
{
    // FIXME: this function is copy-pasted, with only the block array member,
    // winding path, and screen mapping lut changed.

    using namespace raster;

    if (z_view_ > z_limit) {
        z_view_ = z_limit;
    }


    if (globalstate::_recast_shadows) {
        shadowcast();
        globalstate::_recast_shadows = false;
    }

    if (not globalstate::_changed) {
        return;
    }

    if (_db) {
        // Already setup!
        return;
    }

    auto prev_cursor_raster_tiles = globalstate::_cursor_raster_tiles;
    globalstate::_cursor_raster_tiles.clear();

    const bool cursor_moved = globalstate::_cursor_moved;
    const bool grew = globalstate::_grew;


    auto rendering_pass = [&](auto rendering_function) {
        auto project_block = [&](int x, int y, int z) {
            auto slab = blocks_[z];

            auto& block = slab[x][y];

            if (not(block.type_ > 0)) {
                return;
            }

            int t_start = screen_mapping_lut[x][y];
            t_start += 30 * 3;
            t_start -= 30 * z;


            int texture = (block.type_ - 1) * 12 + 480;
            if (block.shadowed_) {
                texture += 6;
            }

            auto blit = [&](int texture, int t_start) {
                rendering_function(
                    Vec3<u8>{(u8)x, (u8)y, (u8)z}, texture, t_start);
                if (block.type() == Type::selector) {
                    globalstate::_cursor_raster_tiles.push_back(t_start);
                }

                if (block.repaint_) {
                    if (t_start < 480) {
                        (*_db)->depth_1_needs_repaint.set(t_start, true);
                    } else {
                        (*_db)->depth_2_needs_repaint.set(t_start - 480, true);
                    }
                }
            };

            blit(texture, t_start);
            blit(texture + 1, t_start + 1);

            t_start += 30;

            blit(texture + 2, t_start);
            blit(texture + 3, t_start + 1);

            t_start += 30;

            blit(texture + 4, t_start);
            blit(texture + 5, t_start + 1);
        };


        static const Vec2<u8> winding_path[] = {
            {0, 0},  {1, 0},   {0, 1},   {2, 0},   {1, 1},  {0, 2},  {3, 0},
            {2, 1},  {1, 2},   {0, 3},   {4, 0},   {3, 1},  {2, 2},  {1, 3},
            {0, 4},  {5, 0},   {4, 1},   {3, 2},   {2, 3},  {1, 4},  {0, 5},
            {6, 0},  {5, 1},   {4, 2},   {3, 3},   {2, 4},  {1, 5},  {0, 6},
            {7, 0},  {6, 1},   {5, 2},   {4, 3},   {3, 4},  {2, 5},  {1, 6},
            {0, 7},  {8, 0},   {7, 1},   {6, 2},   {5, 3},  {4, 4},  {3, 5},
            {2, 6},  {1, 7},   {0, 8},   {9, 0},   {8, 1},  {7, 2},  {6, 3},
            {5, 4},  {4, 5},   {3, 6},   {2, 7},   {1, 8},  {0, 9},  {10, 0},
            {9, 1},  {8, 2},   {7, 3},   {6, 4},   {5, 5},  {4, 6},  {3, 7},
            {2, 8},  {1, 9},   {0, 10},  {11, 0},  {10, 1}, {9, 2},  {8, 3},
            {7, 4},  {6, 5},   {5, 6},   {4, 7},   {3, 8},  {2, 9},  {1, 10},
            {0, 11}, {11, 1},  {10, 2},  {9, 3},   {8, 4},  {7, 5},  {6, 6},
            {5, 7},  {4, 8},   {3, 9},   {2, 10},  {1, 11}, {11, 2}, {10, 3},
            {9, 4},  {8, 5},   {7, 6},   {6, 7},   {5, 8},  {4, 9},  {3, 10},
            {2, 11}, {11, 3},  {10, 4},  {9, 5},   {8, 6},  {7, 7},  {6, 8},
            {5, 9},  {4, 10},  {3, 11},  {11, 4},  {10, 5}, {9, 6},  {8, 7},
            {7, 8},  {6, 9},   {5, 10},  {4, 11},  {11, 5}, {10, 6}, {9, 7},
            {8, 8},  {7, 9},   {6, 10},  {5, 11},  {11, 6}, {10, 7}, {9, 8},
            {8, 9},  {7, 10},  {6, 11},  {11, 7},  {10, 8}, {9, 9},  {8, 10},
            {7, 11}, {11, 8},  {10, 9},  {9, 10},  {8, 11}, {11, 9}, {10, 10},
            {9, 11}, {11, 10}, {10, 11}, {11, 11},

        };

        for (int z = 0; z < z_view_; ++z) {

            for (auto& p : winding_path) {
                project_block(p.x, p.y, z);
            }
        }
    };


    if (not _db) {
        _db.emplace(
            allocate_dynamic<raster::DepthBuffer>("depth-buffer", pfrm));
    }

    rendering_pass([&](const Vec3<u8>& p, int texture, int t_start) {
        auto n = (*_db)->depth_node_allocator_.alloc<DepthNode>();
        if (n == nullptr) {
            Platform::fatal("depth node allocator out of memory!");
        }

        n->set_position(p);
        n->tile_ = texture - 480;

        if (t_start < 480) {
            n->next_ = (*_db)->depth_1_->visible_[t_start];
            // NOTE: it's bulk allocation, there's no leak here. The destructor
            // won't be called, but we're dealing with a primitive type.
            (*_db)->depth_1_->visible_[t_start] = n.release();
        } else {
            n->next_ = (*_db)->depth_2_->visible_[t_start - 480];
            (*_db)->depth_2_->visible_[t_start - 480] = n.release();
        }
    });


    if (cursor_moved) {
        for (auto& t : prev_cursor_raster_tiles) {
            if (t < 480) {
                (*_db)->depth_1_cursor_redraw.set(t, true);
            } else {
                (*_db)->depth_2_cursor_redraw.set(t - 480, true);
            }
        }
        for (auto& t : globalstate::_cursor_raster_tiles) {
            if (t < 480) {
                (*_db)->depth_1_cursor_redraw.set(t, true);
            } else {
                (*_db)->depth_2_cursor_redraw.set(t - 480, true);
            }
        }
    }


    for (int i = 0; i < 480; ++i) {
        if (auto head = (*_db)->depth_1_->visible_[i]) {
            bool skip_repaint = true;
            if ((*_db)->depth_1_cursor_redraw.get(i)) {
                skip_repaint = false;
            }

            // Some tiles think that they need to be repainted. But we know that
            // the cursor simply moved position, the terrain layout did not in
            // fact change, and we can often skip this step entirely.
            if (grew or not cursor_moved) {
                if ((*_db)->depth_1_needs_repaint.get(i)) {
                    skip_repaint = false;
                }
            }

            if (skip_repaint) {
                (*_db)->depth_1_skip_clear.set(i, true);
                (*_db)->depth_1_->visible_[i] = nullptr;
                continue;
            }

            Buffer<TileCategory, 8> seen;
            while (head) {
                auto cg = tile_category(head->tile_);
                if (cg == opaque) {
                    // Cull non-visible tiles.
                    head->next_ = nullptr;
                    (*_db)->depth_1_skip_clear.set(i, true);
                    break;
                } else {
                    switch (cg) {
                    default:
                        break;

                    case top_angled_l:
                        // Basically, if we have a top slanted tile going in one
                        // direction, and the bottom tile slanted in the
                        // opposite direction has been rendered, then everything
                        // below would be covered up, so there's no need to draw
                        // anything beneath.
                        for (auto& s : seen) {
                            if (s == bot_angled_r) {
                                head->next_ = nullptr;
                                (*_db)->depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case top_angled_r:
                        for (auto& s : seen) {
                            if (s == bot_angled_l) {
                                head->next_ = nullptr;
                                (*_db)->depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_l:
                        for (auto& s : seen) {
                            if (s == top_angled_r) {
                                head->next_ = nullptr;
                                (*_db)->depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_r:
                        for (auto& s : seen) {
                            if (s == top_angled_l) {
                                head->next_ = nullptr;
                                (*_db)->depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;
                    }
                    seen.push_back(cg);
                }

                head = head->next_;
            }
        } else {
            (*_db)->depth_1_empty.set(i, true);
        }
        if (auto head = (*_db)->depth_2_->visible_[i]) {
            bool skip_repaint = true;
            if ((*_db)->depth_2_cursor_redraw.get(i)) {
                skip_repaint = false;
            }
            if (grew or not cursor_moved) {
                if ((*_db)->depth_2_needs_repaint.get(i)) {
                    skip_repaint = false;
                }
            }
            if (skip_repaint) {
                (*_db)->depth_2_skip_clear.set(i, true);
                (*_db)->depth_2_->visible_[i] = nullptr;
                continue;
            }
            Buffer<TileCategory, 8> seen;
            while (head) {
                auto cg = tile_category(head->tile_);
                if (cg == opaque) {
                    // Cull non-visible tiles.
                    head->next_ = nullptr;
                    (*_db)->depth_2_skip_clear.set(i, true);
                    break;
                } else {
                    switch (cg) {
                    default:
                        break;

                    case top_angled_l:
                        // Basically, if we have a top slanted tile going in one
                        // direction, and the bottom tile slanted in the
                        // opposite direction has been rendered, then everything
                        // below would be covered up, so there's no need to draw
                        // anything beneath.
                        for (auto& s : seen) {
                            if (s == bot_angled_r) {
                                head->next_ = nullptr;
                                (*_db)->depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case top_angled_r:
                        for (auto& s : seen) {
                            if (s == bot_angled_l) {
                                head->next_ = nullptr;
                                (*_db)->depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_l:
                        for (auto& s : seen) {
                            if (s == top_angled_r) {
                                head->next_ = nullptr;
                                (*_db)->depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_r:
                        for (auto& s : seen) {
                            if (s == top_angled_l) {
                                head->next_ = nullptr;
                                (*_db)->depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;
                    }
                    seen.push_back(cg);
                }

                head = head->next_;
            }
        } else {
            (*_db)->depth_2_empty.set(i, true);
        }
    }

    // Performs drawing for jagged edge tiles in software.
    for (int i = 0; i < 480; ++i) {

        auto insert_edges = [&](auto head) {
            bool has_tl = false;
            bool has_tr = false;

            while (head->next_) {
                auto cat = tile_category(head->tile_);
                // The top-left or top-right tile would obscure the one that we
                // want to draw anyway, so skip it.
                if (cat == TileCategory::top_angled_l) {
                    has_tl = true;
                }
                if (cat == TileCategory::top_angled_r) {
                    has_tr = true;
                }
                head = head->next_;
            }

            const u16 edge_l = 496 - 480;
            const u16 edge_r = 497 - 480;

            auto cat = tile_category(head->tile_);
            if (head->position().z == 0 and head->tile_ not_eq edge_l and
                head->tile_ not_eq edge_r) {
                if ((cat == bot_angled_l and not has_tr) or
                    (cat == bot_angled_r and not has_tl)) {
                    auto n = (*_db)->depth_node_allocator_.alloc<DepthNode>();
                    n->set_position(head->position());
                    n->next_ = nullptr;

                    if (cat == bot_angled_l) {
                        n->tile_ = edge_l;
                    } else if (cat == bot_angled_r) {
                        n->tile_ = edge_r;
                    }

                    head->next_ = n.release();
                }
            }
        };

        if (auto head = (*_db)->depth_1_->visible_[i]) {
            insert_edges(head);
        }

        if (auto head = (*_db)->depth_2_->visible_[i]) {
            insert_edges(head);
        }
    }

    for (u32 i = 0; i < globalstate::_cursor_raster_tiles.size(); ++i) {
        auto t = globalstate::_cursor_raster_tiles[i];

        globalstate::_cursor_raster_stack[i].clear();

        DepthNode* head = nullptr;
        if (t >= 480) {
            head = (*_db)->depth_2_->visible_[t - 480];
        } else {
            head = (*_db)->depth_1_->visible_[t];
        }

        while (head) {
            globalstate::_cursor_raster_stack[i].push_back(head->tile_);
            head = head->next_;
        }
    }
}



} // namespace skyland::macro
