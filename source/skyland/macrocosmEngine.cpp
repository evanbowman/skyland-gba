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


#include "macrocosmEngine.hpp"
#include "allocator.hpp"
#include "memory/buffer.hpp"
#include "platform/platform.hpp"



namespace skyland::macro
{



SystemString terrain::name(Type t)
{
    switch (t) {
    case terrain::Type::air:
        return SystemString::block_air;

    case terrain::Type::building:
        return SystemString::block_building;

    case terrain::Type::rock_edge:
    case terrain::Type::rock_stacked:
        return SystemString::block_terrain;

    case terrain::Type::masonry:
        return SystemString::block_masonry;

    default:
    case terrain::Type::selector:
        return SystemString::gs_error;

    case terrain::Type::water:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return SystemString::block_water;

    case terrain::Type::wheat:
        return SystemString::block_wheat;
    }
}



SystemString terrain::Block::name() const
{
    return terrain::name((Type)type_);
}



void terrain::Sector::rotate()
{
    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < 8 / 2; x++) {
            for (int y = x; y < 8 - x - 1; y++) {
                auto temp = blocks_[z][x][y];
                blocks_[z][x][y] = blocks_[z][y][8 - 1 - x];
                blocks_[z][y][8 - 1 - x] = blocks_[z][8 - 1 - x][8 - 1 - y];
                blocks_[z][8 - 1 - x][8 - 1 - y] = blocks_[z][8 - 1 - y][x];
                blocks_[z][8 - 1 - y][x] = temp;
            }
        }
    }

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
                auto& block = blocks_[z][x][y];
                block.repaint_ = true;
                if (block.type_ == (u8)terrain::Type::selector) {
                    cursor_ = {(u8)x, (u8)y, (u8)z};
                }
            }
        }
    }

    changed_ = true;
    shrunk_ = true;

    orientation_ = (Orientation)(((int)orientation_ + 1) % 4);
}



Buffer<terrain::Type, 10> terrain::improvements(Type t)
{
    return {};
}



u16 terrain::Sector::cursor_raster_pos() const
{
    int min = 9999;
    for (auto p : cursor_raster_tiles_) {
        if (p < min) {
            min = p;
        }
    }

    return min;
}



static bool blocks_light(terrain::Type t)
{
    static const bool result[(int)terrain::Type::count] = {
        false,
        true,
        true,
        false,
        true,
        true,
        false,
        true,
        false,
        false,
        false,
    };

    return result[(int)t];
}



void terrain::Sector::shadowcast()
{
    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
                blocks_[z][x][y].shadowed_ = false;
            }
        }
    }

    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            bool shadow = false;
            for (int z = z_limit - 1; z > -1; --z) {
                auto t = blocks_[z][x][y].type_;
                if (shadow) {
                    blocks_[z][x][y].shadowed_ = true;
                } else if (blocks_light((terrain::Type)t)) {
                    shadow = true;
                }
            }
        }
    }
}



const terrain::Block& terrain::Sector::get_block(const Vec3<u8>& coord) const
{
    return blocks_[coord.z][coord.x][coord.y];
}



void terrain::Sector::set_block(const Vec3<u8>& coord, Type type)
{
    auto& selected = blocks_[coord.z][coord.x][coord.y];
    if (selected.type_ == (u8)type) {
        return;
    }

    selected.type_ = (u8)type;
    selected.repaint_ = true;

    for (int z = coord.z - 1; z > -1; --z) {
        auto& selected = blocks_[z][coord.x][coord.y];
        if (selected.type_ not_eq 0 and not selected.shadowed_) {
            selected.repaint_ = true;
        }
    }

    shadowcast();

    changed_ = true;
}



void terrain::Sector::set_cursor(const Vec3<u8>& pos)
{
    auto old_cursor = cursor_;
    auto& block = blocks_[old_cursor.z][old_cursor.x][old_cursor.y];
    if (block.type_ == (u8)terrain::Type::selector) {
        set_block(old_cursor, macro::terrain::Type::air);
    }

    if (old_cursor.z > 0) {
        auto& block = blocks_[old_cursor.z + 1][old_cursor.x][old_cursor.y];
        block.repaint_ = true;
    }

    cursor_ = pos;
    while (blocks_[cursor_.z][cursor_.x][cursor_.y].type_ not_eq
           (u8) terrain::Type::air) {
        ++cursor_.z;
    }

    cursor_moved_ = true;

    while (cursor_.z > 0 and
           blocks_[cursor_.z - 1][cursor_.x][cursor_.y].type_ ==
               (u8)terrain::Type::air) {
        --cursor_.z;
    }

    set_block(cursor_, terrain::Type::selector);
}



static const u16 screen_mapping_lut[8][8] = {
    {14, 45, 76, 107, 138, 169, 200, 231},
    {43, 74, 105, 136, 167, 198, 229, 260},
    {72, 103, 134, 165, 196, 227, 258, 289},
    {101, 132, 163, 194, 225, 256, 287, 318},
    {130, 161, 192, 223, 254, 285, 316, 347},
    {159, 190, 221, 252, 283, 314, 345, 376},
    {188, 219, 250, 281, 312, 343, 374, 405},
    {217, 248, 279, 310, 341, 372, 403, 434}};



enum TileCategory {
    empty,
    opaque,
    top_angled_l,
    top_angled_r,
    bot_angled_l,
    bot_angled_r,
};



// Some texture indices completely cover everything underneath them, allowing
// the render to skip some steps.
static TileCategory tile_category(int texture_id)
{
    // NOTE: for our isometric tiles, the middle row is fully opaque, i.e. we
    // don't need to worry about rendering anything underneath. The top and
    // bottom rows have transparent pixels, and cannot necessarily be skipped.

    // clang-format off
    static const std::array<TileCategory, 200> category =
        {top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         empty, empty, empty, empty, empty, empty,
         empty, empty, empty, empty, empty, empty,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r,
        };
    // clang-format on

    return category[texture_id];
}



namespace raster
{



struct DepthNode
{
    Vec3<u8> position_;
    u8 tile_ = 0;
    DepthNode* next_;
};

struct DepthBufferSlab
{
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
    // NOTE: DepthBufferSlab won't fit in a single allocation.
    DynamicMemory<DepthBufferSlab> depth_1_;
    DynamicMemory<DepthBufferSlab> depth_2_;

    BulkAllocator<18> depth_node_allocator_;

    DepthBuffer(Platform& pfrm)
        : depth_1_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
          depth_2_(allocate_dynamic<DepthBufferSlab>("iso-depth-buffer")),
          depth_node_allocator_(pfrm)
    {
    }
};



} // namespace raster



void terrain::Sector::render(Platform& pfrm)
{
    // TODO: simplify this rendering code. The output texture is split across
    // two tile layers, so there's some copy-pasted code that needs to be
    // re-organized into common functions.

    if (not changed_) {
        return;
    }

    auto prev_cursor_raster_tiles = cursor_raster_tiles_;
    cursor_raster_tiles_.clear();


    auto rendering_pass = [&](auto rendering_function) {
        auto project_block = [&](int x, int y, int z) {
            auto slab = blocks_[z];

            auto& block = slab[x][y];

            if (not(block.type_ > 0)) {
                return;
            }

            int t_start = screen_mapping_lut[x][y];
            t_start += 30 * 8;
            t_start -= 30 * z;


            int texture = (block.type_ - 1) * 12 + 480;
            if (block.shadowed_) {
                texture += 6;
            }

            auto blit = [&](int texture, int t_start) {
                rendering_function(
                    Vec3<u8>{(u8)x, (u8)y, (u8)z}, texture, t_start);
                if (block.type_ == (u8)Type::selector) {
                    cursor_raster_tiles_.push_back(t_start);
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



        for (int z = 0; z < z_view_; ++z) {

            static const Vec2<u8> winding_path[] = {
                {0, 0}, {1, 0}, {0, 1}, {2, 0}, {1, 1}, {0, 2}, {3, 0}, {2, 1},
                {1, 2}, {0, 3}, {4, 0}, {3, 1}, {2, 2}, {1, 3}, {0, 4}, {5, 0},
                {4, 1}, {3, 2}, {2, 3}, {1, 4}, {0, 5}, {6, 0}, {5, 1}, {4, 2},
                {3, 3}, {2, 4}, {1, 5}, {0, 6}, {7, 0}, {6, 1}, {5, 2}, {4, 3},
                {3, 4}, {2, 5}, {1, 6}, {0, 7}, {7, 1}, {6, 2}, {5, 3}, {4, 4},
                {3, 5}, {2, 6}, {1, 7}, {7, 2}, {6, 3}, {5, 4}, {4, 5}, {3, 6},
                {2, 7}, {7, 3}, {6, 4}, {5, 5}, {4, 6}, {3, 7}, {7, 4}, {6, 5},
                {5, 6}, {4, 7}, {7, 5}, {6, 6}, {5, 7}, {7, 6}, {6, 7}, {7, 7},
            };

            for (auto& p : winding_path) {
                project_block(p.x, p.y, z);
            }
        }
    };


    std::optional<raster::DepthBuffer> db_;


    if (not db_) {
        db_.emplace(pfrm);

        rendering_pass([&](const Vec3<u8>& p, int texture, int t_start) {
            auto n = db_->depth_node_allocator_.alloc<raster::DepthNode>();
            if (n == nullptr) {
                Platform::fatal("depth node allocator out of memory!");
            }

            n->position_ = p;
            n->tile_ = texture - 480;

            if (t_start < 480) {
                n->next_ = db_->depth_1_->visible_[t_start];
                // NOTE: it's bulk allocation, there's no leak here. The destructor
                // won't be called, but we're dealing with a primitive type.
                db_->depth_1_->visible_[t_start] = n.release();
            } else {
                n->next_ = db_->depth_2_->visible_[t_start - 480];
                db_->depth_2_->visible_[t_start - 480] = n.release();
            }
        });
    }

    // A combination of tiles fully covers whatever's beneath, so no need to
    // clear out the current contents of vram.
    Bitvector<480> depth_1_skip_clear;
    Bitvector<480> depth_2_skip_clear;

    Bitvector<480> depth_1_empty;
    Bitvector<480> depth_2_empty;

    // Culling for non-visible tiles
    for (int i = 0; i < 480; ++i) {
        if (auto head = db_->depth_1_->visible_[i]) {
            auto temp = head;
            bool skip_repaint = true;
            while (temp) {
                if (cursor_moved_) {
                    for (auto& t : prev_cursor_raster_tiles) {
                        if (t == i) {
                            skip_repaint = false;
                        }
                    }
                }
                auto pos = temp->position_;
                if (blocks_[pos.z][pos.x][pos.y].repaint_) {
                    skip_repaint = false;
                }
                temp = temp->next_;
            }
            if (skip_repaint) {
                depth_1_skip_clear.set(i, true);
                db_->depth_1_->visible_[i] = nullptr;
                continue;
            }
            Buffer<TileCategory, 8> seen;
            while (head) {
                auto cg = tile_category(head->tile_);
                if (cg == opaque) {
                    // Cull non-visible tiles.
                    head->next_ = nullptr;
                    depth_1_skip_clear.set(i, true);
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
                                depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case top_angled_r:
                        for (auto& s : seen) {
                            if (s == bot_angled_l) {
                                head->next_ = nullptr;
                                depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_l:
                        for (auto& s : seen) {
                            if (s == top_angled_r) {
                                head->next_ = nullptr;
                                depth_1_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_r:
                        for (auto& s : seen) {
                            if (s == top_angled_l) {
                                head->next_ = nullptr;
                                depth_1_skip_clear.set(i, true);
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
            depth_1_empty.set(i, true);
        }
        if (auto head = db_->depth_2_->visible_[i]) {
            auto temp = head;
            bool skip_repaint = true;
            while (temp) {
                if (cursor_moved_) {
                    for (auto& t : prev_cursor_raster_tiles) {
                        if (t - 480 == i) {
                            skip_repaint = false;
                        }
                    }
                }
                auto pos = temp->position_;
                if (blocks_[pos.z][pos.x][pos.y].repaint_) {
                    skip_repaint = false;
                }
                temp = temp->next_;
            }
            if (skip_repaint) {
                depth_2_skip_clear.set(i, true);
                db_->depth_2_->visible_[i] = nullptr;
                continue;
            }
            Buffer<TileCategory, 8> seen;
            while (head) {
                auto cg = tile_category(head->tile_);
                if (cg == opaque) {
                    // Cull non-visible tiles.
                    head->next_ = nullptr;
                    depth_2_skip_clear.set(i, true);
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
                                depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case top_angled_r:
                        for (auto& s : seen) {
                            if (s == bot_angled_l) {
                                head->next_ = nullptr;
                                depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_l:
                        for (auto& s : seen) {
                            if (s == top_angled_r) {
                                head->next_ = nullptr;
                                depth_2_skip_clear.set(i, true);
                                break;
                            }
                        }
                        break;

                    case bot_angled_r:
                        for (auto& s : seen) {
                            if (s == top_angled_l) {
                                head->next_ = nullptr;
                                depth_2_skip_clear.set(i, true);
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
            depth_2_empty.set(i, true);
        }
    }



    // Actually perform the rendering. At this point, ideally, everything that's
    // not actually visible in the output should have been removed from the
    // depth buffer.
    pfrm.system_call("vsync", nullptr);
    for (int i = 0; i < 480; ++i) {

        if (auto head = db_->depth_1_->visible_[i]) {
            if (not depth_1_skip_clear.get(i)) {
                pfrm.blit_t0_erase(i);
            }

            Buffer<int, 6> stack;
            while (head) {
                stack.push_back(head->tile_);
                if (head->tile_) {
                }
                head = head->next_;
            }

            while (not stack.empty()) {
                int tile = stack.back();
                pfrm.blit_t0_tile_to_texture(tile + 480, i, false);
                stack.pop_back();
            }
        } else if (shrunk_ and not depth_1_skip_clear.get(i)) {
            pfrm.blit_t0_erase(i);
        }

        if (auto head = db_->depth_2_->visible_[i]) {
            if (not depth_2_skip_clear.get(i)) {
                pfrm.blit_t1_erase(i);
            }

            Buffer<int, 6> stack;
            while (head) {
                stack.push_back(head->tile_);
                head = head->next_;
            }

            while (not stack.empty()) {
                int tile = stack.back();
                pfrm.blit_t1_tile_to_texture(tile + 480, i, false);
                stack.pop_back();
            }
        } else if (shrunk_ and not depth_2_skip_clear.get(i)) {
            pfrm.blit_t1_erase(i);
        }
    }


    if (cursor_moved_) {
        // Handle these out of line, as not to slow down the main rendering
        // block.
        for (int i = 0; i < 480; ++i) {
            if (cursor_moved_ and depth_1_empty.get(i)) {
                pfrm.blit_t0_erase(i);
            }
            if (cursor_moved_ and depth_2_empty.get(i)) {
                pfrm.blit_t1_erase(i);
            }
        }
    }


    for (auto& layer : blocks_) {
        for (auto& slice : layer) {
            for (auto& block : slice) {
                block.repaint_ = false;
            }
        }
    }

    changed_ = false;
    shrunk_ = false;
    cursor_moved_ = false;
}



// clang-format off
typedef void(*UpdateFunction)(terrain::Sector&, terrain::Block&, Vec3<u8>);
static const UpdateFunction update_functions[(int)terrain::Type::count] = {
    nullptr, // Air has no update code.
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        // TODO...
    },
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        // TODO...
    },
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        // Vec3<u8> spread_pos{(u8)(position.x + 1), position.y, position.z};
        // auto a = s.get_block(spread_pos);
        // if (a.type_ == (u8)terrain::Type::air) {
        //     s.set_block(spread_pos, terrain::Type::water_slant_a);
        // }
    },
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        // TODO...
    },
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        // TODO...
    },
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ > 6) {
            block.data_ = 0;
            block.shadowed_ = not block.shadowed_;
            block.repaint_ = true;
            s.changed_ = true;
        }

    },
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (block.shadowed_) {
            block.type_ = (u8)terrain::Type::rock_stacked;
            block.repaint_ = true;
            s.changed_ = true;
        }
    },
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        // TODO...
    },
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        // TODO...
    },
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {

    },
};
// clang-format on



void terrain::Sector::update(Platform& pfrm)
{
    for (u8 z = 0; z < z_limit; ++z) {
        for (u8 x = 0; x < 8; ++x) {
            for (u8 y = 0; y < 8; ++y) {

                auto& block = blocks_[z][x][y];

                auto update = update_functions[block.type_];
                if (update) {
                    update(*this, block, {x, y, z});
                }
            }
        }
    }
}



} // namespace skyland::macro
