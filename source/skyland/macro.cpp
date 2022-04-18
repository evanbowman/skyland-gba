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


#include "macro.hpp"
#include "allocator.hpp"
#include "memory/buffer.hpp"
#include "platform/platform.hpp"



namespace skyland::macro
{



void terrain::Chunk::rotate()
{
    for (int z = 0; z < 8; ++z) {
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

    db_.reset();
}



void terrain::Chunk::shadowcast()
{
    for (int z = 0; z < 8; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
                blocks_[z][x][y].shadowed_ = true;
            }
        }
    }

    for (int x = 0; x < 8; ++x) {
        for (int y = 0; y < 8; ++y) {
            for (int z = 7; z > -1; --z) {
                if (blocks_[z][x][y].type_ > 0) {
                    blocks_[z][x][y].shadowed_ = false;
                    break;
                }
            }
        }
    }
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



void render(Platform& pfrm, terrain::Chunk& chunk)
{
    auto rendering_pass = [&](auto rendering_function) {
        auto draw_block = [&](int x, int y, int z) {
            auto slab = chunk.blocks_[z];

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



        for (int z = 0; z < 8; ++z) {

            draw_block(0, 0, z);

            draw_block(1, 0, z);
            draw_block(0, 1, z);

            draw_block(2, 0, z);
            draw_block(1, 1, z);
            draw_block(0, 2, z);

            draw_block(3, 0, z);
            draw_block(2, 1, z);
            draw_block(1, 2, z);
            draw_block(0, 3, z);

            draw_block(4, 0, z);
            draw_block(3, 1, z);
            draw_block(2, 2, z);
            draw_block(1, 3, z);
            draw_block(0, 4, z);

            draw_block(5, 0, z);
            draw_block(4, 1, z);
            draw_block(3, 2, z);
            draw_block(2, 3, z);
            draw_block(1, 4, z);
            draw_block(0, 5, z);

            draw_block(6, 0, z);
            draw_block(5, 1, z);
            draw_block(4, 2, z);
            draw_block(3, 3, z);
            draw_block(2, 4, z);
            draw_block(1, 5, z);
            draw_block(0, 6, z);

            draw_block(7, 0, z);
            draw_block(6, 1, z);
            draw_block(5, 2, z);
            draw_block(4, 3, z);
            draw_block(3, 4, z);
            draw_block(2, 5, z);
            draw_block(1, 6, z);
            draw_block(0, 7, z);

            draw_block(7, 1, z);
            draw_block(6, 2, z);
            draw_block(5, 3, z);
            draw_block(4, 4, z);
            draw_block(3, 5, z);
            draw_block(2, 6, z);
            draw_block(1, 7, z);

            draw_block(7, 2, z);
            draw_block(6, 3, z);
            draw_block(5, 4, z);
            draw_block(4, 5, z);
            draw_block(3, 6, z);
            draw_block(2, 7, z);

            draw_block(7, 3, z);
            draw_block(6, 4, z);
            draw_block(5, 5, z);
            draw_block(4, 6, z);
            draw_block(3, 7, z);

            draw_block(7, 4, z);
            draw_block(6, 5, z);
            draw_block(5, 6, z);
            draw_block(4, 7, z);

            draw_block(7, 5, z);
            draw_block(6, 6, z);
            draw_block(5, 7, z);

            draw_block(7, 6, z);
            draw_block(6, 7, z);

            draw_block(7, 7, z);
        }
    };


    chunk.db_.reset();

    if (not chunk.db_) {
        chunk.db_.emplace(pfrm);

        rendering_pass([&](const Vec3<u8>& p, int texture, int t_start) {
            auto n = chunk.db_->depth_node_allocator_
                         .alloc<terrain::Chunk::DepthNode>();
            if (n == nullptr) {
                Platform::fatal("depth node allocator out of memory!");
            }

            n->position_ = p;

            if (t_start < 480) {
                n->next_ = chunk.db_->depth_1_->visible_[t_start];
                // NOTE: it's bulk allocation, there's no leak here. The destructor
                // won't be called, but we're dealing with a primitive type.
                chunk.db_->depth_1_->visible_[t_start] = n.release();
            } else {
                n->next_ = chunk.db_->depth_2_->visible_[t_start - 480];
                chunk.db_->depth_2_->visible_[t_start - 480] = n.release();
            }
        });
    }


    for (int i = 0; i < 480; ++i) {
        // if (chunk.db_->depth_1_->visible_[i]) {
            pfrm.blit_t0_erase(i);
        // }
        // if (chunk.db_->depth_2_->visible_[i]) {
            pfrm.blit_t1_erase(i);
        // }
    }


    rendering_pass([&](const Vec3<u8>& p, int texture, int t_start) {
        static const int giveup_depth = 4;

        if (t_start < 480) {
            auto n = chunk.db_->depth_1_->visible_[t_start];
            int d = 0;
            while (n and d < giveup_depth) {
                auto pos = n->position_;
                if (p == pos) {
                    pfrm.blit_t0_tile_to_texture(texture, t_start, false);
                    break;
                }
                n = n->next_;
                ++d;
            }

        } else {
            auto n = chunk.db_->depth_2_->visible_[t_start - 480];
            int d = 0;
            while (n and d < giveup_depth) {
                auto pos = n->position_;
                if (p == pos) {
                    pfrm.blit_t1_tile_to_texture(texture, t_start - 480, false);
                    break;
                }
                n = n->next_;
                ++d;
            }
        }
    });
}



} // namespace skyland::macro
