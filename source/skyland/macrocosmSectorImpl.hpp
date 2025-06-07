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


#include "macrocosmRaster.hpp"
#include "macrocosmSector.hpp"



namespace skyland::macro::terrain
{



bool blocks_light(terrain::Type t);



template <typename Derived, s32 sx, s32 sy, s32 sz, s32 screen_y_offset>
class MacrocosmSectorImpl : public Sector
{
public:
    static_assert(sx < 32 and sy < 32 and sz < 32,
                  "Raster depthnode position_ holds data ranges of zero to "
                  "31. Therefore, sector size should not exceed 32 in any "
                  "dimension. raster::DepthNode::set_position() does not do "
                  "any bounds check for performance reasons.");

    static_assert(sx == sy,
                  "Sectors must be square. Rotation logic assumes squareness.");


    MacrocosmSectorImpl(Vec2<s8> position, Sector::Shape shape)
        : Sector(position, shape, {sx, sy, sz})
    {
    }


    const Block& get_block(const Vec3<u8>& coord) const override final
    {
        return blocks_[coord.z][coord.x][coord.y];
    }


    Block& ref_block(const Vec3<u8>& coord) override final
    {
        return blocks_[coord.z][coord.x][coord.y];
    }


    void rotate() override final
    {
        // NOTE: I decided to implement rotation by actually rotating the level's
        // blocks and fixing up the coordinates. Simplifies the renderer,
        // basically. Somewhat, costly, but an infrequent operation, unlike
        // rendering.

        for (int z = 0; z < sz; ++z) {
            for (int x = 0; x < sx / 2; x++) {
                for (int y = x; y < sy - x - 1; y++) {
                    auto temp = blocks_[z][x][y];
                    blocks_[z][x][y] = blocks_[z][y][sx - 1 - x];
                    blocks_[z][y][sx - 1 - x] =
                        blocks_[z][sx - 1 - x][sx - 1 - y];
                    blocks_[z][sx - 1 - x][sx - 1 - y] =
                        blocks_[z][sx - 1 - y][x];
                    blocks_[z][sx - 1 - y][x] = temp;
                }
            }
        }

        for (int z = 0; z < sz; ++z) {
            for (int x = 0; x < sx; ++x) {
                for (int y = 0; y < sy; ++y) {
                    auto& block = blocks_[z][x][y];
                    switch (block.type()) {
                    case terrain::Type::selector:
                        p_.cursor_ = {(u8)x, (u8)y, (u8)z};
                        break;

                    case terrain::Type::road_ns:
                        block.type_ = (u8)terrain::Type::road_we;
                        break;

                    case terrain::Type::road_we:
                        block.type_ = (u8)terrain::Type::road_ns;
                        break;

                    case terrain::Type::lava_spread_laterally_a:
                        block.type_ =
                            (u8)terrain::Type::lava_spread_laterally_b;
                        break;

                    case terrain::Type::lava_spread_laterally_b:
                        block.type_ =
                            (u8)terrain::Type::lava_spread_laterally_c;
                        break;

                    case terrain::Type::lava_spread_laterally_c:
                        block.type_ =
                            (u8)terrain::Type::lava_spread_laterally_d;
                        break;

                    case terrain::Type::lava_spread_laterally_d:
                        block.type_ =
                            (u8)terrain::Type::lava_spread_laterally_a;
                        break;

                    case terrain::Type::water_spread_laterally_a:
                        block.type_ =
                            (u8)terrain::Type::water_spread_laterally_b;
                        break;

                    case terrain::Type::water_spread_laterally_b:
                        block.type_ =
                            (u8)terrain::Type::water_spread_laterally_c;
                        break;

                    case terrain::Type::water_spread_laterally_c:
                        block.type_ =
                            (u8)terrain::Type::water_spread_laterally_d;
                        break;

                    case terrain::Type::water_spread_laterally_d:
                        block.type_ =
                            (u8)terrain::Type::water_spread_laterally_a;
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
        raster::globalstate::_recalc_depth_test.fill();
    }


    void erase() override
    {
        set_name("");

        for (auto& slab : blocks_) {
            for (auto& slice : slab) {
                for (auto& block : slice) {
                    block.type_ = (u8)Type::air;
                    block.shadowed_ = true;
                    block.shadowed_day_ = true;
                }
            }
        }

        raster::globalstate::_recalc_depth_test.fill();
    }


    void on_block_changed(const Vec3<u8>& coord) override
    {
        int t_start = Derived::screen_mapping_lut[coord.x][coord.y];
        t_start += 30 * screen_y_offset;
        t_start -= 30 * coord.z;

        auto safeset = [&](int index) {
            if (index >= 0) {
                raster::globalstate::_recalc_depth_test.set(index, true);
            }
        };

        safeset(t_start);
        safeset(t_start + 1);

        t_start += 30;

        safeset(t_start);
        safeset(t_start + 1);

        t_start += 30;

        safeset(t_start);
        safeset(t_start + 1);
    }


    void shadowcast() override final
    {
        auto& prev_cursor = ref_block(cursor());

        for (int z = 0; z < sz; ++z) {
            for (int x = 0; x < sx; ++x) {
                for (int y = 0; y < sy; ++y) {
                    blocks_[z][x][y].shadowed_day_ = false;
                    if (raster::globalstate::is_night) {
                        blocks_[z][x][y].shadowed_ = true;
                    } else {
                        blocks_[z][x][y].shadowed_ = false;
                    }
                }
            }
        }

        for (int x = 0; x < sx; ++x) {
            for (int y = 0; y < sy; ++y) {
                bool shadow = false;
                for (int z = sz - 1; z > -1; --z) {
                    auto t = blocks_[z][x][y].type();
                    if (shadow) {
                        blocks_[z][x][y].shadowed_day_ = true;
                        if (not raster::globalstate::is_night) {
                            blocks_[z][x][y].shadowed_ = true;
                        }
                    } else if (blocks_light(t)) {
                        shadow = true;
                    }
                }
            }
        }

        for (int z = 0; z < sz; ++z) {
            for (int x = 0; x < sx; ++x) {
                for (int y = 0; y < sy; ++y) {
                    auto& block = blocks_[z][x][y];
                    if (block.type() == Type::light_source) {
                        // beneath:
                        for (int zz = z - 1; zz > z - 4; --zz) {
                            if (zz > -1) {
                                auto& block = blocks_[zz][x][y];
                                blocks_[zz][x][y].shadowed_ = false;
                                blocks_[zz][x][y].shadowed_day_ = false;
                                if (blocks_light(block.type())) {
                                    break;
                                }
                            }
                        }

                        // raycast positive x:
                        for (int xx = x + 1; xx < x + 4; ++xx) {
                            if (xx < sx) {
                                auto& block = blocks_[z][xx][y];
                                if (blocks_light(block.type())) {
                                    break;
                                }
                                block.shadowed_ = false;
                                if (z > 0) {
                                    blocks_[z - 1][xx][y].shadowed_ = false;
                                    blocks_[z - 1][xx][y].shadowed_day_ = false;
                                }
                            }
                        }

                        // raycast negative x:
                        for (int xx = x - 1; xx > x - 4; --xx) {
                            if (xx > -1) {
                                auto& block = blocks_[z][xx][y];
                                if (blocks_light(block.type())) {
                                    break;
                                }
                                block.shadowed_ = false;
                                if (z > 0) {
                                    blocks_[z - 1][xx][y].shadowed_ = false;
                                    blocks_[z - 1][xx][y].shadowed_day_ = false;
                                }
                            }
                        }

                        // raycast positive y:
                        for (int yy = y + 1; yy < y + 4; ++yy) {
                            if (yy < sy) {
                                auto& block = blocks_[z][x][yy];
                                if (blocks_light(block.type())) {
                                    break;
                                }
                                block.shadowed_ = false;
                                if (z > 0) {
                                    blocks_[z - 1][x][yy].shadowed_ = false;
                                    blocks_[z - 1][x][yy].shadowed_day_ = false;
                                }
                            }
                        }

                        // raycast negative y:
                        for (int yy = y - 1; yy > y - 4; --yy) {
                            if (yy > -1) {
                                auto& block = blocks_[z][x][yy];
                                if (blocks_light(block.type())) {
                                    break;
                                }
                                block.shadowed_ = false;
                                if (z > 0) {
                                    blocks_[z - 1][x][yy].shadowed_ = false;
                                    blocks_[z - 1][x][yy].shadowed_day_ = false;
                                }
                            }
                        }

                        // above:
                        for (int zz = z + 1; zz < z + 4; ++zz) {
                            if (zz < sz) {
                                auto& block = blocks_[zz][x][y];
                                if (blocks_light(block.type())) {
                                    break;
                                }
                                blocks_[zz][x][y].shadowed_ = false;
                                blocks_[zz][x][y].shadowed_day_ = false;
                            }
                        }
                    }
                }
            }
        }

        ref_block(cursor()) = prev_cursor;
    }



    void render_setup() override final
    {
        using namespace raster;

        using fast_bool = int;

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

        // Deals with a number of edge cases, primarily where the layout of the
        // level changes without the cursor moving, e.g. fluids. We keep a cache
        // of the depth buffer entries associated with the cursor, so we need to
        // make sure we do depth testing for raster tiles associated with the
        // cursor block at all times. An inconvenience resulting from the many
        // optimizations that I do on the cursor block to make the game feel
        // smooth.
        on_block_changed(p_.cursor_);

        if (z_view_ > size().z) {
            set_z_view(size().z);
        }

        [[maybe_unused]] auto start = PLATFORM.delta_clock().sample();

        auto prev_cursor_raster_tiles = globalstate::_cursor_raster_tiles;
        globalstate::_cursor_raster_tiles.clear();

        const fast_bool cursor_moved = globalstate::_cursor_moved;
        const fast_bool grew = globalstate::_grew;

#ifndef __CMD_MACRO_RAST__
        // NOTE: this block segfaults when creating a sector from qrcode
        // data. FIXME!!!
        {
            // NOTE: code from below copy-pasted here, while working on
            // performance optimization. Need to map cursor tile indices, better
            // done out-of-line here rather than in the main geometry loop.

            auto c = cursor();
            auto slab = blocks_[c.z];

            auto& block = slab[c.x][c.y];

            if (block.type() not_eq Type::selector) {
                goto SKIP_CURSOR;
            }

            int t_start = Derived::screen_mapping_lut[c.x][c.y];
            static constexpr const auto shift = 30 * screen_y_offset;
            t_start += shift;
            t_start -= c.z * 30;

            auto blit = [&](int t_start) {
                if (t_start < 0 or
                    not raster::globalstate::_recalc_depth_test.get(t_start)) {
                    return;
                }

                globalstate::_cursor_raster_tiles.push_back(t_start);
            };

            blit(t_start);
            blit(t_start + 1);

            t_start += 30;

            blit(t_start);
            blit(t_start + 1);

            t_start += 30;

            blit(t_start);
            blit(t_start + 1);
        }
#endif // __CMD_MACRO_RAST__
    SKIP_CURSOR:

        auto rendering_pass = [&](auto rendering_function) {
            auto project_block = [&](u8 x, u8 y, u8 z) {
                auto slab = blocks_[z];

                auto& block = slab[x][y];

                if (not(block.type_ > 0)) {
                    return;
                }

                int t_start = Derived::screen_mapping_lut[x][y];
                static constexpr const auto shift = 30 * screen_y_offset;
                t_start += shift;
                t_start -= z * 30;

                int texture = (block.type_ - 1) * 12;
                if (block.shadowed_) {
                    texture += 6;
                }

                auto blit = [&](int texture, int t_start) {
                    if (t_start < 0 or
                        not raster::globalstate::_recalc_depth_test.get(
                            t_start)) {
                        return;
                    }
                    rendering_function(
                        Vec3<u8>{(u8)x, (u8)y, (u8)z}, texture, t_start);

                    if (t_start < RASTER_CELLCOUNT) {
                        (*_db)->depth_1_needs_repaint.set(t_start, true);
                    } else {
                        (*_db)->depth_2_needs_repaint.set(
                            t_start - RASTER_CELLCOUNT, true);
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

            int z_limit = z_view_ - 1;

            for (int z = 0; z < z_limit; ++z) {

                for (auto& p : Derived::winding_path) {
                    project_block(p.x, p.y, z);
                }
            }

            // NOTE: top layer is empty, only exists for cursor block.
            if (cursor().z == sz - 1) {
                project_block(cursor().x, cursor().y, cursor().z);
            }
        };


        if (not _db) {
            _db.emplace(allocate_dynamic<raster::DepthBuffer>("depth-buffer"));
        }

        [[maybe_unused]] auto t2 = PLATFORM.delta_clock().sample();

        auto& node_allocator = (*_db)->depth_node_allocator_;

        rendering_pass([&](const Vec3<u8>& p, int texture, int t_start) {
            auto n = node_allocator.alloc();

            n->set_position(p);
            n->tile_ = texture;

            if (t_start < RASTER_CELLCOUNT) {
                n->next_ = (*_db)->depth_1_->visible_[t_start];
                (*_db)->depth_1_->visible_[t_start] = n;
            } else {
                n->next_ =
                    (*_db)->depth_2_->visible_[t_start - RASTER_CELLCOUNT];
                (*_db)->depth_2_->visible_[t_start - RASTER_CELLCOUNT] = n;
            }
        });


        [[maybe_unused]] auto t3 = PLATFORM.delta_clock().sample();

        if (cursor_moved) {
            for (auto& t : prev_cursor_raster_tiles) {
                if (t < RASTER_CELLCOUNT) {
                    (*_db)->depth_1_cursor_redraw.set(t, true);
                } else {
                    (*_db)->depth_2_cursor_redraw.set(t - RASTER_CELLCOUNT,
                                                      true);
                }
            }
            for (auto& t : globalstate::_cursor_raster_tiles) {
                if (t < RASTER_CELLCOUNT) {
                    (*_db)->depth_1_cursor_redraw.set(t, true);
                } else {
                    (*_db)->depth_2_cursor_redraw.set(t - RASTER_CELLCOUNT,
                                                      true);
                }
            }
        }

        [[maybe_unused]] auto t4 = PLATFORM.delta_clock().sample();

        for (int i = 0; i < RASTER_CELLCOUNT; ++i) {

            auto insert_edges = [](auto tail) {
                const u16 edge_l = 496 - 480;
                const u16 edge_r = 497 - 480;

                if (tail->position().z == 0 and tail->tile_ not_eq edge_l and
                    tail->tile_ not_eq edge_r) {
                    auto cat = tile_category(tail->tile_);
                    if ((cat == bot_angled_l) or (cat == bot_angled_r)) {
                        auto n = (*_db)->depth_node_allocator_.alloc();
                        n->set_position(tail->position());
                        n->next_ = nullptr;

                        if (cat == bot_angled_l) {
                            n->tile_ = edge_l;
                        } else if (cat == bot_angled_r) {
                            n->tile_ = edge_r;
                        }

                        tail->next_ = n;
                    }
                }
            };


            if (auto head = (*_db)->depth_1_->visible_[i]) {
                fast_bool skip_repaint = true;
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
                    (*_db)->depth_1_->visible_[i] = nullptr;
                    continue;
                }

                auto last = head;
                fast_bool occluded = false;

                fast_bool seen_top_angled_l = false;
                fast_bool seen_top_angled_r = false;
                fast_bool seen_bot_angled_l = false;
                fast_bool seen_bot_angled_r = false;

                while (head) {
                    auto cg = tile_category(head->tile_);
                    if (cg == opaque) {
                        // Cull non-visible tiles.
                        head->next_ = nullptr;
                        occluded = true;
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
                            seen_top_angled_l = true;

                            if (seen_bot_angled_r) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR1;
                            }
                            break;

                        case top_angled_r:
                            seen_top_angled_r = true;

                            if (seen_bot_angled_l) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR1;
                            }
                            break;

                        case bot_angled_l:
                            seen_bot_angled_l = true;
                            if (seen_top_angled_r) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR1;
                            }
                            break;

                        case bot_angled_r:
                            seen_bot_angled_r = true;
                            if (seen_top_angled_l) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR1;
                            }
                            break;
                        }
                    }

                    last = head;
                    head = head->next_;
                }

                if (last and not occluded) {
                    insert_edges(last);
                }

            BR1:;
            }

            if (auto head = (*_db)->depth_2_->visible_[i]) {
                fast_bool skip_repaint = true;
                if ((*_db)->depth_2_cursor_redraw.get(i)) {
                    skip_repaint = false;
                }
                if (grew or not cursor_moved) {
                    if ((*_db)->depth_2_needs_repaint.get(i)) {
                        skip_repaint = false;
                    }
                }
                if (skip_repaint) {
                    (*_db)->depth_2_->visible_[i] = nullptr;
                    continue;
                }

                auto last = head;
                fast_bool occluded = false;

                fast_bool seen_top_angled_l = false;
                fast_bool seen_top_angled_r = false;
                fast_bool seen_bot_angled_l = false;
                fast_bool seen_bot_angled_r = false;

                while (head) {
                    auto cg = tile_category(head->tile_);
                    if (cg == opaque) {
                        // Cull non-visible tiles.
                        occluded = true;
                        head->next_ = nullptr;
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
                            seen_top_angled_l = true;

                            if (seen_bot_angled_r) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR2;
                            }
                            break;

                        case top_angled_r:
                            seen_top_angled_r = true;

                            if (seen_bot_angled_l) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR2;
                            }
                            break;

                        case bot_angled_l:
                            seen_bot_angled_l = true;
                            if (seen_top_angled_r) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR2;
                            }
                            break;

                        case bot_angled_r:
                            seen_bot_angled_r = true;
                            if (seen_top_angled_l) {
                                occluded = true;
                                head->next_ = nullptr;
                                goto BR2;
                            }
                            break;
                        }
                    }

                    last = head;
                    head = head->next_;
                }
                if (last and not occluded) {
                    insert_edges(last);
                }
            BR2:;
            }
        }

        [[maybe_unused]] auto t5 = PLATFORM.delta_clock().sample();

        for (u32 i = 0; i < globalstate::_cursor_raster_tiles.size(); ++i) {
            auto t = globalstate::_cursor_raster_tiles[i];

            globalstate::_cursor_raster_stack[i].clear();

            DepthNode* head = nullptr;
            if (t >= RASTER_CELLCOUNT) {
                head = (*_db)->depth_2_->visible_[t - RASTER_CELLCOUNT];
            } else {
                head = (*_db)->depth_1_->visible_[t];
            }

            while (head) {
                globalstate::_cursor_raster_stack[i].push_back(head->tile_);
                head = head->next_;
            }
        }

        [[maybe_unused]] auto stop = PLATFORM.delta_clock().sample();
        // PLATFORM.fatal(format("%, %, %, % (%)",
        //                   t2 - start,
        //                   t3 - t2,
        //                   t4 - t3,
        //                   t5 - t4,
        //                   stop - start)
        //                .c_str());
    }


    u16 project_block(int x, int y, int z) const override
    {
        int t_start = Derived::screen_mapping_lut[x][y];
        static constexpr const auto shift = 30 * screen_y_offset;
        t_start += shift;
        t_start -= z * 30;

        return t_start;
    }


protected:
    Block blocks_[sz][sx][sy];

    struct OcclusionTable
    {
        bool covered_[sz][sx][sy];
        bool bottom_edge_visible_[sz][sx][sy];
    };

    Optional<DynamicMemory<OcclusionTable>> occlusion_;

    void setup_occlusion()
    {
        occlusion_ = allocate_dynamic<OcclusionTable>("occ-table");

        auto partially_transparent = [](Type t) {
            return t == Type::air or t == Type::lumber or t == Type::selector;
        };


        for (u8 z = 0; z < size().z; ++z) {
            for (u8 y = 0; y < size().y; ++y) {
                for (u8 x = 0; x < size().x; ++x) {
                    (*occlusion_)->bottom_edge_visible_[z][x][y] = true;
                    (*occlusion_)->covered_[z][x][y] = true;
                }
            }
        }

        for (u8 z = 0; z < size().z - 1; ++z) {
            for (u8 y = 0; y < size().y - 1; ++y) {
                for (u8 x = 0; x < size().x - 1; ++x) {
                    auto above = blocks_[z - 1][x][y].type();
                    auto l = blocks_[z][x + 1][y].type();
                    auto r = blocks_[z][x][y + 1].type();

                    if (not partially_transparent(l) and
                        not partially_transparent(r)) {
                        (*occlusion_)->bottom_edge_visible_[z][x][y] = false;
                    } else {
                        (*occlusion_)->bottom_edge_visible_[z][x][y] = true;
                    }

                    if (not partially_transparent(above) and
                        not partially_transparent(l) and
                        not partially_transparent(r)) {

                        (*occlusion_)->covered_[z][x][y] = true;
                    } else {
                        (*occlusion_)->covered_[z][x][y] = false;
                    }
                }
            }
        }
    }
};



template <typename Derived, s32 sx, s32 sy, s32 sz, s32 screen_y_offset>
class MacrocosmSectorImplFull
    : public MacrocosmSectorImpl<Derived, sx, sy, sz, screen_y_offset>
{
public:
    MacrocosmSectorImplFull(Vec2<s8> position, Sector::Shape shape)
        : MacrocosmSectorImpl<Derived, sx, sy, sz, screen_y_offset>(position,
                                                                    shape)
    {
    }

private:
};



} // namespace skyland::macro::terrain
