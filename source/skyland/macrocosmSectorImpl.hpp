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


#include "macrocosmRaster.hpp"
#include "macrocosmSector.hpp"



namespace skyland::macro::terrain
{



bool blocks_light(terrain::Type t);



template <typename Derived, s32 sx, s32 sy, s32 sz, s32 screen_y_offset>
class MacrocosmSectorImpl : public Sector
{
public:
    // Sectors must be square.
    static_assert(sx == sy);


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

        auto rotate_coord = [](Vec3<u8> input) -> Vec3<u8> {
            return {(u8)((sx - 1) - input.y), input.x, input.z};
        };

        if (auto e = exports()) {
            for (auto& exp : *e) {
                exp.source_coord_ = rotate_coord(exp.source_coord_);
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

        if (auto e = exports()) {
            e->clear();
        }

        raster::globalstate::_recalc_depth_test.fill();

        base_stats_cache_clear();
    }


    void on_block_changed(const Vec3<u8>& coord) override
    {
        int t_start = Derived::screen_mapping_lut[coord.x][coord.y];
        t_start += 30 * screen_y_offset;
        t_start -= 30 * coord.z;
        raster::globalstate::_recalc_depth_test.set(t_start, true);
        raster::globalstate::_recalc_depth_test.set(t_start + 1, true);

        t_start += 30;

        raster::globalstate::_recalc_depth_test.set(t_start, true);
        raster::globalstate::_recalc_depth_test.set(t_start + 1, true);

        t_start += 30;

        raster::globalstate::_recalc_depth_test.set(t_start, true);
        raster::globalstate::_recalc_depth_test.set(t_start + 1, true);
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



    void render_setup(Platform& pfrm) override final
    {
        using namespace raster;

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

                int t_start = Derived::screen_mapping_lut[x][y];
                t_start += 30 * screen_y_offset;
                t_start -= 30 * z;

                int texture = (block.type_ - 1) * 12 + 480;
                if (block.shadowed_) {
                    texture += 6;
                }

                auto blit = [&](int texture, int t_start) {
                    if (not raster::globalstate::_recalc_depth_test.get(
                            t_start)) {
                        return;
                    }
                    rendering_function(
                        Vec3<u8>{(u8)x, (u8)y, (u8)z}, texture, t_start);
                    if (block.type() == Type::selector) {
                        globalstate::_cursor_raster_tiles.push_back(t_start);
                    }


                    if (t_start < 480) {
                        (*_db)->depth_1_needs_repaint.set(t_start, true);
                    } else {
                        (*_db)->depth_2_needs_repaint.set(t_start - 480, true);
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

                for (auto& p : Derived::winding_path) {
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
                    (*_db)->depth_1_->visible_[i] = nullptr;
                    continue;
                }

                Buffer<TileCategory, 8> seen;
                while (head) {
                    auto cg = tile_category(head->tile_);
                    if (cg == opaque) {
                        // Cull non-visible tiles.
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
                            for (auto& s : seen) {
                                if (s == bot_angled_r) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;

                        case top_angled_r:
                            for (auto& s : seen) {
                                if (s == bot_angled_l) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;

                        case bot_angled_l:
                            for (auto& s : seen) {
                                if (s == top_angled_r) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;

                        case bot_angled_r:
                            for (auto& s : seen) {
                                if (s == top_angled_l) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;
                        }
                        seen.push_back(cg);
                    }

                    head = head->next_;
                }
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
                    (*_db)->depth_2_->visible_[i] = nullptr;
                    continue;
                }
                Buffer<TileCategory, 8> seen;
                while (head) {
                    auto cg = tile_category(head->tile_);
                    if (cg == opaque) {
                        // Cull non-visible tiles.
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
                            for (auto& s : seen) {
                                if (s == bot_angled_r) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;

                        case top_angled_r:
                            for (auto& s : seen) {
                                if (s == bot_angled_l) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;

                        case bot_angled_l:
                            for (auto& s : seen) {
                                if (s == top_angled_r) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;

                        case bot_angled_r:
                            for (auto& s : seen) {
                                if (s == top_angled_l) {
                                    head->next_ = nullptr;
                                    break;
                                }
                            }
                            break;
                        }
                        seen.push_back(cg);
                    }

                    head = head->next_;
                }
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
                        auto n =
                            (*_db)->depth_node_allocator_.alloc<DepthNode>();
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



protected:
    Block blocks_[sz][sx][sy];
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


    Sector::Exports* exports() override
    {
        return &exports_;
    }


    void set_export(const Sector::ExportInfo& e) override
    {
        remove_export(e.source_coord_);

        auto& block = this->get_block(e.source_coord_);
        if (block.type() not_eq Type::port) {
            return;
        }

        if (not exports_.full()) {
            exports_.emplace_back();
            memcpy(&exports_.back(), &e, sizeof e);
        }
    }


    void remove_export(Vec3<u8> source_coord) override
    {
        for (auto it = exports_.begin(); it not_eq exports_.end();) {
            if (it->source_coord_ == source_coord) {
                it = exports_.erase(it);
                return;
            } else {
                ++it;
            }
        }
    }


    void base_stats_cache_clear() const override
    {
        base_stats_cache_.reset();
    }


    void base_stats_cache_store(const Stats& s) const override
    {
        base_stats_cache_ = s;
    }


    Stats* base_stats_cache_load() const override
    {
        if (base_stats_cache_) {
            return &*base_stats_cache_;
        } else {
            return nullptr;
        }
    }


private:
    Sector::Exports exports_;

    // Recalculating stats for everything when we have multiple levels slows
    // down the game significantly, so we cache previous results. I mean, a
    // sector has ~512 blocks, and if you have 20 sectors, that's a lot of
    // number crunching and will definitely lag the game if done frequently.
    mutable std::optional<Stats> base_stats_cache_;
};



} // namespace skyland::macro::terrain
