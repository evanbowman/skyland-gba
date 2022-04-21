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



Coins State::coin_yield()
{
    auto coins = data_->origin_sector_.coin_yield();

    for (auto& sector : data_->other_sectors_) {
        coins += sector->coin_yield();
    }

    return coins;
}



Coins terrain::Sector::coin_yield() const
{
    Coins result = 0;

    auto st = stats();

    int productive_population = population_;
    int unproductive_population = 0;

    if (st.housing_ < population_) {
        // Homeless people are less economically productive? Sounds cynical, but
        // probably true.
        productive_population = st.housing_;
        unproductive_population = population_ - st.housing_;
    }

    int employed_population = productive_population;
    int unemployed_population = 0;
    if (st.employment_ < employed_population) {
        unemployed_population = productive_population - st.employment_;
        employed_population = productive_population - unemployed_population;
    }

    Float productivity =
        employed_population * 0.3f +
        unemployed_population * 0.1f;

    // Un-housed people put a drain on state resources. Build them some housing!
    productivity -= unproductive_population * 0.1f;

    if (productivity < 1) {
        productivity = 1;
    }

    result += productivity;

    for (auto& c : st.commodities_) {
        auto value = c.value(c.type_);
        for (int i = 0; i < c.supply_; ++i) {
            result += value;
            value *= 0.75f; // Diminishing returns
        }
    }

    return result;
}



void State::advance(int elapsed_years)
{
    sector().advance(elapsed_years);
    data_->year_ += elapsed_years;

    data_->coins_ += coin_yield() * elapsed_years;
}



terrain::Sector::Sector(Vec2<s8> position)
{
    x_ = position.x;
    y_ = position.y;
}



void terrain::Sector::advance(int years)
{
    population_ += population_growth_rate() * years;
}



terrain::Stats terrain::stats(Type t)
{
    terrain::Stats result;


    switch (t) {
    case terrain::Type::building:
        result.housing_ += 40;
        break;

    case terrain::Type::terrain:
        result.food_ += 1;
        break;

    case terrain::Type::wheat:
        result.food_ += 5;
        result.employment_ += 2;
        break;

    case terrain::Type::indigo:
        result.commodities_.push_back({Commodity::Type::indigo, 1});
        result.employment_ += 4;
        break;

    case terrain::Type::madder:
        result.commodities_.push_back({Commodity::Type::rose_madder, 1});
        result.employment_ += 4;
        break;

    default:
        break;
    }

    return result;
}



Coins terrain::Commodity::value(Commodity::Type t)
{
    // TODO...
    return 5;
}



terrain::Stats terrain::Block::stats() const
{
    return terrain::stats(type());
}



terrain::Improvements terrain::Block::improvements() const
{
    return terrain::improvements(type());
}



terrain::Stats terrain::Sector::stats() const
{
    terrain::Stats result;

    for (int z = 0; z < z_limit - 1; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
                auto block_stats = blocks_[z][x][y].stats();

                // NOTE: if a block is covered, then it's not possible to
                // harvest the supplied food, so a stacked block should yield
                // zero food.
                if (blocks_[z + 1][x][y].type() == Type::air or
                    blocks_[z + 1][x][y].type() == Type::selector) {
                    result.food_ += block_stats.food_;
                }

                result.employment_ = block_stats.employment_;

                result.housing_ += block_stats.housing_;

                for (auto& c : block_stats.commodities_) {
                    bool existing = false;
                    for (auto& e : result.commodities_) {
                        if (e.type_ == c.type_) {
                            e.supply_ += c.supply_;
                            existing = true;
                            break;
                        }
                    }

                    if (not existing) {
                        result.commodities_.push_back(c);
                    }
                }
            }
        }
    }

    return result;
}



Float terrain::Sector::population_growth_rate() const
{
    auto s = stats();

    auto required_food = population_ / food_consumption_factor;

    Float result = 0.f;


    if (s.food_ >= required_food) {
        result = 0.1f * (s.food_ - required_food);
    } else {
        result = -0.5f * (required_food - s.food_);
    }

    if (population_ > s.housing_) {
        result -= 0.025f * (population_ - s.housing_);
    } else {
        result += 0.025f * (s.housing_ - population_);
    }


    return result;
}



terrain::Category terrain::category(Type t)
{
    switch (t) {
    default:
        return Category::basic;

    case terrain::Type::wheat:
    case terrain::Type::madder:
    case terrain::Type::indigo:
        return Category::crop;

    case terrain::Type::water:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return Category::fluid;
    }
}



Coins terrain::cost(Sector& s, Type t)
{
    switch (t) {
    case terrain::Type::__invalid:
        break;

    case terrain::Type::air:
        return 0;

    case terrain::Type::building:
        return 320;

    case terrain::Type::terrain:
        return 100;

    case terrain::Type::masonry:
        return 30;

    case terrain::Type::count:
    case terrain::Type::selector:
        return 0;

    case terrain::Type::water:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return 30;

    case terrain::Type::wheat:
        return 40;

    case terrain::Type::indigo:
        return 100;

    case terrain::Type::madder:
        return 100;

    case terrain::Type::gold:
        return 1000;

    case terrain::Type::workshop:
        return 100;
    }

    return 0;
}



SystemString terrain::name(Type t)
{
    switch (t) {
    case terrain::Type::__invalid:
        break;

    case terrain::Type::air:
        return SystemString::block_air;

    case terrain::Type::building:
        return SystemString::block_building;

    case terrain::Type::terrain:
        return SystemString::block_terrain;

    case terrain::Type::masonry:
        return SystemString::block_masonry;

    case terrain::Type::count:
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

    case terrain::Type::indigo:
        return SystemString::block_indigo;

    case terrain::Type::madder:
        return SystemString::block_madder;

    case terrain::Type::gold:
        return SystemString::block_gold;

    case terrain::Type::workshop:
        return SystemString::block_workshop;
    }

    return SystemString::gs_error;
}



SystemString terrain::Block::name() const
{
    return terrain::name(type());
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
                if (block.type() == terrain::Type::selector) {
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
    Buffer<terrain::Type, 10> result;

    switch (t) {
    case Type::terrain: {
        result.push_back(Type::wheat);
        result.push_back(Type::indigo);
        result.push_back(Type::madder);
        break;
    }

    default:
        break;
    }

    return result;
}



std::pair<int, int> terrain::icons(Type t)
{
    switch (t) {
    case terrain::Type::air:
        return {2488, 2504};

    case terrain::Type::building:
        return {1448, 1464};

    case terrain::Type::terrain:
        return {2632, 2648};

    case terrain::Type::masonry:
        return {1448, 1464};

    case terrain::Type::count:
    case terrain::Type::__invalid:
    case terrain::Type::selector:
        return {};

    case terrain::Type::water:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return {2120, 2136};

    case terrain::Type::wheat:
        return {1448, 1464};

    case terrain::Type::indigo:
        return {2696, 2712};

    case terrain::Type::madder:
        return {2664, 2680};

    case terrain::Type::gold:
        return {2440, 2456};

    case terrain::Type::workshop:
        return {776, 760};
    }

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
                auto t = blocks_[z][x][y].type();
                if (shadow) {
                    blocks_[z][x][y].shadowed_ = true;
                } else if (blocks_light(t)) {
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
    if (selected.type() == type) {
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



void terrain::Sector::set_cursor(const Vec3<u8>& pos, bool lock_to_floor)
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

    if (lock_to_floor) {
        while (blocks_[cursor_.z][cursor_.x][cursor_.y].type() not_eq
               terrain::Type::air) {
            ++cursor_.z;
        }

        cursor_moved_ = true;

        while (cursor_.z > 0 and
               blocks_[cursor_.z - 1][cursor_.x][cursor_.y].type() ==
                   terrain::Type::air) {
            --cursor_.z;
        }
    }

    set_block(cursor_, terrain::Type::selector);
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
        // if (position.z == 0) {
        //     return;
        // }

        // auto lp = position;
        // lp.x++;

        // if (lp.x < 8) {
        //     auto& block = s.get_block(lp);
        //     if (block.type() == terrain::Type::air) {
        //         s.set_block(lp, terrain::Type::water_slant_a);
        //     }
        // }

        // if (position.y < 8) {
        //     auto rp = position;
        //     ++rp.y;

        //     auto& block = s.get_block(rp);
        //     if (block.type() == terrain::Type::air) {
        //         s.set_block(rp, terrain::Type::water_slant_b);
        //     }
        // }

    },
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
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
        // if (block.shadowed_) {
        //     block.type_ = (u8)terrain::Type::rock_stacked;
        //     block.repaint_ = true;
        //     s.changed_ = true;
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

    },
};
// clang-format on



void terrain::Sector::update()
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
    static const std::array<TileCategory, 400> category =
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
         empty, top_angled_r, empty, opaque, bot_angled_l, bot_angled_r,
         empty, top_angled_r, empty, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, empty, opaque, empty, bot_angled_l, bot_angled_r,
         top_angled_l, empty, opaque, empty, bot_angled_l, bot_angled_r,
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
                if (block.type() == Type::selector) {
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

        for (int z = 0; z < z_view_; ++z) {

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

            const u8 edge_l = 496 - 480;
            const u8 edge_r = 497 - 480;

            auto cat = tile_category(head->tile_);
            if (head->position_.z == 0 and head->tile_ not_eq edge_l and
                head->tile_ not_eq edge_r) {
                if ((cat == bot_angled_l and not has_tr) or
                    (cat == bot_angled_r and not has_tl)) {
                    auto n =
                        db_->depth_node_allocator_.alloc<raster::DepthNode>();
                    n->position_ = head->position_;
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

        if (auto head = db_->depth_1_->visible_[i]) {
            insert_edges(head);
        }

        if (auto head = db_->depth_2_->visible_[i]) {
            insert_edges(head);
        }
    }

    // #define RASTER_DEBUG_ENABLE

#ifdef RASTER_DEBUG_ENABLE
#define RASTER_DEBUG() do {pfrm.sleep(30);} while (false)
#else
#define RASTER_DEBUG() do {} while (false)
#endif

    // Actually perform the rendering. At this point, ideally, everything that's
    // not actually visible in the output should have been removed from the
    // depth buffer.
    pfrm.system_call("vsync", nullptr);
    for (int i = 0; i < 480; ++i) {

        if (auto head = db_->depth_1_->visible_[i]) {

            Buffer<int, 6> stack;
            while (head) {
                stack.push_back(head->tile_);
                if (head->tile_) {
                }
                head = head->next_;
            }

            // The first tile can be drawn much faster, as we don't care what's
            // currently onscreen.
            bool overwrite = true;

            while (not stack.empty()) {
                int tile = stack.back();
                RASTER_DEBUG();
                pfrm.blit_t0_tile_to_texture(tile + 480, i, overwrite);
                stack.pop_back();
                overwrite = false;
            }
        } else if (shrunk_ and not depth_1_skip_clear.get(i)) {
            pfrm.blit_t0_erase(i);
        }

        if (auto head = db_->depth_2_->visible_[i]) {

            Buffer<int, 6> stack;
            while (head) {
                stack.push_back(head->tile_);
                head = head->next_;
            }

            bool overwrite = true;

            while (not stack.empty()) {
                int tile = stack.back();
                RASTER_DEBUG();
                pfrm.blit_t1_tile_to_texture(tile + 480, i, overwrite);
                stack.pop_back();
                overwrite = false;
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



} // namespace skyland::macro
