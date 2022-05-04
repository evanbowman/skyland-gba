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
#include "macrocosmPancakeSector.hpp"
#include "memory/buffer.hpp"
#include "platform/platform.hpp"
#include "platform/ram_filesystem.hpp"



// NOTE: I put most of the code in this one file, because I originally intended
// this code to be multiboot-compatible. But, at this point, it's grown too
// large.



namespace skyland::macro
{



namespace raster
{



std::optional<DynamicMemory<raster::DepthBuffer>> _db;



namespace globalstate
{

bool _recast_shadows = false;
bool _changed = false;
bool _shrunk = false;
bool _grew = false;
bool _cursor_moved = false;
bool _changed_cursor_flicker_only = false;
Buffer<u16, 6> _cursor_raster_tiles;
Buffer<u16, 6> _cursor_raster_stack[6];

} // namespace globalstate
} // namespace raster



State* _bound_state;


State::State(Platform& pfrm) : data_(allocate_dynamic<Data>("macrocosm-data"))
{
    _bound_state = this;
}



void State::newgame(Platform& pfrm)
{
    data_->current_sector_ = -1;

    data_->erase_other_sectors();
    data_->other_sector_mem_.reset();
    data_->p().year_.set(1);

    auto& sector = this->sector();
    sector.erase();
    sector.set_name("origin");

    sector.set_block({3, 3, 0}, macro::terrain::Type::terrain);
    sector.set_block({3, 2, 0}, macro::terrain::Type::terrain);
    sector.set_block({2, 3, 0}, macro::terrain::Type::terrain);
    sector.set_block({3, 4, 0}, macro::terrain::Type::terrain);
    sector.set_block({4, 3, 0}, macro::terrain::Type::terrain);
    sector.set_block({4, 4, 0}, macro::terrain::Type::terrain);
    sector.set_block({2, 2, 0}, macro::terrain::Type::masonry);
    sector.set_block({4, 2, 0}, macro::terrain::Type::masonry);
    sector.set_block({3, 3, 1}, macro::terrain::Type::building);

    sector.set_cursor({3, 3, 1});
    sector.set_population(8);

    data_->p().coins_.set(160);
}



namespace fiscal
{



void Ledger::add_entry(LineItem::Label label, Float contribution)
{
    auto next = alloc_.alloc<LineItem>();
    if (next) {
        next->next_ = entries_;
        next->label_ = label;
        next->contribution_ = contribution;

        entries_ = next.release();
    }
}



const LineItem* Ledger::entries() const
{
    return entries_;
}



} // namespace fiscal



Coins State::coin_yield()
{
    auto coins = data_->origin_sector_.coin_yield();

    for (auto& sector : data_->other_sectors_) {
        coins += sector->coin_yield();
    }

    return coins;
}



std::pair<Coins, terrain::Sector::Population> State::colony_cost() const
{
    if (data_->other_sectors_.full()) {
        return {999999999, 9999};
    } else if (data_->other_sectors_.size() < 8) {
        return {1500 + 3000 * data_->other_sectors_.size(), 300};
    } else {
        return {6000 + 3000 * data_->other_sectors_.size(), 300};
    }
}



void State::advance(int elapsed_years)
{
    data_->origin_sector_.advance(elapsed_years);

    for (auto& s : data_->other_sectors_) {
        s->advance(elapsed_years);
    }

    data_->p().year_.set(data_->p().year_.get() + elapsed_years);

    auto add_coins = coin_yield() * elapsed_years;
    data_->p().coins_.set(data_->p().coins_.get() + add_coins);
}



namespace save
{
static const char* path = "/save/macro.dat";

static const char version = 'a';


struct Header
{
    State::Data::Persistent p_;
    u8 num_sectors_;
};


// Note about wrapper: I enjoy playing skyland macro mode myself, and sometimes
// I want to add persistent fields without losing my own save data! So I use
// these templates to add extra bytes to my save file when saving (but not
// loading), allowing me to inflate sections of the save file, and fill in the
// space later. Maybe you're wondering, why not just create something more
// flexible, with tagged attributes appended to the end of the save data? I
// could, but we're really limited in terms of available save memory on the gba.
template <u32 extra_bytes> struct PersistentWrapper
{
    terrain::Sector::Persistent p_;
    u8 extra_bytes_[extra_bytes];
};
template <> struct PersistentWrapper<0>
{
    terrain::Sector::Persistent p_;
};



template <u32 inflate> struct Sector
{
    PersistentWrapper<inflate> p_;
    union
    {
        u8 cube_[9][8][8];
        u8 pancake_[4][12][12];
    } blocks_;


    Sector()
    {
    }

    Sector(const macro::terrain::Sector& source)
    {
        memcpy(&p_.p_, &source.persistent(), sizeof p_);

        switch (source.persistent().shape_) {
        case terrain::Sector::Shape::cube:
            for (u8 z = 0; z < macro::terrain::Sector::z_limit; ++z) {
                for (u8 x = 0; x < 8; ++x) {
                    for (u8 y = 0; y < 8; ++y) {
                        blocks_.cube_[z][x][y] =
                            source.get_block({x, y, z}).type_;
                    }
                }
            }
            break;

        case terrain::Sector::Shape::pancake:
            for (u8 z = 0; z < 4; ++z) {
                for (u8 x = 0; x < 12; ++x) {
                    for (u8 y = 0; y < 12; ++y) {
                        blocks_.pancake_[z][x][y] =
                            source.get_block({x, y, z}).type_;
                    }
                }
            }
            break;
        }
    }
};

} // namespace save



// Save Format:
// Header
// Sector_0_data
// Sector_0_export_count (one byte)
// Sector_0_exports[...]
// Sector_1_data
// Sector_1_export_count
// Sector_1_exports
// ...
// Sector_N_data
// you get the idea, right?



void State::save(Platform& pfrm)
{
    Vector<char> save_data;

    save::Header header;
    memcpy(&header.p_, &data_->persistent_, sizeof data_->persistent_);
    header.num_sectors_ = 1 + data_->other_sectors_.size();

    for (u32 i = 0; i < sizeof header; ++i) {
        save_data.push_back(((u8*)&header)[i]);
    }

    auto store_sector = [&save_data](const macro::terrain::Sector& sector) {
        save::Sector<0> out(sector);
        for (u32 i = 0; i < sizeof out; ++i) {
            save_data.push_back(((u8*)&out)[i]);
        }

        save_data.push_back((u8)sector.exports().size());

        for (auto& exp : sector.exports()) {
            for (u32 j = 0; j < sizeof exp; ++j) {
                save_data.push_back(((u8*)&exp)[j]);
            }
        }
    };

    store_sector(data_->origin_sector_);

    for (auto& s : data_->other_sectors_) {
        store_sector(*s);
    }

    if (not ram_filesystem::store_file_data(pfrm, save::path, save_data)) {
        info(pfrm, "macro save failed!");
    }
}



macro::terrain::Sector& State::sector()
{
    if (data_->current_sector_ == -1) {
        return data_->origin_sector_;
    } else {
        if ((u32)data_->current_sector_ >= data_->other_sectors_.size()) {
            Platform::fatal("out of bounds sector access");
        }
        return *data_->other_sectors_[data_->current_sector_];
    }
}



bool State::load(Platform& pfrm)
{
    Vector<char> input;

    if (ram_filesystem::read_file_data(pfrm, save::path, input)) {
        auto it = input.begin();

        save::Header header;
        for (u32 i = 0; i < sizeof header; ++i) {
            if (it == input.end()) {
                Platform::fatal("macro save data invalid!");
            }
            ((u8*)&header)[i] = *it;
            ++it;
        }

        memcpy(&data_->p(), &header.p_, sizeof header.p_);

        auto load_sector = [&](terrain::Sector* dest) {
            save::Sector<0> s;
            for (u32 i = 0; i < sizeof s; ++i) {
                if (it == input.end()) {
                    Platform::fatal("macro save data invalid!");
                }
                ((u8*)&s)[i] = *it;
                ++it;
            }

            if (dest == nullptr) {
                if (make_sector({s.p_.p_.x_, s.p_.p_.y_}, s.p_.p_.shape_)) {
                    dest = data_->other_sectors_.back();
                }
            }

            if (dest == nullptr) {
                info(pfrm, "failed to load sector!");
            }

            switch (s.p_.p_.shape_) {
            case terrain::Sector::Shape::cube:
                dest->restore(s.p_.p_, s.blocks_.cube_);
                break;

            case terrain::Sector::Shape::pancake:
                dest->restore(s.p_.p_, s.blocks_.pancake_);
                break;
            }

            u8 export_count = *(it++);

            for (int i = 0; i < export_count; ++i) {
                terrain::Sector::ExportInfo info;
                for (u32 i = 0; i < sizeof info; ++i) {
                    if (it == input.end()) {
                        Platform::fatal("failed while loading exports");
                    }
                    ((u8*)&info)[i] = *it;
                    ++it;
                }
                dest->set_export(info);
            }

            dest->shadowcast();
        };

        load_sector(&data_->origin_sector_);

        data_->erase_other_sectors();
        data_->other_sector_mem_.reset();


        for (int i = 0; i < header.num_sectors_ - 1; ++i) {
            load_sector(nullptr);
        }

    } else /* No existing save file */ {

        newgame(pfrm);
    }

    data_->current_sector_ = -1;
    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;

    return true;
}



bool State::make_sector(Vec2<s8> coord, terrain::Sector::Shape shape)
{
    if (load_sector(coord)) {
        return false;
    }

    if (data_->other_sectors_.full()) {
        return false;
    }

    if (not data_->other_sector_mem_) {
        data_->other_sector_mem_.emplace(Platform::instance());
    }

    auto s = [&]() -> terrain::Sector* {
        switch (shape) {
        case terrain::Sector::Shape::cube:
            return data_->other_sector_mem_->alloc<terrain::CubeSector>(coord)
                .release();

        case terrain::Sector::Shape::pancake:
            return data_->other_sector_mem_
                ->alloc<terrain::PancakeSector>(coord)
                .release();

        default:
            return nullptr;
        }
    }();
    if (s) {
        StringBuffer<terrain::Sector::name_len - 1> n("colony_");
        n += stringify(data_->other_sectors_.size() + 1).c_str();
        s->set_name(n);
        return data_->other_sectors_.push_back(s);
    } else {
        Platform::fatal("failed to allocate sector!");
    }
    return false;
}



namespace terrain
{
Stats stats(Type t, bool shadowed)
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

    case terrain::Type::potatoes:
        result.food_ += 15;
        result.employment_ += 3;
        break;

    case terrain::Type::sunflowers:
        if (not shadowed) {
            result.commodities_.push_back({Commodity::Type::sunflowers, 1});
        }
        result.employment_ += 2;
        break;

    case terrain::Type::indigo:
        if (not shadowed) {
            result.commodities_.push_back({Commodity::Type::indigo, 1});
        }
        result.employment_ += 4;
        break;

    case terrain::Type::madder:
        if (not shadowed) {
            result.commodities_.push_back({Commodity::Type::rose_madder, 1});
        }
        result.employment_ += 4;
        break;

    case terrain::Type::shellfish:
        if (not shadowed) {
            result.commodities_.push_back({Commodity::Type::shellfish, 1});
        }
        result.employment_ += 1;
        result.food_ += 2;
        break;

    case terrain::Type::cocoa:
        if (not shadowed) {
            result.commodities_.push_back({Commodity::Type::cocoa, 3});
            result.employment_ += 6;
        }
        break;

    case terrain::Type::wool:
        result.employment_ += 1;
        result.food_ += 1;
        result.commodities_.push_back({Commodity::Type::wool, 2});
        break;

    case terrain::Type::saffron:
        result.employment_ += 6;
        result.commodities_.push_back({Commodity::Type::saffron, 2});
        break;

    case terrain::Type::windmill_stone_base:
    case terrain::Type::windmill:
        result.employment_ += 20;
        break;

    case terrain::Type::port:
        result.employment_ += 8;
        break;

    default:
        break;
    }

    return result;
}
} // namespace terrain



Coins terrain::Commodity::value(Commodity::Type t)
{
    if (t == Commodity::Type::food) {
        Platform::fatal("attempt to appraise value of food");
    }
    return 6;
}



SystemString terrain::name(terrain::Commodity::Type t)
{
    switch (t) {
    case Commodity::indigo:
        return SystemString::block_indigo;

    case Commodity::rose_madder:
        return SystemString::block_madder;

    case Commodity::shellfish:
        return SystemString::block_shellfish;

    case Commodity::sunflowers:
        return SystemString::block_sunflower;

    case Commodity::wool:
        return SystemString::block_wool;

    case Commodity::food:
        return SystemString::block_food;

    case Commodity::saffron:
        return SystemString::block_saffron;

    case Commodity::cocoa:
        return SystemString::block_cocoa;
    }

    return SystemString::empty;
}



SystemString terrain::Commodity::name() const
{
    return terrain::name(type_);
}



terrain::Stats terrain::Block::stats() const
{
    auto st = terrain::stats(type(), shadowed_);
    if (shadowed_) {
        st.food_ /= 2;
    }
    return st;
}



terrain::Improvements terrain::Block::improvements() const
{
    return terrain::improvements(type());
}



terrain::Categories terrain::categories(Type t)
{
    switch (t) {
    default:
        return Categories::basic;

    case terrain::Type::wool:
        return Categories::livestock;

    case terrain::Type::wheat:
    case terrain::Type::potatoes:
    case terrain::Type::madder:
    case terrain::Type::indigo:
    case terrain::Type::sunflowers:
    case terrain::Type::saffron:
    case terrain::Type::cocoa:
        return Categories::crop;

    case terrain::Type::water_source:
    case terrain::Type::water_spread_downwards:
    case terrain::Type::water_spread_laterally_a:
    case terrain::Type::water_spread_laterally_b:
    case terrain::Type::water_spread_laterally_c:
    case terrain::Type::water_spread_laterally_d:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return Categories::fluid_water;

    case terrain::Type::lava_source:
    case terrain::Type::lava_spread_downwards:
    case terrain::Type::lava_spread_laterally_a:
    case terrain::Type::lava_spread_laterally_b:
    case terrain::Type::lava_spread_laterally_c:
    case terrain::Type::lava_spread_laterally_d:
    case terrain::Type::lava_slant_a:
    case terrain::Type::lava_slant_b:
    case terrain::Type::lava_slant_c:
    case terrain::Type::lava_slant_d:
        return Categories::fluid_lava;

    case terrain::Type::shellfish:
        return (Categories)(Categories::crop | Categories::fluid_water);
    }
}



Coins terrain::cost(Sector& s, Type t)
{
    switch (t) {
    case terrain::Type::food:
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

    case terrain::Type::volcanic_soil:
        return 100;

    case terrain::Type::cocoa:
        return 160;

    case terrain::Type::ice:
        return 5;

    case terrain::Type::shrubbery:
        return 5;

    case terrain::Type::count:
    case terrain::Type::selector:
        return 0;

    case terrain::Type::water_source:
    case terrain::Type::water_spread_downwards:
    case terrain::Type::water_spread_laterally_a:
    case terrain::Type::water_spread_laterally_b:
    case terrain::Type::water_spread_laterally_c:
    case terrain::Type::water_spread_laterally_d:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return 30;

    case terrain::Type::lava_source:
    case terrain::Type::lava_spread_downwards:
    case terrain::Type::lava_spread_laterally_a:
    case terrain::Type::lava_spread_laterally_b:
    case terrain::Type::lava_spread_laterally_c:
    case terrain::Type::lava_spread_laterally_d:
    case terrain::Type::lava_slant_a:
    case terrain::Type::lava_slant_b:
    case terrain::Type::lava_slant_c:
    case terrain::Type::lava_slant_d:
        return 800;

    case terrain::Type::wheat:
        return 40;

    case terrain::Type::potatoes:
        return 270;

    case terrain::Type::sunflowers:
        return 120;

    case terrain::Type::indigo:
        return 120;

    case terrain::Type::shellfish:
        return 120;

    case terrain::Type::wool:
        return 260;

    case terrain::Type::saffron:
        return 260;

    case terrain::Type::madder:
        return 120;

    case terrain::Type::gold:
        return 1000;

    case terrain::Type::workshop:
        return 100;

    case terrain::Type::light_source:
        return 200;

    case terrain::Type::windmill_stone_base:
        return 150;

    case terrain::Type::windmill:
        return 80;

    case terrain::Type::port:
        return 200;
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

    case terrain::Type::volcanic_soil:
        return SystemString::block_volcanic_soil;

    case terrain::Type::shrubbery:
        return SystemString::block_shrubbery;

    case terrain::Type::count:
    case terrain::Type::selector:
        return SystemString::gs_error;

    case terrain::Type::water_source:
    case terrain::Type::water_spread_downwards:
    case terrain::Type::water_spread_laterally_a:
    case terrain::Type::water_spread_laterally_b:
    case terrain::Type::water_spread_laterally_c:
    case terrain::Type::water_spread_laterally_d:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return SystemString::block_water;

    case terrain::Type::lava_source:
    case terrain::Type::lava_spread_downwards:
    case terrain::Type::lava_spread_laterally_a:
    case terrain::Type::lava_spread_laterally_b:
    case terrain::Type::lava_spread_laterally_c:
    case terrain::Type::lava_spread_laterally_d:
    case terrain::Type::lava_slant_a:
    case terrain::Type::lava_slant_b:
    case terrain::Type::lava_slant_c:
    case terrain::Type::lava_slant_d:
        return SystemString::block_lava;

    case terrain::Type::ice:
        return SystemString::block_ice;

    case terrain::Type::wheat:
        return SystemString::block_wheat;

    case terrain::Type::potatoes:
        return SystemString::block_potatoes;

    case terrain::Type::sunflowers:
        return SystemString::block_sunflower;

    case terrain::Type::indigo:
        return SystemString::block_indigo;

    case terrain::Type::madder:
        return SystemString::block_madder;

    case terrain::Type::shellfish:
        return SystemString::block_shellfish;

    case terrain::Type::cocoa:
        return SystemString::block_cocoa;

    case terrain::Type::wool:
        return SystemString::block_wool;

    case terrain::Type::saffron:
        return SystemString::block_saffron;

    case terrain::Type::gold:
        return SystemString::block_gold;

    case terrain::Type::workshop:
        return SystemString::block_workshop;

    case terrain::Type::light_source:
        return SystemString::block_light_source;

    case terrain::Type::windmill_stone_base:
    case terrain::Type::windmill:
        return SystemString::block_windmill;

    case terrain::Type::port:
        return SystemString::block_harbor;

    case terrain::Type::food:
        return SystemString::block_food;
    }

    return SystemString::gs_error;
}



SystemString terrain::Block::name() const
{
    return terrain::name(type());
}



terrain::Improvements terrain::improvements(Type t)
{
    terrain::Improvements result;

    auto remove_self = [&] {
        for (auto it = result.begin(); it not_eq result.end(); ++it) {
            if (*it == t) {
                result.erase(it);
                return;
            }
        }
    };


    auto push_terrain_defaults = [&] {
        result.push_back(Type::wheat);
        result.push_back(Type::potatoes);
        result.push_back(Type::windmill);
        result.push_back(Type::indigo);
        result.push_back(Type::madder);
        result.push_back(Type::sunflowers);
        result.push_back(Type::saffron);
        result.push_back(Type::wool);
        remove_self();
    };

    switch (t) {
    case Type::wheat:
    case Type::potatoes:
    case Type::sunflowers:
    case Type::indigo:
    case Type::madder:
    case Type::wool:
    case Type::saffron:
    case Type::terrain: {
        push_terrain_defaults();
        break;
    }

    case terrain::Type::water_source:
    case terrain::Type::water_spread_downwards:
    case terrain::Type::water_spread_laterally_a:
    case terrain::Type::water_spread_laterally_b:
    case terrain::Type::water_spread_laterally_c:
    case terrain::Type::water_spread_laterally_d:
        result.push_back(Type::ice);
        result.push_back(Type::shellfish);
        break;

    case Type::masonry:
        result.push_back(Type::windmill_stone_base);
        break;

    case Type::volcanic_soil:
        result.push_back(Type::cocoa);
        break;

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
        return {2760, 2776};

    case terrain::Type::terrain:
        return {2632, 2648};

    case terrain::Type::masonry:
        return {1448, 1464};

    case terrain::Type::cocoa:
        return {1864, 1880};

    case terrain::Type::ice:
        return {2344, 2360};

    case terrain::Type::shrubbery:
        return {1416, 1432};

    case terrain::Type::volcanic_soil:
    case terrain::Type::count:
    case terrain::Type::__invalid:
    case terrain::Type::selector:
        return {};

    case terrain::Type::shellfish:
        return {2824, 2840};

    case terrain::Type::wool:
        return {2920, 2936};

    case terrain::Type::saffron:
        return {2952, 2968};

    case terrain::Type::water_source:
    case terrain::Type::water_spread_downwards:
    case terrain::Type::water_spread_laterally_a:
    case terrain::Type::water_spread_laterally_b:
    case terrain::Type::water_spread_laterally_c:
    case terrain::Type::water_spread_laterally_d:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        return {2120, 2136};

    case terrain::Type::lava_source:
    case terrain::Type::lava_spread_downwards:
    case terrain::Type::lava_spread_laterally_a:
    case terrain::Type::lava_spread_laterally_b:
    case terrain::Type::lava_spread_laterally_c:
    case terrain::Type::lava_spread_laterally_d:
    case terrain::Type::lava_slant_a:
    case terrain::Type::lava_slant_b:
    case terrain::Type::lava_slant_c:
    case terrain::Type::lava_slant_d:
        return {2152, 2168};

    case terrain::Type::potatoes:
        return {2856, 2872};

    case terrain::Type::sunflowers:
        return {1896, 1912};

    case terrain::Type::wheat:
        return {2728, 2744};

    case terrain::Type::indigo:
        return {2696, 2712};

    case terrain::Type::madder:
        return {2664, 2680};

    case terrain::Type::gold:
        return {2440, 2456};

    case terrain::Type::workshop:
        return {776, 760};

    case terrain::Type::light_source:
        return {2280, 2296};

    case terrain::Type::windmill_stone_base:
    case terrain::Type::windmill:
        return {2792, 2808};

    case terrain::Type::port:
        return {776, 760};

    case terrain::Type::food:
        return {2888, 2904};
    }

    return {};
}



static bool revert_if_covered(terrain::Sector& s,
                              terrain::Block& block,
                              Vec3<u8> position,
                              terrain::Type revert_to)
{
    if (position.z < terrain::Sector::z_limit) {
        position.z++;
        auto& above = s.get_block(position);
        if (revert_to == terrain::Type::terrain and
            terrain::categories(above.type()) &
                terrain::Categories::fluid_lava) {
            block.type_ = (u8)terrain::Type::volcanic_soil;
            block.repaint_ = true;
            raster::globalstate::_changed = true;
            s.clear_cache();
            return true;
        } else if (above.type() not_eq terrain::Type::selector and
                   above.type() not_eq terrain::Type::air) {
            block.type_ = (u8)revert_to;
            block.repaint_ = true;
            raster::globalstate::_changed = true;
            s.clear_cache();
            return true;
        }
    }

    return false;
}



static bool destroyed_by_lava(terrain::Type t)
{
    return t == terrain::Type::building or t == terrain::Type::port or
           t == terrain::Type::shrubbery or t == terrain::Type::ice or
           categories(t) & terrain::Categories::fluid_water;
}



static void update_lava_slanted(terrain::Sector& s,
                                terrain::Block& block,
                                Vec3<u8> position)
{
    if (position.z == 0) {
        return;
    }

    const Vec3<u8> beneath_coord = {position.x, position.y, u8(position.z - 1)};

    auto& beneath = s.get_block(beneath_coord);
    const auto tp = beneath.type();
    if (tp == terrain::Type::air or destroyed_by_lava(tp)) {
        s.set_block(beneath_coord, terrain::Type::lava_spread_downwards);
    } else if ((categories(tp) & terrain::Categories::fluid_lava) and
               tp not_eq terrain::Type::lava_source and
               tp not_eq terrain::Type::lava_spread_downwards and
               tp not_eq terrain::Type::lava_spread_laterally_a and
               tp not_eq terrain::Type::lava_spread_laterally_b and
               tp not_eq terrain::Type::lava_spread_laterally_c and
               tp not_eq terrain::Type::lava_spread_laterally_d) {
        s.set_block(beneath_coord, terrain::Type::lava_spread_downwards);
    }
}



static void lava_spread(terrain::Sector& s, Vec3<u8> target, terrain::Type tp)
{
    auto prev_tp = s.get_block(target).type();
    if (UNLIKELY(prev_tp not_eq tp and
                 (prev_tp == terrain::Type::lava_slant_a or
                  prev_tp == terrain::Type::lava_slant_b or
                  prev_tp == terrain::Type::lava_slant_c or
                  prev_tp == terrain::Type::lava_slant_d))) {
        switch (tp) {
        case terrain::Type::lava_slant_a:
            s.set_block(target, terrain::Type::lava_spread_laterally_a);
            break;

        case terrain::Type::lava_slant_b:
            s.set_block(target, terrain::Type::lava_spread_laterally_b);
            break;

        case terrain::Type::lava_slant_c:
            s.set_block(target, terrain::Type::lava_spread_laterally_c);
            break;

        case terrain::Type::lava_slant_d:
            s.set_block(target, terrain::Type::lava_spread_laterally_d);
            break;

        default:
            Platform::fatal("Invalid water lateral spread type");
        }
    } else if (prev_tp == terrain::Type::air or destroyed_by_lava(prev_tp)) {
        s.set_block(target, tp);
    }
}



static void
update_lava_still(terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
{
    const Vec3<u8> beneath_coord = {position.x, position.y, u8(position.z - 1)};

    auto beneath_tp = terrain::Type::air;
    if (position.z > 0) {
        auto& beneath = s.get_block(beneath_coord);
        beneath_tp = beneath.type();
    }

    if (position.z > 0 and
        (destroyed_by_lava(beneath_tp) or beneath_tp == terrain::Type::air or
         beneath_tp == terrain::Type::lava_slant_a or
         beneath_tp == terrain::Type::lava_slant_b or
         beneath_tp == terrain::Type::lava_slant_c or
         beneath_tp == terrain::Type::lava_slant_d)) {
        s.set_block(beneath_coord, terrain::Type::lava_spread_downwards);
    } else if (position.z == 0 or not(terrain::categories(beneath_tp) &
                                      terrain::Categories::fluid_lava)) {
        auto lp = position;
        lp.x++;

        if (position.x < s.size().x - 1) {
            lava_spread(s, lp, terrain::Type::lava_slant_a);
        }

        if (position.y < s.size().y - 1) {
            auto rp = position;
            ++rp.y;
            lava_spread(s, rp, terrain::Type::lava_slant_b);
        }

        if (position.x > 0) {
            auto up = position;
            --up.x;
            lava_spread(s, up, terrain::Type::lava_slant_c);
        }

        if (position.y > 0) {
            auto down = position;
            --down.y;
            lava_spread(s, down, terrain::Type::lava_slant_d);
        }
    }
}



static void update_water_slanted(terrain::Sector& s,
                                 terrain::Block& block,
                                 Vec3<u8> position)
{
    if (position.z == 0) {
        return;
    }

    const Vec3<u8> beneath_coord = {position.x, position.y, u8(position.z - 1)};

    auto& beneath = s.get_block(beneath_coord);
    const auto tp = beneath.type();
    if (terrain::categories(tp) & terrain::Categories::fluid_lava) {
        s.set_block(beneath_coord, terrain::Type::volcanic_soil);
    } else if (tp == terrain::Type::air) {
        s.set_block(beneath_coord, terrain::Type::water_spread_downwards);
    } else if ((categories(tp) & terrain::Categories::fluid_water) and
               tp not_eq terrain::Type::water_source and
               tp not_eq terrain::Type::water_spread_downwards and
               tp not_eq terrain::Type::water_spread_laterally_a and
               tp not_eq terrain::Type::water_spread_laterally_b and
               tp not_eq terrain::Type::water_spread_laterally_c and
               tp not_eq terrain::Type::water_spread_laterally_d) {
        s.set_block(beneath_coord, terrain::Type::water_spread_downwards);
    }
}



static void water_spread(terrain::Sector& s, Vec3<u8> target, terrain::Type tp)
{
    auto prev_tp = s.get_block(target).type();
    if (terrain::categories(tp) & terrain::Categories::fluid_lava) {
        s.set_block(target, terrain::Type::volcanic_soil);
    }
    if (UNLIKELY(prev_tp not_eq tp and
                 (prev_tp == terrain::Type::water_slant_a or
                  prev_tp == terrain::Type::water_slant_b or
                  prev_tp == terrain::Type::water_slant_c or
                  prev_tp == terrain::Type::water_slant_d))) {
        switch (tp) {
        case terrain::Type::water_slant_a:
            s.set_block(target, terrain::Type::water_spread_laterally_a);
            break;

        case terrain::Type::water_slant_b:
            s.set_block(target, terrain::Type::water_spread_laterally_b);
            break;

        case terrain::Type::water_slant_c:
            s.set_block(target, terrain::Type::water_spread_laterally_c);
            break;

        case terrain::Type::water_slant_d:
            s.set_block(target, terrain::Type::water_spread_laterally_d);
            break;

        default:
            Platform::fatal("Invalid water lateral spread type");
        }
    } else if (prev_tp == terrain::Type::air) {
        s.set_block(target, tp);
    }
}



static void
update_water_still(terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
{
    const Vec3<u8> beneath_coord = {position.x, position.y, u8(position.z - 1)};

    auto beneath_tp = terrain::Type::air;
    if (position.z > 0) {
        auto& beneath = s.get_block(beneath_coord);
        beneath_tp = beneath.type();
    }

    if (position.z > 0 and
        terrain::categories(beneath_tp) & terrain::Categories::fluid_lava) {
        s.set_block(beneath_coord, terrain::Type::volcanic_soil);
    } else if (position.z > 0 and
               (beneath_tp == terrain::Type::air or
                beneath_tp == terrain::Type::water_slant_a or
                beneath_tp == terrain::Type::water_slant_b or
                beneath_tp == terrain::Type::water_slant_c or
                beneath_tp == terrain::Type::water_slant_d)) {
        s.set_block(beneath_coord, terrain::Type::water_spread_downwards);
    } else if (position.z == 0 or not(terrain::categories(beneath_tp) &
                                      terrain::Categories::fluid_water)) {
        auto lp = position;
        lp.x++;

        if (position.x < s.size().x - 1) {
            water_spread(s, lp, terrain::Type::water_slant_a);
        }

        if (position.y < s.size().y - 1) {
            auto rp = position;
            ++rp.y;
            water_spread(s, rp, terrain::Type::water_slant_b);
        }

        if (position.x > 0) {
            auto up = position;
            --up.x;
            water_spread(s, up, terrain::Type::water_slant_c);
        }

        if (position.y > 0) {
            auto down = position;
            --down.y;
            water_spread(s, down, terrain::Type::water_slant_d);
        }
    }
}



static bool is_still_water(terrain::Type t)
{
    return (terrain::categories(t) & terrain::Categories::fluid_water) and
           t not_eq terrain::Type::water_slant_a and
           t not_eq terrain::Type::water_slant_b and
           t not_eq terrain::Type::water_slant_c and
           t not_eq terrain::Type::water_slant_d;
}



static bool is_still_lava(terrain::Type t)
{
    return (terrain::categories(t) & terrain::Categories::fluid_lava) and
           t not_eq terrain::Type::lava_slant_a and
           t not_eq terrain::Type::lava_slant_b and
           t not_eq terrain::Type::lava_slant_c and
           t not_eq terrain::Type::lava_slant_d;
}



static const int lava_spread_viscosity = 12;



template <typename F>
bool parent_exists_dir_a(terrain::Sector& s,
                         terrain::Block& block,
                         Vec3<u8> position,
                         F&& typecheck)
{
    if (position.x > 0) {
        auto behind = position;
        behind.x--;
        auto& block = s.get_block(behind);
        return typecheck(block.type());
    }
    return true;
}



template <typename F>
bool parent_exists_dir_b(terrain::Sector& s,
                         terrain::Block& block,
                         Vec3<u8> position,
                         F&& typecheck)
{
    if (position.y > 0) {
        auto behind = position;
        --behind.y;
        auto& block = s.get_block(behind);
        return typecheck(block.type());
    }
    return true;
}



template <typename F>
bool parent_exists_dir_c(terrain::Sector& s,
                         terrain::Block& block,
                         Vec3<u8> position,
                         F&& typecheck)
{
    if (position.x < s.size().x - 1) {
        auto behind = position;
        ++behind.x;
        auto& block = s.get_block(behind);
        return typecheck(block.type());
    }
    return true;
}



template <typename F>
bool parent_exists_dir_d(terrain::Sector& s,
                         terrain::Block& block,
                         Vec3<u8> position,
                         F&& typecheck)
{
    if (position.y < s.size().y - 1) {
        auto behind = position;
        ++behind.y;
        auto& block = s.get_block(behind);
        return typecheck(block.type());
    }
    return true;
}



// clang-format off
typedef void(*UpdateFunction)(terrain::Sector&, terrain::Block&, Vec3<u8>);
static const UpdateFunction update_functions[(int)terrain::Type::count] = {
    nullptr, // Air has no update code.
    // building
    nullptr,
    // __invalid
    [](terrain::Sector&, terrain::Block& block, Vec3<u8> position)
    {
        Platform::fatal("invoke hook for invalid block!");
    },
    // water
    update_water_still,
    // terrain
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (position.z < terrain::Sector::z_limit) {
            position.z++;
            auto& above = s.get_block(position);
            if (terrain::categories(above.type()) & terrain::Categories::fluid_lava) {
                block.type_ = (u8)terrain::Type::volcanic_soil;
                block.repaint_ = true;
                raster::globalstate::_changed = true;
                s.clear_cache();
            }
        }
    },
    // masonry
    nullptr,
    // selector
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_--;
        if (block.data_ == 0) {
            block.data_ = 10;
            block.shadowed_ = not block.shadowed_;
            block.repaint_ = true;
            raster::globalstate::_changed_cursor_flicker_only = true;
        }

    },
    // wheat
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // indigo
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // madder
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // gold
    nullptr,
    // workshop
    nullptr,
    // water_slant_a
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_a(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_slanted(s, block, position);
    },
    // water_slant_b
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_b(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_slanted(s, block, position);
    },
    // water_slant_c
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_c(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_slanted(s, block, position);
    },
    // water_slant_d
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_d(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_slanted(s, block, position);
    },
    // light source
    nullptr,
    // windmill
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // windmill_stone_base
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::masonry);
    },
    // shellfish
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not revert_if_covered(s, block, position, terrain::Type::water_source)) {
            update_water_still(s, block, position);
        }

    },
    // port
    nullptr,
    // potatoes
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // sunflowers
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // food
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        Platform::fatal("food sentinel created as a terrain block");
    },
    // shrubbery
    nullptr,
    // wool
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // saffron
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // ice
    nullptr,
    // lava
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ > lava_spread_viscosity) {
            update_lava_still(s, block, position);
            block.data_ = 0;
        }
    },
    // lava_slant_a
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ < lava_spread_viscosity + 1) {
            return;
        }
        block.data_ = 0;

        if (not parent_exists_dir_a(s, block, position, is_still_lava)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_lava_slanted(s, block, position);
    },
    // lava_slant_b
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ < lava_spread_viscosity + 1) {
            return;
        }
        block.data_ = 0;

        if (not parent_exists_dir_b(s, block, position, is_still_lava)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_lava_slanted(s, block, position);
    },
    // lava_slant_c
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ < lava_spread_viscosity + 1) {
            return;
        }
        block.data_ = 0;

        if (not parent_exists_dir_c(s, block, position, is_still_lava)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_lava_slanted(s, block, position);
    },
    // lava_slant_d
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ < lava_spread_viscosity + 1) {
            return;
        }
        block.data_ = 0;

        if (not parent_exists_dir_d(s, block, position, is_still_lava)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_lava_slanted(s, block, position);
    },
    // volcanic_soil
    nullptr,
    // cocoa
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::volcanic_soil);
    },
    // water_spread_down
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        update_water_still(s, block, position);

        if (position.z < terrain::Sector::z_limit - 1) {
            auto above_coord = position;
            ++above_coord.z;
            auto& above = s.get_block(above_coord);
            if (not (terrain::categories(above.type()) &
                     terrain::Categories::fluid_water)) {
                s.set_block(position, terrain::Type::air);
            }
        }
    },
    // water_spread_laterally_a
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_a(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_still(s, block, position);
    },
    // water_spread_laterally_b
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_b(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_still(s, block, position);
    },
    // water_spread_laterally_c
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_c(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_still(s, block, position);
    },
    // water_spread_laterally_d
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not parent_exists_dir_d(s, block, position, is_still_water)) {
            s.set_block(position, terrain::Type::air);
            return;
        }
        update_water_still(s, block, position);
    },
    // lava_spread_down
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ < lava_spread_viscosity + 1) {
            return;
        }

        block.data_ = 0;

        update_lava_still(s, block, position);

        if (position.z < terrain::Sector::z_limit - 1) {
            auto above_coord = position;
            ++above_coord.z;
            auto& above = s.get_block(above_coord);
            if (not (terrain::categories(above.type()) &
                     terrain::Categories::fluid_lava)) {
                s.set_block(position, terrain::Type::air);
            }
        }
    },
    // lava_spread_laterally_a
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ > lava_spread_viscosity) {
            if (not parent_exists_dir_a(s, block, position, is_still_lava)) {
                s.set_block(position, terrain::Type::air);
                return;
            }
            update_lava_still(s, block, position);
            block.data_ = 0;
        }
    },
    // lava_spread_laterally_b
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ > lava_spread_viscosity) {
            if (not parent_exists_dir_b(s, block, position, is_still_lava)) {
                s.set_block(position, terrain::Type::air);
                return;
            }
            update_lava_still(s, block, position);
            block.data_ = 0;
        }
    },
    // lava_spread_laterally_c
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ > lava_spread_viscosity) {
            if (not parent_exists_dir_c(s, block, position, is_still_lava)) {
                s.set_block(position, terrain::Type::air);
                return;
            }
            update_lava_still(s, block, position);
            block.data_ = 0;
        }
    },
    // lava_spread_laterally_d
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        block.data_++;
        if (block.data_ > lava_spread_viscosity) {
            if (not parent_exists_dir_d(s, block, position, is_still_lava)) {
                s.set_block(position, terrain::Type::air);
                return;
            }
            update_lava_still(s, block, position);
            block.data_ = 0;
        }
    },
};
// clang-format on



namespace terrain
{
bool blocks_light(terrain::Type t)
{
    if (t == terrain::Type::air or t == terrain::Type::selector or
        (terrain::categories(t) & terrain::Categories::fluid_water) or
        (terrain::categories(t) & terrain::Categories::fluid_lava)) {
        return false;
    }

    return true;
}
} // namespace terrain



void terrain::CubeSector::update()
{
    for (int z = 0; z < z_limit; ++z) {
        for (u8 x = 0; x < 8; ++x) {
            for (u8 y = 0; y < 8; ++y) {

                auto& block = blocks_[z][x][y];

                auto update = update_functions[block.type_];
                if (update) {
                    update(*this, block, {x, y, (u8)z});
                }
            }
        }
    }
}



void terrain::PancakeSector::update()
{
    for (int z = 0; z < z_limit; ++z) {
        for (u8 x = 0; x < length; ++x) {
            for (u8 y = 0; y < length; ++y) {

                auto& block = blocks_[z][x][y];

                auto update = update_functions[block.type_];
                if (update) {
                    update(*this, block, {x, y, (u8)z});
                }
            }
        }
    }
}



// Some texture indices completely cover everything underneath them, allowing
// the render to skip some steps.
raster::TileCategory raster::tile_category(int texture_id)
{
    // NOTE: for our isometric tiles, the middle row is fully opaque, i.e. we
    // don't need to worry about rendering anything underneath. The top and
    // bottom rows have transparent pixels, and cannot necessarily be skipped.

    // Could I clean this table up to use less space? I tried to. But we
    // basically have six tiles per isometric block, and I'm trying to avoid a
    // div/mod 6. Each entry in the table describes the shape of the isometric
    // tile in the tile texture.

#define ISO_DEFAULT_CGS                                                        \
    top_angled_l, top_angled_r, opaque, opaque, bot_angled_l, bot_angled_r

#define ISO_SELECTOR_CGS                                                       \
    irregular, irregular, irregular, irregular, irregular, irregular

    // clang-format off
    static const std::array<TileCategory,
                            // NOTE: 6 tiles per block, x2 for shadowed blocks.
                            (int)terrain::Type::count * 6 * 2> category =
        {ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_SELECTOR_CGS,
         ISO_SELECTOR_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         // Non-standard shapes for slanted water blocks
         irregular, top_angled_r, irregular, opaque, bot_angled_l, bot_angled_r,
         irregular, top_angled_r, irregular, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, irregular, opaque, irregular, bot_angled_l, bot_angled_r,
         top_angled_l, irregular, opaque, irregular, bot_angled_l, bot_angled_r,
         irregular, irregular, opaque, top_angled_r, bot_angled_l, bot_angled_r,
         irregular, irregular, opaque, top_angled_r, bot_angled_l, bot_angled_r,
         irregular, irregular, top_angled_l, opaque, bot_angled_l, bot_angled_r,
         irregular, irregular, top_angled_l, opaque, bot_angled_l, bot_angled_r,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         // Non-standard shapes for slanted lava blocks
         irregular, top_angled_r, irregular, opaque, bot_angled_l, bot_angled_r,
         irregular, top_angled_r, irregular, opaque, bot_angled_l, bot_angled_r,
         top_angled_l, irregular, opaque, irregular, bot_angled_l, bot_angled_r,
         top_angled_l, irregular, opaque, irregular, bot_angled_l, bot_angled_r,
         irregular, irregular, opaque, top_angled_r, bot_angled_l, bot_angled_r,
         irregular, irregular, opaque, top_angled_r, bot_angled_l, bot_angled_r,
         irregular, irregular, top_angled_l, opaque, bot_angled_l, bot_angled_r,
         irregular, irregular, top_angled_l, opaque, bot_angled_l, bot_angled_r,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

        };
    // clang-format on

    return category[texture_id];
}



} // namespace skyland::macro
