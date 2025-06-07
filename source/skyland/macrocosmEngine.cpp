////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "macrocosmEngine.hpp"
#include "allocator.hpp"
#include "macrocosmFreebuildFlatSector.hpp"
#include "macrocosmFreebuildSector.hpp"
#include "macrocosmFreebuildWideSector.hpp"
#include "macrocosmPancakeSector.hpp"
#include "macrocosmPillarSector.hpp"
#include "memory/buffer.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include "rle.hpp"
#include "skyland/entity/macro/macrocosmEffect.hpp"
#include "skyland/skyland.hpp"
extern "C" {
// FIXME!!!
#include "heatshrink/heatshrink_decoder.c"
}
#include "compression.hpp"
#include "script/listBuilder.hpp"



namespace skyland::macro
{



namespace raster
{



Optional<DynamicMemory<raster::DepthBuffer>> _db;



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
bool is_night = false;
bool _upper_half_only = false;

Bitvector<RASTER_CELLCOUNT * 2> _recalc_depth_test;

} // namespace globalstate
} // namespace raster



EngineImpl* _bound_state;



EngineImpl& bound_state()
{
    if (not _bound_state) {
        Platform::fatal("unbound macro engine state");
    }
    return *_bound_state;
}



EngineImpl::EngineImpl(App* app)
    : data_(allocate_dynamic<Data>("macrocosm-data"))
{
    _bound_state = this;

    if (app) {
        app->invoke_script("/scripts/config/macro.lisp", true);
    }
}



EngineImpl::Data::Data()
    : origin_sector_(
          allocate_dynamic<terrain::FreebuildWideSector>("sector-mem",
                                                         Vec2<s8>{0, 0})),
      current_sector_(&*origin_sector_)
{
}



void EngineImpl::newgame()
{
    // data_->current_sector_ = &data_->origin_sector_;

    PLATFORM.load_background_texture("background_macro");
    raster::globalstate::is_night = false;

    data_->other_sectors_.clear();
    memset((void*)&data_->p(), 0, sizeof(Data::Persistent));

    data_->p().year_.set(0);
    data_->p().day_.set(0);


    auto& sector = this->sector();
    sector.erase();
    sector.set_name("origin");

    sector.generate_terrain_origin(120);

    if (data_->checkers_mode_) {
        // ...
    } else if (data_->freebuild_mode_) {
        APP.invoke_script("/scripts/macro/start_layout.lisp");
    } else {
        APP.invoke_script("/scripts/macro/newgame.lisp");
    }

    sector.set_population(1);
    sector.on_day_transition();
    sector.set_food(5);
    sector.recalc_stats();
}



Float EngineImpl::commodity_diminishing_return_percent()
{
    return 0;
}



int EngineImpl::food_consumption_factor()
{
    return 1;
}



std::pair<Coins, Population> EngineImpl::colony_cost() const
{
    if (data_->other_sectors_.full()) {
        return {999999999, 9999};
    } else if (data_->other_sectors_.size() == 0) {
        return {1500 + 3000 * data_->other_sectors_.size(), 100};
    } else if (data_->other_sectors_.size() < 3) {
        return {1500 + 3000 * data_->other_sectors_.size(), 150};
    } else if (data_->other_sectors_.size() < 4) {
        return {4000 + 3200 * data_->other_sectors_.size(), 200};
    } else if (data_->other_sectors_.size() < 6) {
        return {6000 + 3600 * data_->other_sectors_.size(), 400};
    } else {
        return {7000 + 4000 * data_->other_sectors_.size(), 600};
    }
}



namespace save
{
static const char* path = "/save/macro2.dat";
static const char* timestamp_path = "/save/mt.dat";


struct Header
{
    EngineImpl::Data::Persistent p_;
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
        u8 pillar_[16][6][6];
        u8 fb_wide_[6][12][12];
    } blocks_;


    auto read(Vector<char>& save_data, Vector<char>::Iterator rd)
    {
        for (u32 i = 0; i < sizeof p_; ++i) {
            if (rd == save_data.end()) {
                Platform::fatal("save data corrupted!");
            }

            ((u8*)&p_)[i] = *rd;
            ++rd;
        }

        HostInteger<u16> encoded_size;
        for (u32 i = 0; i < sizeof encoded_size; ++i) {
            if (rd == save_data.end()) {
                Platform::fatal("save data corrupted");
            }

            ((u8*)&encoded_size)[i] = *rd;
            ++rd;
        }

        struct Ctx
        {
            Buffer<char, 1000> compressed;
            Buffer<char, 1000> decompressed;
        };

        auto c = allocate_dynamic<Ctx>("decompression-context");

        for (u32 i = 0; i < encoded_size.get(); ++i) {
            if (rd == save_data.end()) {
                Platform::fatal("save data corrupted");
            }

            c->compressed.push_back(*rd);
            ++rd;
        }

        decompress(c->compressed, c->decompressed);
        if (c->decompressed.size() not_eq sizeof blocks_) {
            Platform::fatal(format("unpacked rle size % does not match %",
                                   c->decompressed.size(),
                                   sizeof blocks_)
                                .c_str());
        }

        auto store = c->decompressed.begin();
        for (u32 i = 0; i < sizeof blocks_; ++i) {
            if (store == c->decompressed.end()) {
                Platform::fatal("error in unpacked data format!");
            }
            ((u8*)&blocks_)[i] = *store;
            ++store;
        }

        return rd;
    }


    void write(Vector<char>& save_data)
    {
        for (u32 i = 0; i < sizeof p_; ++i) {
            save_data.push_back(((u8*)&p_)[i]);
        }

        struct Ctx
        {
            Buffer<char, 1000> block_data;
            Buffer<char, 1000> compressed;
        };

        auto c = allocate_dynamic<Ctx>("compression-context");

        for (u32 i = 0; i < sizeof blocks_; ++i) {
            c->block_data.push_back(((u8*)&blocks_)[i]);
        }

        compress(c->block_data, c->compressed);

        HostInteger<u16> encoded_size;
        encoded_size.set(c->compressed.size());

        for (u32 i = 0; i < sizeof encoded_size; ++i) {
            save_data.push_back(((u8*)&encoded_size)[i]);
        }

        for (char c : c->compressed) {
            save_data.push_back(c);
        }
    }


    Sector()
    {
    }

    Sector(const macro::terrain::Sector& source)
    {
        memcpy(&p_.p_, &source.persistent(), sizeof p_);

        // Better compression ratio by memsetting?
        memset(blocks_.fb_wide_, 0, sizeof blocks_.fb_wide_);

        switch (source.persistent().shape_) {
        case terrain::Sector::Shape::reserved_important_never_use:
        case terrain::Sector::Shape::freebuild_flat:
        case terrain::Sector::Shape::freebuild:
            Platform::fatal("save unimplemented for freebuild sector");
            break;

        case terrain::Sector::Shape::freebuild_wide:
            for (u8 z = 0; z < 6; ++z) {
                for (u8 x = 0; x < 12; ++x) {
                    for (u8 y = 0; y < 12; ++y) {
                        blocks_.fb_wide_[z][x][y] =
                            source.get_block({x, y, z}).type_;
                    }
                }
            }
            break;

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

        case terrain::Sector::Shape::pillar:
            for (u8 z = 0; z < 16; ++z) {
                for (u8 x = 0; x < 6; ++x) {
                    for (u8 y = 0; y < 6; ++y) {
                        blocks_.pillar_[z][x][y] =
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



void EngineImpl::save()
{
    // Dump any memory associated with the rasterizer. We don't want to run out
    // of memory while saving...
    raster::_db.reset();

    Vector<char> save_data;

    save::Header header;
    memcpy(&header.p_, &data_->persistent_, sizeof data_->persistent_);
    header.num_sectors_ = 1 + data_->other_sectors_.size();

    for (u32 i = 0; i < sizeof header; ++i) {
        save_data.push_back(((u8*)&header)[i]);
    }

    auto store_sector = [&save_data](macro::terrain::Sector& sector) {
        save::Sector<0> out(sector);
        out.write(save_data);
        save_data.push_back(0);
    };

    store_sector(*data_->origin_sector_);

    for (auto& s : data_->other_sectors_) {
        store_sector(*s);
    }

    const int sbr_used = save_data.chunks_used();

    if (not flash_filesystem::store_file_data_binary(save::path, save_data)) {
        info("macro save failed!");
    } else {
        info(format("macro save used % buffers", sbr_used).c_str());
    }
}



macro::terrain::Sector& EngineImpl::sector()
{
    if (data_->current_sector_) {
        return *data_->current_sector_;
    } else {
        Platform::fatal("out of bounds sector access");
    }
}



bool EngineImpl::load()
{
    Vector<char> input;

    if (flash_filesystem::read_file_data_binary(save::path, input)) {
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
            it = s.read(input, it);

            if (dest == nullptr) {
                dest = make_sector({s.p_.p_.x_, s.p_.p_.y_}, s.p_.p_.shape_);
            }

            if (dest == nullptr) {
                info("failed to load sector!");
            }

            switch (s.p_.p_.shape_) {
            case terrain::Sector::Shape::cube:
                dest->restore(s.p_.p_, s.blocks_.cube_);
                break;

            case terrain::Sector::Shape::pancake:
                dest->restore(s.p_.p_, s.blocks_.pancake_);
                break;

            case terrain::Sector::Shape::pillar:
                dest->restore(s.p_.p_, s.blocks_.pillar_);
                break;

            case terrain::Sector::Shape::freebuild_wide:
                dest->restore_fb(s.p_.p_, s.blocks_.fb_wide_);
                break;

            case terrain::Sector::Shape::reserved_important_never_use:
            case terrain::Sector::Shape::freebuild_flat:
            case terrain::Sector::Shape::freebuild:
                Platform::fatal("freebuild sector cannot be saved!");
                break;
            }

            [[maybe_unused]] u8 export_count = *(it++);

            dest->shadowcast();
            dest->recalc_stats();
        };

        load_sector(&*data_->origin_sector_);

        data_->other_sectors_.clear();

        for (int i = 0; i < header.num_sectors_ - 1; ++i) {
            load_sector(nullptr);
        }

    } else /* No existing save file */ {

        newgame();
    }

    data_->current_sector_ = &*data_->origin_sector_;
    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;


#ifndef __CMD_MACRO_RAST__
    lisp::ListBuilder conf;

    input.clear();
    if (flash_filesystem::read_file_data_binary(save::timestamp_path, input)) {
        DateTime dt;
        for (u32 i = 0; i < sizeof dt; ++i) {
            ((u8*)&dt)[i] = input[i];
        }

        lisp::ListBuilder fmt;
        fmt.push_back(L_INT(dt.date_.year_));
        fmt.push_back(L_INT(dt.date_.month_));
        fmt.push_back(L_INT(dt.date_.day_));
        fmt.push_back(L_INT(dt.hour_));
        fmt.push_back(L_INT(dt.minute_));
        fmt.push_back(L_INT(dt.second_));

        conf.push_back(L_CONS(lisp::make_symbol("tm"), fmt.result()));
    }

    lisp::set_var("conf", conf.result());


    APP.invoke_script("/scripts/macro/onload.lisp");
#endif


    return true;
}



terrain::Sector* EngineImpl::make_sector(Vec2<s8> coord,
                                         terrain::Sector::Shape shape)
{
    if (load_sector(coord)) {
        return nullptr;
    }

    if (data_->other_sectors_.full()) {
        return nullptr;
    }

    auto s = [&]() -> terrain::Sector* {
        switch (shape) {
        case terrain::Sector::Shape::cube:
            data_->other_sectors_.emplace_back(
                allocate_dynamic<terrain::CubeSector>("sector-mem", coord));
            return &*data_->other_sectors_.back();

        case terrain::Sector::Shape::pancake:
            data_->other_sectors_.emplace_back(
                allocate_dynamic<terrain::PancakeSector>("sector-mem", coord));
            return &*data_->other_sectors_.back();

        case terrain::Sector::Shape::pillar:
            data_->other_sectors_.emplace_back(
                allocate_dynamic<terrain::PillarSector>("sector-mem", coord));
            return &*data_->other_sectors_.back();

        case terrain::Sector::Shape::freebuild_wide:
            data_->other_sectors_.emplace_back(
                allocate_dynamic<terrain::FreebuildWideSector>("sector-mem",
                                                               coord));
            return &*data_->other_sectors_.back();

        case terrain::Sector::Shape::freebuild_flat:
            data_->other_sectors_.emplace_back(
                allocate_dynamic<terrain::FreebuildFlatSector>("sector-mem",
                                                               coord));
            return &*data_->other_sectors_.back();

        case terrain::Sector::Shape::freebuild:
            data_->other_sectors_.emplace_back(
                allocate_dynamic<terrain::FreebuildSector>("sector-mem",
                                                           coord));
            return &*data_->other_sectors_.back();

        case terrain::Sector::Shape::reserved_important_never_use:
            break;
        }

        return nullptr;
    }();
    if (s) {
        StringBuffer<terrain::Sector::name_len - 1> n;
        n += "isle_";
        n += stringify(data_->other_sectors_.size() + 1).c_str();

        s->set_name(n);
        return s;
    } else {
        Platform::fatal(
            format("failed to allocate sector! %", (int)shape).c_str());
    }
    return nullptr;
}



namespace terrain
{
Stats stats(Type t, bool shadowed)
{
    terrain::Stats result;


    switch (t) {
    case terrain::Type::dome:
        break;

    case terrain::Type::building:
        break;

    case terrain::Type::farmhouse:
        break;

    case terrain::Type::workshop:
        break;

    case terrain::Type::granary:
        break;

    case terrain::Type::terrain:
        break;

    case terrain::Type::wheat:
    case terrain::Type::wheat_ripe:
        break;

    case terrain::Type::potatoes:
    case terrain::Type::potatoes_planted:
        break;

    case terrain::Type::sunflowers:
    case terrain::Type::tulips:
        break;

    case terrain::Type::indigo:
        break;

    case terrain::Type::madder:
        break;

    case terrain::Type::shellfish:
        break;

    case terrain::Type::pearls:
        break;

    case terrain::Type::honey:
        break;

    case terrain::Type::cocoa:
        break;

    case terrain::Type::tea:
        break;

    case terrain::Type::lumber:
    case terrain::Type::lumber_spawn:
        break;

    case terrain::Type::rice_terrace:
    case terrain::Type::rice_ripe:
        break;

    case terrain::Type::wool:
        break;

    case terrain::Type::saffron:
        break;

    case terrain::Type::windmill_stone_base:
    case terrain::Type::windmill:
        break;

    case terrain::Type::port:
        break;

    case terrain::Type::marble:
    case terrain::Type::marble_top:
        break;

    case terrain::Type::carved_crystal:
    case terrain::Type::crystal_pillar:
        break;

    case terrain::Type::crystal:
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
    return 5;
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

    case Commodity::pearls:
        return SystemString::block_pearls;

    case Commodity::honey:
        return SystemString::block_honey;

    case Commodity::sunflowers:
        return SystemString::block_sunflower;

    case Commodity::tulips:
        return SystemString::block_tulips;

    case Commodity::wool:
        return SystemString::block_wool;

    case Commodity::food:
        return SystemString::block_food;

    case Commodity::saffron:
        return SystemString::block_saffron;

    case Commodity::cocoa:
        return SystemString::block_cocoa;

    case Commodity::tea:
        return SystemString::block_tea;

    case Commodity::lumber:
        return SystemString::block_lumber;
    }

    return SystemString::empty;
}



SystemString terrain::Commodity::name() const
{
    return terrain::name(type_);
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
    case terrain::Type::wheat_ripe:
    case terrain::Type::potatoes:
    case terrain::Type::potatoes_planted:
    case terrain::Type::madder:
    case terrain::Type::indigo:
    case terrain::Type::sunflowers:
    case terrain::Type::saffron:
    case terrain::Type::cocoa:
    case terrain::Type::tea:
    case terrain::Type::lumber:
    case terrain::Type::lumber_spawn:
    case terrain::Type::tulips:
    case terrain::Type::honey:
    case terrain::Type::rice_terrace:
    case terrain::Type::rice_ripe:
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

    case terrain::Type::pearls:
    case terrain::Type::shellfish:
        return (Categories)(Categories::crop | Categories::fluid_water);
    }
}



std::pair<terrain::Cost, terrain::Type> terrain::harvest(Type t)
{
    Cost cost;
    Type nt = terrain::Type::air;

    switch (t) {
    case terrain::Type::tulips:
        cost.productivity_ = 2;
        nt = terrain::Type::terrain;
        break;

    case terrain::Type::crops_rotten:
        cost.productivity_ = 6;
        nt = terrain::Type::terrain;
        break;

    case terrain::Type::ice:
        cost.water_ = 10;
        cost.productivity_ = 2;
        break;

    case terrain::Type::volcanic_soil:
    case terrain::Type::terrain:
        cost.stone_ = 10;
        cost.productivity_ = 10;
        break;

    case terrain::Type::dynamite:
        break;

    case terrain::Type::masonry:
    case terrain::Type::arch:
        cost = terrain::cost(t);
        cost.productivity_ = 6;
        break;

    case terrain::Type::carved_basalt:
    case terrain::Type::basalt_brick:
    case terrain::Type::basalt:
        cost.stone_ = 50;
        cost.productivity_ = 60;
        break;

    case terrain::Type::carved_crystal:
    case terrain::Type::crystal_pillar:
    case terrain::Type::crystal:
        cost.crystal_ = 10;
        cost.productivity_ = 320;
        break;

    case terrain::Type::marble:
    case terrain::Type::marble_top:
        cost = terrain::cost(t);
        cost.productivity_ = 100;
        break;

    case terrain::Type::wheat:
        nt = terrain::Type::volcanic_soil;
        cost.productivity_ = 1;
        break;

    case terrain::Type::rice_ripe:
        nt = terrain::Type::rice_terrace;
        cost.productivity_ = 12;
        cost.food_ = 16;
        break;

    case terrain::Type::farmhouse:
        cost = terrain::cost(terrain::Type::farmhouse);
        cost.productivity_ = 0;
        break;

    case terrain::Type::wheat_ripe:
        nt = terrain::Type::volcanic_soil;
        cost.productivity_ = 2;
        cost.food_ = 3;
        break;

    case terrain::Type::potatoes_planted:
        nt = terrain::Type::volcanic_soil;
        cost.productivity_ = 3;
        break;

    case terrain::Type::potatoes:
        nt = terrain::Type::volcanic_soil;
        cost.food_ = 15;
        cost.productivity_ = 10;
        break;

    case terrain::Type::lumber:
        cost.lumber_ = 10;
        cost.productivity_ = 4;
        break;

    case terrain::Type::lumber_spawn:
        cost.productivity_ = 2;
        nt = Type::terrain;
        break;

    default: {
        auto c = terrain::cost(t);
        c.lumber_ /= 4;
        return {c, terrain::Type::air};
    }
    }

    return {cost, nt};
}



terrain::Cost terrain::cost(Type t)
{
    Cost cost;

    switch (t) {
    case terrain::Type::reserved_1:
    case terrain::Type::reserved_2:
    case terrain::Type::food:
    case terrain::Type::__invalid:
    case terrain::Type::checker_red:
    case terrain::Type::checker_black:
    case terrain::Type::checker_red_king:
    case terrain::Type::checker_black_king:
    case terrain::Type::checker_highlight:
        break;

    case terrain::Type::air:
        cost.productivity_ = 0;
        break;

    case terrain::Type::farmhouse:
        break;

    case terrain::Type::granary:
    case terrain::Type::building:
        cost.stone_ = 10;
        cost.lumber_ = 18;
        cost.productivity_ = 10;
        break;

    case terrain::Type::dome:
        cost.stone_ = 16;
        cost.lumber_ = 10;
        cost.productivity_ = 60;
        break;

    case terrain::Type::terrain:
        cost = harvest(t).first;
        break;

    case terrain::Type::basalt:
        cost = harvest(t).first;
        break;

    case terrain::Type::rice_terrace:
        cost.productivity_ = 20;
        cost.water_ = 10;
        break;

    case terrain::Type::rice_ripe:
        break;

    case terrain::Type::carved_hematite:
    case terrain::Type::hematite_pillar:
    case terrain::Type::carved_basalt:
    case terrain::Type::basalt_brick:
    case terrain::Type::carved_stone:
    case terrain::Type::stone_pillar:
    case terrain::Type::carved_crystal:
    case terrain::Type::crystal_pillar:
        cost.productivity_ = 6;
        break;

    case terrain::Type::crops_rotten:
        cost.productivity_ = 8;
        break;

    case terrain::Type::dynamite:
    case terrain::Type::hematite:
    case terrain::Type::masonry:
    case terrain::Type::ocher:
    case terrain::Type::hull:
        cost.stone_ = 6;
        cost.clay_ = 6;
        cost.productivity_ = 8;
        break;

    case terrain::Type::arch:
        cost.stone_ = 5;
        cost.clay_ = 6;
        cost.productivity_ = 20;
        break;

    case terrain::Type::road_ns:
    case terrain::Type::road_we:
    case terrain::Type::road_hub:
        cost.stone_ = 2;
        cost.productivity_ = 30;
        break;

    case terrain::Type::scaffolding:
        cost.stone_ = 6;
        cost.productivity_ = 8;
        break;

    case terrain::Type::sand:
        cost.clay_ = 8;
        cost.productivity_ = 6;
        break;

    case terrain::Type::volcanic_soil:
        cost.productivity_ = 3;
        break;

    case terrain::Type::ice:
        cost.productivity_ = 4;
        break;

    case terrain::Type::shrubbery:
        break;

    case terrain::Type::count:
    case terrain::Type::selector:
        break;

    case terrain::Type::water_source:
        cost.water_ = 10;
        cost.productivity_ = 2;
        break;

    case terrain::Type::water_spread_downwards:
    case terrain::Type::water_spread_laterally_a:
    case terrain::Type::water_spread_laterally_b:
    case terrain::Type::water_spread_laterally_c:
    case terrain::Type::water_spread_laterally_d:
    case terrain::Type::water_slant_a:
    case terrain::Type::water_slant_b:
    case terrain::Type::water_slant_c:
    case terrain::Type::water_slant_d:
        break;

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
        cost.productivity_ = 80;
        break;

    case terrain::Type::wheat_ripe:
    case terrain::Type::wheat:
        cost.productivity_ = 2;
        break;

    case terrain::Type::potatoes:
    case terrain::Type::potatoes_planted:
        cost.productivity_ = 4;
        break;

    case terrain::Type::sunflowers:
    case terrain::Type::tulips:
        cost.productivity_ = 2;
        break;

    case terrain::Type::indigo:
    case terrain::Type::pearls:
    case terrain::Type::honey:
    case terrain::Type::shellfish:
    case terrain::Type::wool:
    case terrain::Type::saffron:
    case terrain::Type::madder:
    case terrain::Type::tea:
    case terrain::Type::cocoa:
        cost.productivity_ = 20;
        break;

    case terrain::Type::singularity:
        break;

    case terrain::Type::gold:
        break;

    case terrain::Type::crystal:
        cost.crystal_ = 10;
        cost.productivity_ = 4;
        break;

    case terrain::Type::marble:
    case terrain::Type::marble_top:
        cost.crystal_ = 2;
        cost.stone_ = 20;
        cost.productivity_ = 16;
        break;

    case terrain::Type::workshop:
        break;

    case terrain::Type::light_source:
        cost.productivity_ = 6;
        cost.stone_ = 5;
        cost.lumber_ = 4;
        cost.crystal_ = 10;
        break;

    case terrain::Type::windmill_stone_base:
        cost.stone_ = 14;
        cost.productivity_ = 6;
        break;

    case terrain::Type::windmill:
        cost.stone_ = 14;
        cost.productivity_ = 6;
        break;

    case terrain::Type::port:
        break;

    case terrain::Type::lumber:
        cost.productivity_ = 6;
        break;

    case terrain::Type::lumber_spawn:
        cost.productivity_ = 8;
        break;
    }

    return cost;
}



SystemString terrain::name(Type t)
{
    switch (t) {
    case terrain::Type::reserved_1:
    case terrain::Type::reserved_2:
    case terrain::Type::__invalid:
        break;

    case terrain::Type::crops_rotten:
        return SystemString::block_crops_rotten;

    case terrain::Type::checker_red:
    case terrain::Type::checker_red_king:
        return SystemString::red;

    case terrain::Type::checker_black:
    case terrain::Type::checker_black_king:
        return SystemString::black;

    case terrain::Type::singularity:
        return SystemString::block_singularity;

    case terrain::Type::checker_highlight:
    case terrain::Type::air:
        return SystemString::block_air;

    case terrain::Type::building:
        return SystemString::block_building;

    case terrain::Type::farmhouse:
        return SystemString::block_farmhouse;

    case terrain::Type::dome:
        return SystemString::block_dome;

    case terrain::Type::terrain:
        return SystemString::block_terrain;

    case terrain::Type::stone_pillar:
        return SystemString::block_stone_pillar;

    case terrain::Type::ocher:
        return SystemString::block_ocher;

    case terrain::Type::dynamite:
        return SystemString::block_dynamite_1;

    case terrain::Type::masonry:
        return SystemString::block_masonry;

    case terrain::Type::carved_stone:
        return SystemString::block_carved_masonry;

    case terrain::Type::hull:
        return SystemString::block_hull;

    case terrain::Type::road_hub:
        return SystemString::block_road_hub;

    case terrain::Type::road_ns:
        if (_bound_state->sector().orientation() ==
                terrain::Sector::Orientation::north or
            _bound_state->sector().orientation() ==
                terrain::Sector::Orientation::south) {
            return SystemString::block_road_ns;
        } else {
            return SystemString::block_road_we;
        }


    case terrain::Type::road_we:
        if (_bound_state->sector().orientation() ==
                terrain::Sector::Orientation::north or
            _bound_state->sector().orientation() ==
                terrain::Sector::Orientation::south) {
            return SystemString::block_road_we;
        } else {
            return SystemString::block_road_ns;
        }

    case terrain::Type::scaffolding:
        return SystemString::block_scaffolding;

    case terrain::Type::carved_hematite:
        return SystemString::block_carved_hematite;

    case terrain::Type::hematite_pillar:
        return SystemString::block_hematite_pillar;

    case terrain::Type::hematite:
        return SystemString::block_hematite;

    case terrain::Type::sand:
        return SystemString::block_sand;

    case terrain::Type::arch:
        return SystemString::block_arch;

    case terrain::Type::carved_basalt:
        return SystemString::block_basalt_carved;

    case terrain::Type::basalt_brick:
        return SystemString::block_basalt_brick;

    case terrain::Type::basalt:
        return SystemString::block_basalt;

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

    case terrain::Type::wheat_ripe:
    case terrain::Type::wheat:
        return SystemString::block_wheat;

    case terrain::Type::rice_terrace:
    case terrain::Type::rice_ripe:
        return SystemString::block_rice;

    case terrain::Type::potatoes:
    case terrain::Type::potatoes_planted:
        return SystemString::block_potatoes;

    case terrain::Type::sunflowers:
        return SystemString::block_sunflower;

    case terrain::Type::tulips:
        return SystemString::block_tulips;

    case terrain::Type::indigo:
        return SystemString::block_indigo;

    case terrain::Type::madder:
        return SystemString::block_madder;

    case terrain::Type::shellfish:
        return SystemString::block_shellfish;

    case terrain::Type::pearls:
        return SystemString::block_pearls;

    case terrain::Type::honey:
        return SystemString::block_honey;

    case terrain::Type::cocoa:
        return SystemString::block_cocoa;

    case terrain::Type::tea:
        return SystemString::block_tea;

    case terrain::Type::lumber:
    case terrain::Type::lumber_spawn:
        return SystemString::block_lumber;

    case terrain::Type::wool:
        return SystemString::block_wool;

    case terrain::Type::saffron:
        return SystemString::block_saffron;

    case terrain::Type::gold:
        return SystemString::block_gold;

    case terrain::Type::carved_crystal:
        return SystemString::block_carved_crystal;

    case terrain::Type::crystal_pillar:
        return SystemString::block_crystal_pillar;

    case terrain::Type::crystal:
        return SystemString::block_crystal;

    case terrain::Type::marble:
    case terrain::Type::marble_top:
        return SystemString::block_marble;

    case terrain::Type::granary:
        return SystemString::block_granary;

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
        result.push_back(Type::potatoes_planted);
        result.push_back(Type::lumber_spawn);
        // result.push_back(Type::sunflowers);
        result.push_back(Type::tulips);
        // result.push_back(Type::wool);
        remove_self();
    };

    switch (t) {
    case Type::volcanic_soil:
    case Type::wheat:
    case Type::wheat_ripe:
    case Type::potatoes:
    case Type::potatoes_planted:
    case Type::sunflowers:
    case Type::indigo:
    case Type::madder:
    case Type::wool:
    case Type::tulips:
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
        break;

    case Type::masonry:
        result.push_back(Type::carved_stone);
        result.push_back(Type::stone_pillar);
        result.push_back(Type::road_ns);
        result.push_back(Type::road_we);
        result.push_back(Type::road_hub);
        break;

    case Type::hematite:
        result.push_back(Type::carved_hematite);
        result.push_back(Type::hematite_pillar);
        break;

    case Type::basalt:
        result.push_back(Type::rice_terrace);
        result.push_back(Type::carved_basalt);
        result.push_back(Type::basalt_brick);
        break;

    case Type::crystal:
        result.push_back(Type::carved_crystal);
        result.push_back(Type::crystal_pillar);
        break;

    default:
        break;
    }

    return result;
}



std::pair<int, int> terrain::icons(Type t)
{
    switch (t) {
    case terrain::Type::reserved_1:
    case terrain::Type::reserved_2:
    case terrain::Type::checker_highlight:
    case terrain::Type::checker_red:
    case terrain::Type::checker_black:
    case terrain::Type::checker_red_king:
    case terrain::Type::checker_black_king:
    case terrain::Type::singularity:
    case terrain::Type::air:
        return {2488, 2504};

    case terrain::Type::building:
        return {2760, 2776};

    case terrain::Type::farmhouse:
        return {3928, 3912};

    case terrain::Type::dome:
        return {3640, 3624};

    case terrain::Type::terrain:
        return {2632, 2648};

    case terrain::Type::ocher:
        return {3512, 3528};

    case terrain::Type::masonry:
        return {1448, 1464};

    case terrain::Type::dynamite:
        return {1672, 1688};

    case terrain::Type::hull:
        return {1624, 1640};

    case terrain::Type::scaffolding:
        return {3144, 3160};

    case terrain::Type::hematite:
        return {3448, 3464};

    case terrain::Type::sand:
        return {3048, 3064};

    case terrain::Type::arch:
        return {1544, 1560};

    case terrain::Type::cocoa:
        return {1864, 1880};

    case terrain::Type::ice:
        return {2344, 2360};

    case terrain::Type::shrubbery:
        return {1416, 1432};

    case terrain::Type::carved_hematite:
    case terrain::Type::hematite_pillar:
    case terrain::Type::stone_pillar:
    case terrain::Type::carved_crystal:
    case terrain::Type::crystal_pillar:
    case terrain::Type::basalt_brick:
    case terrain::Type::carved_basalt:
    case terrain::Type::carved_stone:
        return {3384, 3400};

    case terrain::Type::basalt:
        return {3480, 3496};

    case terrain::Type::volcanic_soil:
    case terrain::Type::count:
    case terrain::Type::__invalid:
    case terrain::Type::selector:
        return {};

    case terrain::Type::shellfish:
        return {2824, 2840};

    case terrain::Type::pearls:
        return {3208, 3224};

    case terrain::Type::honey:
        return {3272, 3288};

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

    case terrain::Type::potatoes_planted:
    case terrain::Type::potatoes:
        return {2856, 2872};

    case terrain::Type::sunflowers:
        return {1896, 1912};

    case terrain::Type::tulips:
        return {3176, 3192};

    case terrain::Type::wheat_ripe:
    case terrain::Type::wheat:
        return {2728, 2744};

    case terrain::Type::rice_terrace:
    case terrain::Type::rice_ripe:
        return {3864, 3848};

    case terrain::Type::indigo:
        return {2696, 2712};

    case terrain::Type::madder:
        return {2664, 2680};

    case terrain::Type::gold:
        return {2440, 2456};

    case terrain::Type::granary:
        return {3896, 3880};

    case terrain::Type::workshop:
        return {776, 760};

    case terrain::Type::light_source:
        return {2280, 2296};

    case terrain::Type::windmill_stone_base:
    case terrain::Type::windmill:
        return {2792, 2808};

    case terrain::Type::port:
        return {1512, 1528};

    case terrain::Type::road_ns:
    case terrain::Type::road_hub:
    case terrain::Type::road_we:
        return {776, 760};

    case terrain::Type::crops_rotten:
    case terrain::Type::food:
        return {2888, 2904};

    case terrain::Type::tea:
        return {2984, 3000};

    case terrain::Type::lumber:
    case terrain::Type::lumber_spawn:
        return {3016, 3032};

    case terrain::Type::crystal:
        return {3080, 3096};

    case terrain::Type::marble_top:
    case terrain::Type::marble:
        return {3112, 3128};
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
            raster::globalstate::_changed = true;
            s.on_block_changed(position);
            return true;
        } else if (above.type() not_eq terrain::Type::selector and
                   above.type() not_eq terrain::Type::air) {
            block.type_ = (u8)revert_to;
            raster::globalstate::_changed = true;
            s.on_block_changed(position);
            return true;
        }
    }

    return false;
}



static bool destroyed_by_lava(terrain::Type t)
{
    return t == terrain::Type::building or t == terrain::Type::port or
           t == terrain::Type::shrubbery or t == terrain::Type::ice or
           t == terrain::Type::workshop or t == terrain::Type::dome or
           t == terrain::Type::granary or t == terrain::Type::farmhouse or
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
        s.set_block(beneath_coord, terrain::Type::basalt);
    } else if (tp == terrain::Type::air or tp == terrain::Type::lumber) {
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
        s.set_block(target, terrain::Type::basalt);
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
        s.set_block(beneath_coord, terrain::Type::basalt);
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



bool harvest_block(macro::EngineImpl& state, terrain::Sector& s, Vec3<u8> c)
{
    auto t = s.get_block(c).type();
    auto [cost, nt] = harvest(t);
    if (not state.data_->freebuild_mode_ and
        cost.productivity_ > s.productivity()) {
        return false;
    }
    auto prod = s.productivity();
    prod -= cost.productivity_;
    s.set_productivity(prod);
    auto& p = state.data_->p();
    p.stone_.set(std::min(int(p.stone_.get() + cost.stone_), 99));
    p.lumber_.set(std::min(int(p.lumber_.get() + cost.lumber_), 99));
    p.crystal_.set(std::min(int(p.crystal_.get() + cost.crystal_), 99));
    p.clay_.set(std::min(int(p.clay_.get() + cost.clay_), 99));
    p.water_.set(std::min(int(p.water_.get() + cost.water_), 99));
    p.water_.set(p.water_.get() + cost.water_);
    s.set_food(s.food() + cost.food_);
    if (s.food() > s.food_storage()) {
        s.set_food(s.food_storage());
    }
    s.set_block(c, nt);
    return true;
}



static bool cropcycle_;


void cropcycle(bool on)
{
    cropcycle_ = on;
}



extern u8 screenshake;



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
                raster::globalstate::_changed = true;
                position.z--;
                s.on_block_changed(position);
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
            raster::globalstate::_changed_cursor_flicker_only = true;
        }

    },
    // wheat
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
        if (not block.shadowed_day_ and cropcycle_) {
            block.data_++;
            if (block.data_ > 1) {
                block.data_ = 0;
                s.set_block(position, terrain::Type::wheat_ripe);
            }
        }
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

        if (cropcycle_) {
            block.data_++;
            if (block.data_ > 1) {
                s.set_block(position, terrain::Type::crops_rotten);
            }
        }
    },    // sunflowers
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
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (position.z < terrain::Sector::z_limit) {
            position.z++;
            auto& above = s.get_block(position);
            if (above.type() == terrain::Type::air) {
                block.data_++;
                if (block.data_ > 62) {
                    block.data_ = 0;
                    position.z--;
                    s.set_block(position, terrain::Type::terrain);
                }
            }
        }
    },
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
    // tea
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::volcanic_soil);
    },
    // lumber
    nullptr,
    // basalt,
    nullptr,
    // arch
    nullptr,
    // sand
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (position.z > 0) {
            --position.z;
            if (s.get_block(position).type() == terrain::Type::air) {
                s.set_block(position, terrain::Type::sand);
                ++position.z;
                s.set_block(position, terrain::Type::air);
            }
        }
    },
    // crystal
    nullptr,
    // marble
    nullptr,
    // marble_top
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (position.z < terrain::Sector::z_limit) {
            position.z++;
            auto& above = s.get_block(position);
            if (above.type() == terrain::Type::marble_top or
                above.type() == terrain::Type::marble) {
                block.type_ = (u8)terrain::Type::marble;
                raster::globalstate::_changed = true;
                s.on_block_changed(position);
            }
        }
    },
    // scaffolding
    nullptr,
    // tulips
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // pearls
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not revert_if_covered(s, block, position, terrain::Type::water_source)) {
            update_water_still(s, block, position);
        }
    },
    // road-ns
    nullptr,
    // road-we
    nullptr,
    // honey
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
    // singularity
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (block.data_ > 3) {
            --block.data_;
            return;
        }
        if (block.data_ == 3) {
            s.set_block(position, terrain::Type::air);
            return;
        }

        block.data_++;
        if (block.data_ < 3) {
            return;
        }
        block.data_ = 0;

        auto spread = [&](Vec3<u8> pos) {

                          s.set_block(pos, terrain::Type::singularity);

                      };

        if (position.z > 0) {
            spread({position.x, position.y, u8(position.z - 1)});
        }

        if (position.z < s.size().z - 2) {
            spread({position.x, position.y, u8(position.z + 1)});
        }

        if (position.x < s.size().x - 1) {
            spread({(u8)(position.x + 1), position.y, position.z});
        }

        if (position.y < s.size().y - 1) {
            spread({position.x, (u8)(position.y + 1), position.z});
        }

        if (position.x > 0) {
            spread({(u8)(position.x - 1), position.y, position.z});
        }

        if (position.y > 0) {
            spread({position.x, (u8)(position.y - 1), position.z});
        }

        block.data_ = 24;
    },
    // checker_red
    nullptr,
    // checker_black
    nullptr,
    // checker_sel
    nullptr,
    // checker_red_king
    nullptr,
    // checker_black_king
    nullptr,
    // carved basalt
    nullptr,
    // basalt brick
    nullptr,
    // hull
    nullptr,
    // carved stone
    nullptr,
    // carved crystal
    nullptr,
    // crystal pillar
    nullptr,
    // stone pillar
    nullptr,
    // road hub
    nullptr,
    // hematite
    nullptr,
    // hematite pillar
    nullptr,
    // carved hematite
    nullptr,
    // ocher
    nullptr,
    // dome
    nullptr,
    // potatoes_planted
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (not block.shadowed_day_ and cropcycle_) {
            block.data_++;
            if (block.data_ > 5) {
                block.data_ = 0;
                s.set_block(position, terrain::Type::potatoes);
            }
        }
    },
    // wheat_ripe
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);

        if (cropcycle_) {
            block.data_++;
            if (block.data_ > 1) {
                s.set_block(position, terrain::Type::crops_rotten);
            }
        }
    },
    // lumber_spawn
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);

        if (position.z == s.size().z - 1) {
            s.set_block(position, terrain::Type::terrain);
        }

        if (cropcycle_) {
            block.data_++;
            if (block.data_ > 9) {
                block.data_ = 0;
                s.set_block(position, terrain::Type::volcanic_soil);
                ++position.z;
                s.set_block(position, terrain::Type::lumber);
            }
        }
    },
    // granary
    nullptr,
    // reserved_1
    nullptr,
    // reserved_2
    nullptr,
    // farmhouse
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
    },
    // rice_terrace
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::basalt);

        if (not block.shadowed_day_ and cropcycle_) {
            block.data_++;
            if (block.data_ > 4) {
                block.data_ = 0;
                s.set_block(position, terrain::Type::rice_ripe);
            }
        }
    },
    // rice_ripe
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::basalt);

        if (cropcycle_) {
            block.data_++;
            if (block.data_ > 0) {
                block.data_ = 0;
                s.set_block(position, terrain::Type::rice_terrace);
            }
        }
    },
    // dynamite
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        if (block.data_ > 0) {
            ++block.data_;
        } else {
            if (position.z > 0) {
                --position.z;
                if (s.get_block(position).type() == terrain::Type::air) {
                    s.set_block(position, terrain::Type::dynamite);
                    ++position.z;
                    s.set_block(position, terrain::Type::air);
                } else {
                    ++position.z;
                }
            }
        }

        if (block.data_ == 16) {
            s.set_block(position, terrain::Type::air);
            screenshake = 12;
            Platform::instance().speaker().play_sound("explosion1", 2);

            auto pos = screen_coord(s.project_block(position.x,
                                                    position.y,
                                                    position.z));

            _bound_state->add_entity<MacrocosmEffect>(pos, 8, 13, milliseconds(80));

            for (int z = position.z - 1; z < position.z + 2; ++z) {
                for (int x = position.x - 1; x < position.x + 2; ++x) {
                    for (int y = position.y - 1; y < position.y + 2; ++y) {
                        Vec3<u8> c{(u8)x, (u8)y, (u8)z};
                        if (x > -1 and
                            y > -1 and
                            z > -1 and
                            x < s.size().x and
                            y < s.size().y and
                            z < s.size().z) {

                            if (s.get_block(c).type() == terrain::Type::selector) {

                            } else if (s.get_block(c).type() == terrain::Type::dynamite) {
                                if (s.ref_block(c).data_ == 0) {
                                    s.ref_block(c).data_ = 1;
                                }
                            } else {
                                s.set_block(c, terrain::Type::air);
                            }
                        }
                    }
                }
            }
        }
    },
    // crops (rotten)
    [](terrain::Sector& s, terrain::Block& block, Vec3<u8> position)
    {
        revert_if_covered(s, block, position, terrain::Type::terrain);
    },
};
// clang-format on



namespace terrain
{
bool blocks_light(terrain::Type t)
{
    if (t == terrain::Type::air or t == terrain::Type::selector or
        t == terrain::Type::checker_highlight or t == terrain::Type::crystal or
        t == terrain::Type::carved_crystal or
        t == terrain::Type::crystal_pillar or t == terrain::Type::dome or
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



void terrain::PillarSector::update()
{
    for (int z = 0; z < 16; ++z) {
        for (u8 x = 0; x < 6; ++x) {
            for (u8 y = 0; y < 6; ++y) {

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



void terrain::FreebuildWideSector::update()
{
    for (int z = 0; z < 6; ++z) {
        for (u8 x = 0; x < 12; ++x) {
            for (u8 y = 0; y < 12; ++y) {

                auto& block = blocks_[z][x][y];

                auto update = update_functions[block.type_];
                if (update) {
                    update(*this, block, {x, y, (u8)z});
                }
            }
        }
    }
}



void terrain::FreebuildFlatSector::update()
{
    for (int z = 0; z < 5; ++z) {
        for (u8 x = 0; x < 14; ++x) {
            for (u8 y = 0; y < 14; ++y) {

                auto& block = blocks_[z][x][y];

                auto update = update_functions[block.type_];
                if (update) {
                    update(*this, block, {x, y, (u8)z});
                }
            }
        }
    }
}



void terrain::FreebuildSector::update()
{
    for (int z = 0; z < 9; ++z) {
        for (u8 x = 0; x < 10; ++x) {
            for (u8 y = 0; y < 10; ++y) {

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
                            ((int)terrain::Type::count - 1) * 6 * 2> category =
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

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         ISO_DEFAULT_CGS,
         ISO_DEFAULT_CGS,

         // Arch: similar to a solid block, but the lowest row has some
         // transparency.
         top_angled_l, top_angled_r, opaque, opaque, irregular, irregular,
         top_angled_l, top_angled_r, opaque, opaque, irregular, irregular,

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

         // checkers:
         ISO_SELECTOR_CGS,
         ISO_SELECTOR_CGS,

         ISO_SELECTOR_CGS,
         ISO_SELECTOR_CGS,

         ISO_SELECTOR_CGS,
         ISO_SELECTOR_CGS,

         ISO_SELECTOR_CGS,
         ISO_SELECTOR_CGS,

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

         irregular, irregular, top_angled_l, top_angled_r, bot_angled_l, bot_angled_r,
         irregular, irregular, top_angled_l, top_angled_r, bot_angled_l, bot_angled_r,

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



Vec2<Fixnum> screen_coord(int p)
{
    Vec2<Fixnum> pos;
    int y = p / 30;
    int x = p % 30;
    int real_y = y * 8 + 8;
    int real_x = x * 8;
    pos.y = real_y;
    pos.x = real_x;
    pos.y -= Fixnum::from_integer(PLATFORM.get_scroll(Layer::map_1).y);

    return pos;
}



} // namespace skyland::macro
