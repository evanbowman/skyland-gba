#include "macrocosmSector.hpp"
#include "macrocosmEngine.hpp"



namespace skyland::macro
{



terrain::Sector::Sector(Vec2<s8> position, Shape shape) :
    shape_(shape)
{
    erase();

    p_.x_ = position.x;
    p_.y_ = position.y;

    set_population(4);
}



fiscal::Ledger terrain::Sector::budget() const
{
    fiscal::Ledger result;

    auto st = stats();

    int productive_population = population();
    int unproductive_population = 0;

    if (st.housing_ < population()) {
        // Homeless people are less economically productive? Sounds cynical, but
        // probably true.
        productive_population = st.housing_;
        unproductive_population = population() - st.housing_;
    }

    int employed_population = productive_population;
    int unemployed_population = 0;
    if (st.employment_ < employed_population) {
        unemployed_population = productive_population - st.employment_;
        employed_population = productive_population - unemployed_population;
    }

    auto& pfrm = Platform::instance();

    if (employed_population) {
        result.add_entry(SYSTR(macro_fiscal_employed)->c_str(),
                         employed_population * 0.3f);
    }

    if (unemployed_population) {
        result.add_entry(SYSTR(macro_fiscal_unemployed)->c_str(),
                         unemployed_population * 0.1f);
    }

    if (unproductive_population) {
        result.add_entry(SYSTR(macro_fiscal_homelessness)->c_str(),
                         -unproductive_population * 0.4f);
    }

    if (st.food_ < population() / food_consumption_factor) {
        result.add_entry(
            SYSTR(macro_fiscal_starvation)->c_str(),
            -0.4f * (population() - st.food_ * food_consumption_factor));
    }


    for (auto& c : st.commodities_) {

        if (c.type_ == Commodity::Type::food) {
            Platform::fatal("special case food commodity, unsupported in "
                            "commodity list");
        }

        int count = 0;
        Float accum = 0;
        Float value = c.value(c.type_);
        for (int i = 0; i < c.supply_; ++i) {
            accum += value;
            value *= 0.85f; // Diminishing returns
            ++count;
        }

        fiscal::LineItem::Label l;
        l += loadstr(pfrm, c.name())->c_str();
        l += " (";
        l += stringify(count);
        l += ")";

        result.add_entry(l, accum);
    }

    return result;
}



u16 terrain::Sector::quantity_non_exported(Commodity::Type t)
{
    auto s = base_stats();
    int total = 0;

    if (t == Commodity::Type::food) {
        total = s.food_;
    } else {
        for (auto& c : s.commodities_) {
            if (c.type_ == t) {
                total = c.supply_;
                break;
            }
        }
    }


    for (auto& e : exports()) {
        if (e.c == t) {
            total -= e.export_supply_.get();
        }
    }

    return std::max(0, total);
}



void terrain::Sector::repaint()
{
    for (auto& slab : blocks_) {
        for (auto& slice : slab) {
            for (auto& block : slice) {
                block.repaint_ = true;
            }
        }
    }

    // if (this == _bound_sector) {
    //     sync_to_canvas();
    // }

    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;;
}



Coins terrain::Sector::coin_yield() const
{
    Coins result = 0;

    auto values = budget();
    auto current = values.entries();

    while (current) {
        result += current->contribution_;
        current = current->next_;
    }

    if (result <= 0) {
        result = 1;
    }

    return result;
}



const terrain::Sector::Exports& terrain::Sector::exports() const
{
    return exports_;
}



void terrain::Sector::set_export(const ExportInfo& e)
{
    remove_export(e.source_coord_);

    auto& block = get_block(e.source_coord_);
    if (block.type() not_eq Type::port) {
        return;
    }

    if (not exports_.full()) {
        exports_.emplace_back();
        memcpy(&exports_.back(), &e, sizeof e);
    }
}



void terrain::Sector::remove_export(Vec3<u8> source_coord)
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



void terrain::Sector::set_name(const StringBuffer<name_len - 1>& name)
{
    memset(p_.name_, '\0', name_len);

    auto out = p_.name_;
    for (char c : name) {
        *(out++) = c;
    }
}



StringBuffer<terrain::Sector::name_len - 1> terrain::Sector::name()
{
    auto np = p_.name_;

    StringBuffer<terrain::Sector::name_len - 1> result;

    while (*np not_eq '\0') {
        result.push_back(*(np++));
    }

    return result;
}



terrain::Sector::Population terrain::Sector::population() const
{
    terrain::Sector::Population p;
    memcpy(&p, p_.population_packed_, sizeof p);
    return p;
}



void terrain::Sector::set_population(Population p)
{
    memcpy(p_.population_packed_, &p, sizeof p);
}



Vec2<s8> terrain::Sector::coordinate() const
{
    return {p_.x_, p_.y_};
}



void terrain::Sector::restore(const Persistent& p, u8 blocks[z_limit][8][8])
{
    erase();

    memcpy(&p_, &p, sizeof p);

    for (u8 z = 0; z < macro::terrain::Sector::z_limit; ++z) {
        for (u8 x = 0; x < 8; ++x) {
            for (u8 y = 0; y < 8; ++y) {
                blocks_[z][x][y].type_ = blocks[z][x][y];
                blocks_[z][x][y].repaint_ = true;
                blocks_[z][x][y].data_ = 0;
            }
        }
    }
}



void terrain::Sector::erase()
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



void terrain::Sector::advance(int years)
{
    set_population(population() + population_growth_rate() * years);
}



void add_supply(terrain::Stats& s, terrain::Commodity::Type t, int supply)
{
    if (t == terrain::Commodity::Type::food) {
        s.food_ += supply;
        return;
    }

    for (auto& c : s.commodities_) {
        if (c.type_ == t) {
            c.supply_ += supply;
            return;
        }
    }

    s.commodities_.push_back({t, (u16)supply, false});
}



int remove_supply(terrain::Stats& s, terrain::Commodity::Type t, int supply)
{
    if (t == terrain::Commodity::Type::food) {
        auto remove_amount = std::min((int)s.food_, supply);
        s.food_ -= remove_amount;
        s.food_exports_ += remove_amount;
        return remove_amount;
    }

    for (auto& c : s.commodities_) {
        if (c.type_ == t) {
            auto remove_amount = std::min((int)c.supply_, supply);
            c.supply_ -= remove_amount;
            if (c.supply_ == 0) {
                s.commodities_.erase(&c);
            }
            return remove_amount;
        }
    }

    return 0;
}



static void intersector_exchange_commodities(const Vec2<s8> source_sector,
                                             terrain::Stats& stat)
{
    Buffer<std::pair<const Vec2<s8>, terrain::Stats>, State::max_sectors - 1>
        stats;

    State& state = *_bound_state;

    if (state.data_->origin_sector_.coordinate() == source_sector) {
        stats.push_back({source_sector, stat});
    } else {
        stats.push_back({state.data_->origin_sector_.coordinate(),
                         state.data_->origin_sector_.base_stats()});
    }

    for (auto& sector : state.data_->other_sectors_) {
        if (sector->coordinate() == source_sector) {
            stats.push_back({source_sector, stat});
        } else {
            stats.push_back({sector->coordinate(), sector->base_stats()});
        }
    }


    for (auto& st : stats) {
        if (auto src_sector = state.load_sector(st.first)) {
            for (auto& exp : src_sector->exports()) {

                auto supply = exp.export_supply_.get();

                auto exported = remove_supply(st.second, exp.c, supply);

                for (auto& target : stats) {
                    if (target.first == exp.destination_) {
                        add_supply(target.second, exp.c, exported);
                        break;
                    }
                }
            }
        }
    }


    for (auto& data : stats) {
        if (data.first == source_sector) {
            stat = data.second;
            return;
        }
    }
}



terrain::Stats terrain::Sector::stats() const
{
    auto result = base_stats();

    intersector_exchange_commodities(coordinate(), result);

    return result;
}


terrain::Stats terrain::Sector::base_stats() const
{
    if (base_stats_cache_) {
        return *base_stats_cache_;
    }

    terrain::Stats result;

    for (int z = 0; z < z_limit - 1; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
                if (blocks_[z][x][y].type() == Type::air) {
                    continue;
                }

                auto block_stats = blocks_[z][x][y].stats();

                // NOTE: if a block is covered, then it's not possible to
                // harvest the supplied food, so a stacked block should yield
                // zero food.
                if (blocks_[z + 1][x][y].type() == Type::air or
                    blocks_[z + 1][x][y].type() == Type::selector) {
                    result.food_ += block_stats.food_;
                }

                result.employment_ += block_stats.employment_;

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

    base_stats_cache_ = result;

    return result;
}



Float terrain::Sector::population_growth_rate() const
{
    auto s = stats();

    auto required_food = population() / food_consumption_factor;

    Float result = 0.f;


    if (s.food_ >= required_food) {
        result = 0.1f * (s.food_ - required_food);
    } else {
        result = -0.5f * (required_food - s.food_);
    }

    if (population() > s.housing_) {
        result -= 0.025f * (population() - s.housing_);
    } else {
        result += 0.025f * (s.housing_ - population());
    }


    return result;
}



static Vec3<u8> rotate_coord(Vec3<u8> input)
{
    return {(u8)((8 - 1) - input.y), input.x, input.z};
}



void terrain::Sector::rotate()
{
    // NOTE: I decided to implement rotation by actually rotating the level's
    // blocks and fixing up the coordinates. Simplifies the renderer,
    // basically. Somewhat, costly, but an infrequent operation, unlike
    // rendering.

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < 8 / 2; x++) {
            for (int y = x; y < 8 - x - 1; y++) {
                auto temp = blocks_[z][x][y];
                temp.repaint_ = true;
                blocks_[z][x][y] = blocks_[z][y][8 - 1 - x];
                blocks_[z][y][8 - 1 - x] = blocks_[z][8 - 1 - x][8 - 1 - y];
                blocks_[z][8 - 1 - x][8 - 1 - y] = blocks_[z][8 - 1 - y][x];
                blocks_[z][8 - 1 - y][x] = temp;
            }
        }
    }

    for (auto& exp : exports_) {
        exp.source_coord_ = rotate_coord(exp.source_coord_);
    }

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
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



u16 terrain::Sector::cursor_raster_pos() const
{
    int min = 9999;
    for (auto p : raster::globalstate::_cursor_raster_tiles) {
        if (p < min) {
            min = p;
        }
    }

    return min;
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

    for (int z = 0; z < z_limit; ++z) {
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
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
                        if (xx < 8) {
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
                        if (yy < 8) {
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




const terrain::Block& terrain::Sector::get_block(const Vec3<u8>& coord) const
{
    return blocks_[coord.z][coord.x][coord.y];
}



void terrain::Sector::set_block(const Vec3<u8>& coord, Type type)
{
    auto& selected = blocks_[coord.z][coord.x][coord.y];

    const auto prev_type = selected.type();

    if (prev_type == type) {
        return;
    }

    if (type == Type::selector) {
        selected.data_ = 16;
    } else {
        selected.data_ = 0;
    }

    selected.type_ = (u8)type;
    selected.repaint_ = true;

    if ((prev_type not_eq Type::air and prev_type not_eq Type::selector and
         type == Type::air) or
        (type not_eq Type::selector and type not_eq Type::air)) {
        for (int z = coord.z - 1; z > -1; --z) {
            auto& selected = blocks_[z][coord.x][coord.y];
            if (selected.type_ not_eq 0 and not selected.shadowed_) {
                selected.repaint_ = true;
            }
        }
    }

    if (type not_eq Type::air and type not_eq Type::selector) {
        raster::globalstate::_grew = true;
    }

    if (type not_eq Type::selector) {
        clear_cache();
    }


    raster::globalstate::_recast_shadows = true;


    if (prev_type == Type::light_source or type == Type::light_source or
        (type == Type::air and prev_type not_eq Type::selector)) {

        if (type == Type::air) {
            raster::globalstate::_shrunk = true;
        }


        // Lighting pattern changed, or block removed (assigned as air), just
        // redraw everything.

        for (auto& slab : blocks_) {
            for (auto& slice : slab) {
                for (auto& block : slice) {
                    block.repaint_ = true;
                }
            }
        }
    }

    raster::globalstate::_changed = true;
}




void terrain::Sector::set_cursor(const Vec3<u8>& pos, bool lock_to_floor)
{
    if (pos.z >= z_limit or pos.x >= 8 or pos.y >= 8) {
        Platform::fatal("set cursor to out of bounds position");
    }

    auto old_cursor = p_.cursor_;
    auto& block = blocks_[old_cursor.z][old_cursor.x][old_cursor.y];
    if (block.type_ == (u8)terrain::Type::selector) {
        set_block(old_cursor, macro::terrain::Type::air);
    }

    p_.cursor_ = pos;

    if (lock_to_floor) {
        while (blocks_[p_.cursor_.z][p_.cursor_.x][p_.cursor_.y].type() not_eq
               terrain::Type::air) {
            ++p_.cursor_.z;
        }

        raster::globalstate::_cursor_moved = true;

        while (p_.cursor_.z > 0 and
               blocks_[p_.cursor_.z - 1][p_.cursor_.x][p_.cursor_.y].type() ==
                   terrain::Type::air) {
            --p_.cursor_.z;
        }
    }

    if (not(p_.cursor_ == old_cursor)) {
        raster::globalstate::_cursor_moved = true;
    }

    if (p_.cursor_.z >= z_view_) {
        set_z_view(p_.cursor_.z + 1);
        raster::globalstate::_cursor_moved = false;
    }

    set_block(p_.cursor_, terrain::Type::selector);
}



bool terrain::Sector::set_z_view(u8 z_view)
{
    // auto c = cursor();
    // if (z_view <= c.z) {
    //     return false;
    // }

    if (z_view > z_limit) {
        z_view_ = z_limit;
        return false;
    } else {
        z_view_ = z_view;
    }

    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;
    for (auto& slab : blocks_) {
        for (auto& slice : slab) {
            for (auto& block : slice) {
                block.repaint_ = true;
            }
        }
    }

    return true;
}



void terrain::Sector::clear_cache()
{
    base_stats_cache_.reset();
}



// Projects isometric geometry into indices in the tilemap.
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
    irregular,
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



namespace raster
{



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



std::optional<DynamicMemory<raster::DepthBuffer>> _db;



} // namespace raster



void terrain::Sector::render_setup(Platform& pfrm)
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



void terrain::Sector::render(Platform& pfrm)
{
    using namespace raster;

    // #define RASTER_DEBUG_ENABLE

#ifdef RASTER_DEBUG_ENABLE
#define RASTER_DEBUG()                                                         \
    do {                                                                       \
        pfrm.sleep(20);                                                        \
    } while (false)
#else
#define RASTER_DEBUG()                                                         \
    do {                                                                       \
    } while (false)
#endif

    auto flush_stack_t0 = [&pfrm](auto& stack, int i) {
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
    };


    auto flush_stack_t1 = [&pfrm](auto& stack, int i) {
        bool overwrite = true;

        while (not stack.empty()) {
            int tile = stack.back();
            RASTER_DEBUG();
            pfrm.blit_t1_tile_to_texture(tile + 480, i, overwrite);
            stack.pop_back();
            overwrite = false;
        }
    };


    if (not globalstate::_changed_cursor_flicker_only and
        not globalstate::_changed) {
        return;
    }

    if (globalstate::_changed_cursor_flicker_only and
        not globalstate::_changed) {

        // Optimized rendering for the flickering cursor. No need to perform
        // depth testing and all that stuff just to repaint the cursor tiles!
        // Use a cached copy.

        for (u32 i = 0; i < globalstate::_cursor_raster_tiles.size(); ++i) {
            auto t = globalstate::_cursor_raster_tiles[i];
            auto& stk = globalstate::_cursor_raster_stack[i];
            u32 j = 0;
            for (; j < stk.size(); ++j) {
                // Flickering implementation, manually offset the tiles between
                // light and dark if part of the cursor tile range.
                if (stk[j] > 59 and stk[j] < 66) {
                    stk[j] += 6;
                    break;
                } else if (stk[j] > 65 and stk[j] < 72) {
                    stk[j] -= 6;
                    break;
                }
            }
            if (j == stk.size()) {
                // The rendering stack does not contain the cursor tile in the
                // first place. Skip.
                continue;
            } else if (j == 0 and not stk.empty()) {
                // If the cursor tile sits at the bottom of the rendering stack,
                // it will be drawn last. Therefore, we don't need to worry
                // about overlapping tiles on top of the cursor, and can just
                // draw overtop of what's already in the frame buffer. NOTE:
                // this only works because the two cursor frames occupy the same
                // pixels.
                RASTER_DEBUG();
                if (t >= 480) {
                    pfrm.blit_t1_tile_to_texture(stk[0] + 480, t - 480, false);
                } else {
                    pfrm.blit_t0_tile_to_texture(stk[0] + 480, t, false);
                }
            } else {
                auto stk_cpy = globalstate::_cursor_raster_stack[i];
                if (t >= 480) {
                    flush_stack_t1(stk_cpy, t - 480);
                } else {
                    flush_stack_t0(stk_cpy, t);
                }
            }
        }

        globalstate::_changed_cursor_flicker_only = false;
        return;
    }

    const bool cursor_moved = globalstate::_cursor_moved;
    const bool shrunk = globalstate::_shrunk;
    const bool grew = globalstate::_grew;


    render_setup(pfrm);

    [[maybe_unused]] auto start = pfrm.delta_clock().sample();

    for (int i = 0; i < 480; ++i) {

        if (auto head = (*_db)->depth_1_->visible_[i]) {

            const bool redraw =
                grew or
                (not cursor_moved or
                 (cursor_moved and (*_db)->depth_1_cursor_redraw.get(i)));

            if (redraw) {
                Buffer<int, 6> stack;
                while (head) {
                    stack.push_back(head->tile_);
                    head = head->next_;
                }

                flush_stack_t0(stack, i);
            }

        } else if ((cursor_moved or shrunk) and
                   not(*_db)->depth_1_skip_clear.get(i)) {
            pfrm.blit_t0_erase(i);
        }

        if (auto head = (*_db)->depth_2_->visible_[i]) {

            const bool redraw =
                grew or
                (not cursor_moved or
                 (cursor_moved and (*_db)->depth_2_cursor_redraw.get(i)));

            if (redraw) {
                Buffer<int, 6> stack;
                while (head) {
                    stack.push_back(head->tile_);
                    head = head->next_;
                }

                flush_stack_t1(stack, i);
            }


        } else if ((cursor_moved or shrunk) and
                   not(*_db)->depth_2_skip_clear.get(i)) {
            pfrm.blit_t1_erase(i);
        }
    }

    [[maybe_unused]] auto stop = pfrm.delta_clock().sample();
    // pfrm.fatal(stringify(stop - start).c_str());


    if (globalstate::_cursor_moved) {
        // Handle these out of line, as not to slow down the main rendering
        // block.
        for (int i = 0; i < 480; ++i) {
            if (cursor_moved and (*_db)->depth_1_empty.get(i)) {
                pfrm.blit_t0_erase(i);
            }
            if (cursor_moved and (*_db)->depth_2_empty.get(i)) {
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

    globalstate::_changed = false;
    globalstate::_shrunk = false;
    globalstate::_grew = false;
    globalstate::_cursor_moved = false;
    globalstate::_changed_cursor_flicker_only = false;

    _db.reset();
}



}
