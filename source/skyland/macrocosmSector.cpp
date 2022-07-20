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


#include "macrocosmSector.hpp"
#include "base32.hpp"
#include "compression.hpp"
#include "macrocosmEngine.hpp"
#include "rle.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



terrain::Sector::Sector(Vec2<s8> position, Shape shape, Vec3<u8> size)
    : size_(size)
{
    p_.shape_ = shape;

    set_name("");

    p_.x_ = position.x;
    p_.y_ = position.y;

    set_population(4);
}



terrain::Sector::Happiness terrain::Sector::get_happiness() const
{
    auto ledger = annotate_happiness(true);

    auto ent = ledger.entries();

    Happiness result = 0.f;

    while (ent) {
        result += ent->contribution_;
        ent = ent->next_;
    }

    return result;
}



fiscal::Ledger terrain::Sector::annotate_happiness(bool skip_labels) const
{
    auto stat = stats();



    const auto food_avail =
        stat.food_ - population() / EngineImpl::food_consumption_factor();

    const auto housing_avail = stat.housing_ - population();

    int commodity_supply = 0;
    for (auto& commodity : stat.commodities_) {
        commodity_supply += commodity.supply_;
    }

    auto& pfrm = Platform::instance();

    fiscal::Ledger result;

    auto add_entry = [&](SystemString str, float value) {
        if (skip_labels) {
            result.add_entry("", value);
        } else {
            result.add_entry(loadstr(pfrm, str)->c_str(), value);
        }
    };

    add_entry(SystemString::category_misc, stat.happiness_);
    add_entry(SystemString::macro_commodities, 2 * commodity_supply);
    add_entry(SystemString::macro_food_supply, 0.3f * food_avail);
    add_entry(SystemString::macro_housing_scarcity, 0.2f * housing_avail);
    add_entry(SystemString::macro_population_density, population() * -0.1f);

    return result;
}



fiscal::Ledger terrain::Sector::budget(bool skip_labels) const
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

    auto add_entry = [&](SystemString str, float value) {
        if (skip_labels) {
            result.add_entry("", value);
        } else {
            result.add_entry(loadstr(pfrm, str)->c_str(), value);
        }
    };

    if (employed_population) {
        add_entry(SystemString::macro_fiscal_employed,
                  employed_population * 0.01f *
                      EngineImpl::bindings().mcr_employment_yield_percent);
    }

    if (unemployed_population) {
        add_entry(SystemString::macro_fiscal_unemployed,
                  unemployed_population * 0.01f *
                      EngineImpl::bindings().mcr_jobless_yield_percent);
    }

    if (unproductive_population) {
        add_entry(SystemString::macro_fiscal_homelessness,
                  -unproductive_population * 0.01f *
                      EngineImpl::bindings().mcr_homelessness_penalty_percent);
    }

    auto f = EngineImpl::food_consumption_factor();
    if (st.food_ < population() / f) {
        add_entry(SystemString::macro_fiscal_starvation,
                  -0.01f *
                      EngineImpl::bindings().mcr_starvation_penalty_percent *
                      (population() - st.food_ * f));
    }

    auto happiness = get_happiness();
    if (happiness < 0) {
        add_entry(SystemString::macro_fiscal_unhappiness, happiness / 2);
    } else {
        add_entry(SystemString::macro_fiscal_happiness, happiness / 2);
    }


    const auto dp = EngineImpl::commodity_diminishing_return_percent();

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
            value *= dp;
            ++count;
        }

        if (skip_labels) {
            result.add_entry("", accum);
        } else {
            fiscal::LineItem::Label l;
            l += loadstr(pfrm, c.name())->c_str();
            l += " (";
            l += stringify(count);
            l += ")";

            result.add_entry(l, accum);
        }
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


    if (auto exp = exports()) {
        for (auto& e : *exp) {
            if (e.c == t) {
                total -= e.export_supply_.get();
            }
        }
    }


    return std::max(0, total);
}



void terrain::Sector::repaint()
{
    raster::globalstate::_recalc_depth_test.fill();


    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;
}



Coins terrain::Sector::coin_yield() const
{
    Coins result = 0;

    const bool skip_labels = true;
    auto values = budget(skip_labels);
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

    s.commodities_.push_back({t, false, (u16)supply});
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
    Buffer<std::pair<const Vec2<s8>, terrain::Stats>,
           EngineImpl::max_sectors - 1>
        stats;

    EngineImpl& state = *_bound_state;

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
            if (auto e = src_sector->exports()) {
                for (auto& exp : *e) {

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
    if (auto s = base_stats_cache_load()) {
        return *s;
    }

    terrain::Stats result;

    for (int z = 0; z < size_.z - 1; ++z) {
        for (int x = 0; x < size_.x; ++x) {
            for (int y = 0; y < size_.y; ++y) {
                if (get_block({(u8)x, (u8)y, (u8)z}).type() == Type::air) {
                    continue;
                }

                auto block_stats = get_block({(u8)x, (u8)y, (u8)z}).stats();

                // NOTE: if a block is covered, then it's not possible to
                // harvest the supplied food, so a stacked block should yield
                // zero food.
                if (get_block({(u8)x, (u8)y, (u8)(z + 1)}).type() ==
                        Type::air or
                    get_block({(u8)x, (u8)y, (u8)(z + 1)}).type() ==
                        Type::selector) {
                    result.food_ += block_stats.food_;
                }

                result.employment_ += block_stats.employment_;
                result.happiness_ += block_stats.happiness_;

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

    base_stats_cache_store(result);

    return result;
}



Float terrain::Sector::population_growth_rate_from_food_supply() const
{
    auto s = stats();
    auto& b = EngineImpl::bindings();

    auto required_food = population() / EngineImpl::food_consumption_factor();


    if (s.food_ >= required_food) {
        return 0.01f * b.mcr_pop_growth_food_surplus_percent *
                 (s.food_ - required_food);
    } else {
        return -0.01f * b.mcr_pop_growth_food_shortage_percent *
                 (required_food - s.food_);
    }
}



Float terrain::Sector::population_growth_rate_from_housing_supply() const
{
    auto s = stats();
    auto& b = EngineImpl::bindings();

    if (population() > s.housing_) {
        return -0.001f * b.mcr_pop_growth_housing_factor *
            (population() - s.housing_);
    } else {
        return 0.001f * b.mcr_pop_growth_housing_factor *
            (s.housing_ - population());
    }
}



Float terrain::Sector::population_growth_rate() const
{
    return population_growth_rate_from_food_supply() +
        population_growth_rate_from_housing_supply();
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



void terrain::Sector::set_block(const Vec3<u8>& coord, Type type)
{
    auto& selected = ref_block({(u8)coord.x, (u8)coord.y, (u8)coord.z});

    const auto prev_type = selected.type();

    if (prev_type == type) {
        return;
    }

    if (type not_eq Type::selector) {
        // We do an optimization for redrawing the cursor as it flickers. But
        // doing so skips redrawing any other changes on the frames that the
        // cursor changes, so we need to cancel the cursor optimizations if
        // another block is created at the same time the the cursor
        // changes. Mainly relevant for fluid blocks, which continue to flow
        // into new spaces for a bit after they're created.

        if (raster::globalstate::_changed_cursor_flicker_only) {
            raster::globalstate::_changed_cursor_flicker_only = false;
            on_block_changed(p_.cursor_);
        }

        if (raster::globalstate::_cursor_moved) {
            raster::globalstate::_cursor_moved = false;
        }
    }

    if (type == Type::selector) {
        selected.data_ = 16;
    } else {
        selected.data_ = 0;
    }

    selected.type_ = (u8)type;

    if ((prev_type not_eq Type::air and prev_type not_eq Type::selector and
         type == Type::air) or
        (type not_eq Type::selector and type not_eq Type::air)) {
        for (int z = coord.z - 1; z > -1; --z) {
            auto& selected = ref_block({coord.x, coord.y, (u8)z});
            if (selected.type_ not_eq 0 and not selected.shadowed_day_) {
                on_block_changed({coord.x, coord.y, (u8)z});
            }
        }
    }

    if (type not_eq Type::air and type not_eq Type::selector) {
        raster::globalstate::_grew = true;
    }

    if (type not_eq Type::selector) {
        base_stats_cache_clear();
    }


    if (not((prev_type == Type::air and type == Type::selector) or
            (prev_type == Type::selector and type == Type::air))) {
        raster::globalstate::_recast_shadows = true;
    }

    if (prev_type == Type::light_source or type == Type::light_source or
        (type == Type::air and prev_type not_eq Type::selector)) {

        if (type == Type::air) {
            raster::globalstate::_shrunk = true;
        }


        // Lighting pattern changed, or block removed (assigned as air), just
        // redraw everything.

        raster::globalstate::_recalc_depth_test.fill();
    }

    raster::globalstate::_changed = true;
    on_block_changed(coord);
}



void terrain::Sector::set_cursor(const Vec3<u8>& pos, bool lock_to_floor)
{
    if (pos.z >= size().z or pos.x >= size().x or pos.y >= size().y) {
        Platform::fatal(format("set cursor to out of bounds position"
                               " % % %, % % %",
                               pos.x,
                               pos.y,
                               pos.z,
                               size().x,
                               size().y,
                               size().z)
                            .c_str());
    }

    auto old_cursor = p_.cursor_;
    auto& block = ref_block(old_cursor);
    if (block.type_ == (u8)terrain::Type::selector) {
        set_block(old_cursor, macro::terrain::Type::air);
    }

    p_.cursor_ = pos;

    auto set_cursor_moved = [&] {
        if (not raster::globalstate::_changed) {
            raster::globalstate::_cursor_moved = true;
        } else /* changed == true */ {
            // Not really ideal, just a hacky short-term solution. I created
            // tons of optimizations to make cursor redraw faster, but then
            // added fluids, which expand into new blocks on their own,
            // requiring the engine to cancel out of the optimized drawing
            // code when another block changed in addition to the cursor
            // block. But when we don't set _cursor_moved, the renderer will
            // not clean up tiles in any empty space that the cursor used-to
            // inhabit, so we set the _shrunk flag to tell the renderer to
            // clean up any tiles where there may no longer be blocks.
            raster::globalstate::_shrunk = true;
        }
    };

    if (lock_to_floor) {
        while (ref_block(p_.cursor_).type() not_eq terrain::Type::air) {
            ++p_.cursor_.z;
        }

        set_cursor_moved();

        while (p_.cursor_.z > 0 and
               ref_block({p_.cursor_.x, p_.cursor_.y, (u8)(p_.cursor_.z - 1)})
                       .type() == terrain::Type::air) {
            --p_.cursor_.z;
        }
    }

    if (not(p_.cursor_ == old_cursor)) {
        set_cursor_moved();
    }

    if (p_.cursor_.z >= z_view_) {
        set_z_view(p_.cursor_.z + 1);
        raster::globalstate::_cursor_moved = false;
    }

    set_block(p_.cursor_, terrain::Type::selector);


    auto& cursor_block = ref_block(p_.cursor_);
    cursor_block.shadowed_ = false;

    if (p_.cursor_.z > 0) {
        auto& beneath =
            get_block({p_.cursor_.x, p_.cursor_.y, u8(p_.cursor_.z - 1)});
        switch (beneath.type()) {
        case Type::ice:
        case Type::masonry:
        case Type::terrain:
        case Type::gold:
        case Type::shrubbery:
        case Type::wool:
        case Type::lumber:
        case Type::arch:
        case Type::sand:
        case Type::crystal:
        case Type::marble:
        case Type::marble_top:
        case Type::port:
            // Use a darker default cursor color if the cursor appears overtop
            // of a lighter block, for best visibility.
            cursor_block.shadowed_ = not beneath.shadowed_;
            break;

        default:
            break;
        }
    }
}



bool terrain::Sector::set_z_view(u8 z_view)
{
    if (z_view > size_.z) {
        z_view_ = size_.z;
        return false;
    } else {
        z_view_ = z_view;
    }

    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;

    raster::globalstate::_recalc_depth_test.fill();


    raster::globalstate::_recalc_depth_test.fill();


    return true;
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
                   raster::globalstate::_recalc_depth_test.get(i)) {
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
                   raster::globalstate::_recalc_depth_test.get(i + 480)) {
            pfrm.blit_t1_erase(i);
        }
    }

    [[maybe_unused]] auto stop = pfrm.delta_clock().sample();
    // pfrm.fatal(stringify(stop - start).c_str());


    globalstate::_changed = false;
    globalstate::_shrunk = false;
    globalstate::_grew = false;
    globalstate::_cursor_moved = false;
    globalstate::_changed_cursor_flicker_only = false;

    _db.reset();
    raster::globalstate::_recalc_depth_test.clear();
}



std::optional<QRCode>
terrain::Sector::qr_encode(Platform& pfrm,
                           App& app,
                           Function<16, void(const char*)> msg) const
{
    Vector<u8> data;

    for (u8 z = 0; z < size().z; ++z) {
        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                data.push_back(get_block({x, y, z}).type_);
            }
        }
    }

    msg("compression... (heatshrink)");

    struct Buffers
    {
        Buffer<char, 920> b1_;
        Buffer<char, 920> b2_;
    };
    auto buffers = allocate_dynamic<Buffers>("enc-input");
    auto contiguous_data = &buffers->b1_;

    contiguous_data->push_back((u8)p_.shape_);
    for (char c : data) {
        contiguous_data->push_back(c);
    }

    auto& compr = buffers->b2_;
    compr = compress(*contiguous_data);
    Vector<char> b32_array;
    for (char c : compr) {
        b32_array.push_back(c);
    }

    msg("fetch upload url...");

    const bool was_developer_mode = app.is_developer_mode();
    app.set_developer_mode(true);
    auto v = app.invoke_script(pfrm, "/scripts/config/uploadisle.lisp");
    if (v->type() not_eq lisp::Value::Type::string) {
        Platform::fatal("url lisp script returned non-string result");
    }
    app.set_developer_mode(was_developer_mode);


    msg("base32 encoding...");

    // Encode as base32, because the data is going into a url
    auto encoded = base32::encode(b32_array);
    auto result = allocate_dynamic<StringBuffer<1900>>("result-str");
    *result = v->string().value();
    (*result) += "?d="; // url parameter

    // the encoded data
    for (char c : encoded) {
        result->push_back(c);
    }

    msg("QR encoding...");

    if (auto qr = QRCode::create(result->c_str())) {
        // NOTE: at resolution 240x160, an 80x80 qrcode is the largest that we
        // can display while still allowing the qrcode to show up onscreen,
        // anything larger and we'd need to represent qr blocks with single
        // pixels, which wouldn't register on most cameras given the gba screen
        // size.
        // if (qr->size() <= 77) {
        return qr;
        // }
    }

    return {};
}



} // namespace skyland::macro
