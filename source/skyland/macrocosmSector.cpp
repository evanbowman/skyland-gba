////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "macrocosmSector.hpp"
#include "base32.hpp"
#include "compression.hpp"
#include "macrocosmEngine.hpp"
#include "rle.hpp"
#include "script/lisp.hpp"
#include "skyland/skyland.hpp"
#define FNL_IMPL
#include "fastNoiseLite/FastNoiseLite.h"


namespace skyland::macro
{



void cropcycle(bool on);



void terrain::Sector::on_day_transition()
{
    set_productivity(population() * Population(10));

    set_food(food() - population());
    if (food() < 0) {
        set_food(0);
        if (population() > 1) {
            set_population(population() - 1);
        }
    } else {
        if (housing() > population()) {
            set_population(population() + 1);
        } else if (housing() < population()) {
            set_population(population() - 1);
        }
    }

    cropcycle(true);

    update();

    cropcycle(false);
}



terrain::Sector::Sector(Vec2<s8> position, Shape shape, Vec3<u8> size)
    : size_(size)
{
    p_.shape_ = shape;

    set_name("");

    p_.x_ = position.x;
    p_.y_ = position.y;

    set_population(1);
}



void terrain::Sector::repaint()
{
    raster::globalstate::_recalc_depth_test.fill();


    raster::globalstate::_changed = true;
    raster::globalstate::_shrunk = true;
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



Productivity terrain::Sector::productivity() const
{
    return p_.productivity_.get();
}



Population terrain::Sector::population() const
{
    return p_.population_.get();
}



void terrain::Sector::set_population(Population p)
{
    p_.population_.set(p);
}



void terrain::Sector::set_productivity(Productivity p)
{
    p_.productivity_.set(p);
}



Vec2<s8> terrain::Sector::coordinate() const
{
    return {p_.x_, p_.y_};
}



void add_supply(terrain::Stats& s, terrain::Commodity::Type t, int supply)
{
}



int remove_supply(terrain::Stats& s, terrain::Commodity::Type t, int supply)
{
    return 0;
}



void terrain::Sector::soft_update(EngineImpl& s)
{
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
        // ...
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

    auto should_recalc = [](Type t) {
        return t == Type::building or t == Type::granary;
    };

    if (should_recalc(type) or should_recalc(prev_type)) {
        recalc_stats();
    }
}



void terrain::Sector::recalc_stats()
{
    housing_ = 0;
    granaries_ = 0;
    for (u8 z = 0; z < size().z; ++z) {
        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                auto tp = get_block({x, y, z}).type();
                if (tp == Type::building) {
                    ++housing_;
                } else if (tp == Type::granary) {
                    ++granaries_;
                }
            }
        }
    }
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



void terrain::Sector::render()
{
    using namespace raster;

    // #define RASTER_DEBUG_ENABLE

#ifdef RASTER_DEBUG_ENABLE
#define RASTER_DEBUG()                                                         \
    do {                                                                       \
        PLATFORM.sleep(10);                                                    \
    } while (false)
#else
#define RASTER_DEBUG()                                                         \
    do {                                                                       \
    } while (false)
#endif

    auto flush_stack_t0 = [](auto& stack, int i) {
        // The first tile can be drawn much faster, as we don't care what's
        // currently onscreen.
        bool overwrite = true;

        while (not stack.empty()) {
            int tile = stack.back();
            RASTER_DEBUG();
            PLATFORM.blit_t0_tile_to_texture(
                tile + RASTER_CELLCOUNT, i, overwrite);
            stack.pop_back();
            overwrite = false;
        }
    };


    const auto uho = macro::raster::globalstate::_upper_half_only;


    auto flush_stack_t1 = [&uho](auto& stack, int i) {
        if (uho) {
            return;
        }


        bool overwrite = true;

        while (not stack.empty()) {
            int tile = stack.back();
            RASTER_DEBUG();
            PLATFORM.blit_t1_tile_to_texture(
                tile + RASTER_CELLCOUNT, i, overwrite);
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
                if (t >= RASTER_CELLCOUNT and not uho) {
                    PLATFORM.blit_t1_tile_to_texture(
                        stk[0] + RASTER_CELLCOUNT, t - 480, false);
                } else {
                    PLATFORM.blit_t0_tile_to_texture(
                        stk[0] + RASTER_CELLCOUNT, t, false);
                }
            } else {
                auto stk_cpy = globalstate::_cursor_raster_stack[i];
                if (t >= RASTER_CELLCOUNT) {
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


    render_setup();

    [[maybe_unused]] auto start = PLATFORM.delta_clock().sample();

    for (int i = 0; i < RASTER_CELLCOUNT; ++i) {

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
            PLATFORM.blit_t0_erase(i);
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
                   raster::globalstate::_recalc_depth_test.get(
                       i + RASTER_CELLCOUNT)) {
            if (not uho) {
                PLATFORM.blit_t1_erase(i);
            }
        }
    }

    [[maybe_unused]] auto stop = PLATFORM.delta_clock().sample();
    // PLATFORM.fatal(stringify(stop - start).c_str());


    globalstate::_changed = false;
    globalstate::_shrunk = false;
    globalstate::_grew = false;
    globalstate::_cursor_moved = false;
    globalstate::_changed_cursor_flicker_only = false;

    _db.reset();
    raster::globalstate::_recalc_depth_test.clear();
}



Optional<QRCode> terrain::Sector::qr_encode(


    Function<4 * sizeof(void*), void(const char*)> msg) const
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


    auto buffer1 = allocate_dynamic<Buffer<char, 1000>>("enc-input");
    auto buffer2 = allocate_dynamic<Buffer<char, 1000>>("enc-input");
    auto contiguous_data = &*buffer1;

    contiguous_data->push_back((u8)p_.shape_);
    for (char c : data) {
        contiguous_data->push_back(c);
    }

    auto& compr = *buffer2;
    compress(*contiguous_data, compr);
    Vector<char> b32_array;
    for (char c : compr) {
        b32_array.push_back(c);
    }

    msg("fetch upload url...");

    const bool was_developer_mode = APP.is_developer_mode();
    APP.set_developer_mode(true);
    auto v = APP.invoke_script("/scripts/config/uploadisle.lisp");
    if (v->type() not_eq lisp::Value::Type::string) {
        Platform::fatal("url lisp script returned non-string result");
    }
    APP.set_developer_mode(was_developer_mode);


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



void terrain::Sector::pack(Vector<char>& result)
{
    Vector<u8> out;

    for (u8 z = 0; z < size().z; ++z) {
        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                out.push_back(get_block({x, y, z}).type_);
            }
        }
    }

    auto encoded = rle::encode(out);
    result.push_back((char)p_.shape_);

    for (u8 val : encoded) {
        result.push_back(val);
    }
}



void terrain::Sector::unpack(Vector<char>& input)
{
    bool skip_first = false;

    Vector<u8> data; // rle::decode does not accept Vector<char>
    for (char c : input) {
        if (not skip_first) {
            // Contains island shape byte, not rle data.
            skip_first = true;
        } else {
            data.push_back(c);
        }
    }

    auto decoded = rle::decode(data);

    auto it = decoded.begin();

    const u32 block_count = size().z * size().x * size().y;

    if (decoded.size() not_eq block_count) {
        return;
    }

    for (u8 z = 0; z < size().z; ++z) {
        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                set_block({x, y, z}, (terrain::Type)*it);

                if ((Type)*it == Type::selector) {
                    p_.cursor_ = {x, y, z};
                }

                ++it;
            }
        }
    }

    repaint();

    shadowcast();
}



void terrain::Sector::generate_terrain(int min_blocks, int building_count)
{
    switch (rng::choice<6>(rng::critical_state)) {
    case 0:
    case 1:
        generate_terrain_regular(min_blocks, building_count);
        break;

    case 2:
    case 3:
        generate_terrain_desert(min_blocks, building_count);
        break;

    case 4:
        generate_terrain_tundra(min_blocks, building_count);
        break;

    case 5:
        generate_terrain_molten(min_blocks, building_count);
        break;
    }
}



void terrain::Sector::generate_terrain_origin(int min_blocks)
{
RETRY:

    int count = 0;

    auto gen = [&](int height_scale, Float freq) {
        Float data[16][16];

        fnl_state noise = fnlCreateState(rng::get(rng::critical_state), freq);
        noise.noise_type = FNL_NOISE_OPENSIMPLEX2;

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                data[x][y] = fnlGetNoise2D(&noise, x, y);
            }
        }

        u8 snowline = (size().z - 1) * 0.7f;
        if (p_.shape_ == Shape::pillar) {
            snowline = 10;
        }

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                u8 height = height_scale * data[x][y];
                bool first = true;
                for (int z = height - 1; z > -1; --z) {
                    terrain::Type t = terrain::Type::basalt;

                    if (first) {
                        if (z > snowline) {
                            t = terrain::Type::ice;
                        } else if (z == 0) {
                            t = terrain::Type::sand;
                        } else if (z < snowline) {
                            t = terrain::Type::terrain;
                        }
                        first = false;
                    }

                    set_block({(u8)x, (u8)y, (u8)z}, t);
                    ++count;
                }
            }
        }
    };


    while (count < min_blocks) {

        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                for (u8 z = 0; z < size().z; ++z) {
                    set_block({x, y, z}, terrain::Type::air);
                }
            }
        }

        count = 0;

        gen(3, 0.02f);
        gen(size().z, 0.1f);

        PLATFORM_EXTENSION(feed_watchdog);
    }

    for (int x = 1; x < size().x - 1; ++x) {
        for (int y = 1; y < size().y - 1; ++y) {
            for (int z = 0; z < size().z - 1; ++z) {
                auto get_type = [&](int xoff, int yoff, int zoff) {
                    return get_block(
                               {(u8)(x + xoff), (u8)(y + yoff), (u8)(z + zoff)})
                        .type();
                };

                auto is_air = [&](int xoff, int yoff, int zoff) {
                    return get_type(xoff, yoff, zoff) == terrain::Type::air;
                };

                if (not is_air(0, 0, 0) and
                    (z == 0 or (z > 0 and not is_air(0, 0, -1))) and
                    not is_air(0, 0, 1) and not is_air(-1, 0, 0) and
                    not is_air(1, 0, 0) and not is_air(0, -1, 0) and
                    not is_air(0, 1, 0) and
                    get_type(0, 0, -1) not_eq terrain::Type::terrain) {

                    auto hide = rng::choice<8>(rng::critical_state);
                    switch (hide) {
                        break;

                    case 0:
                    case 1:
                        break;

                    default:
                    case 2:
                    case 3:
                    case 5:
                        if (z < 3 and
                            rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                        break;

                    case 4:
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::lava_source);
                        break;
                    }
                    if (z == 0 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3)) {
                        if (rng::choice<3>(rng::critical_state) > 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                    }
                    if (z == 1 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3) and
                        rng::choice<2>(rng::critical_state)) {
                        if (rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                    }
                }
            }
        }
    }

    u8 z = 0;
    for (; z < size().z; ++z) {
        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
                auto above = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
                if (t == terrain::Type::terrain and
                    above == terrain::Type::air) {
                    goto BREAK;
                }
            }
        }
    }
BREAK:

    for (u8 x = 0; x < size().x; ++x) {
        for (u8 y = 0; y < size().y; ++y) {
            if (get_block({x, y, 0}).type() == Type::air) {
                if (x == 0 or y == 0 or x == size().x - 1 or
                    y == size().y - 1) {
                    if (rng::choice<3>(rng::critical_state) == 0) {
                        set_block({x, y, 0}, Type::sand);
                    }
                } else {
                    set_block({x, y, 0}, Type::terrain);
                }
            }
        }
    }


    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            for (int z = size().z - 2; z > -1; --z) {
                auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
                if (t == terrain::Type::building) {
                    break;
                }
                if (t == terrain::Type::terrain) {

                    if (rng::choice<4>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z + 1)},
                                  terrain::Type::lumber);
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::volcanic_soil);
                    } else if (rng::choice<7>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::tulips);
                    }
                    break;
                }
            }
        }
    }


    const auto area = size().x * size().y;
    int low_count = 0;
    for (u8 x = 0; x < size().x; ++x) {
        for (u8 y = 0; y < size().y; ++y) {
            auto t = get_block({(u8)x, (u8)y, (u8)0}).type();
            auto above = get_block({(u8)x, (u8)y, (u8)1}).type();
            if (t not_eq terrain::Type::air and above == terrain::Type::air) {
                ++low_count;
            }
        }
    }

    if (low_count < (1 * area) / 3) {
        PLATFORM_EXTENSION(feed_watchdog);
        goto RETRY;
    }


    int tries = 0;
    while (1) {
        auto x = rng::choice(size().x - 2, rng::critical_state) + 1;
        auto y = rng::choice(size().y - 2, rng::critical_state) + 1;

        ++tries;

        if (tries == 1000) {
            goto RETRY;
        }

        int z = 0;

        auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
        auto above = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        auto u = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        auto d = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        auto l = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        auto r = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        if (t == terrain::Type::terrain and above == terrain::Type::air and
            u == terrain::Type::air and d == terrain::Type::air and
            l == terrain::Type::air and r == terrain::Type::air) {
            Vec3<u8> building_coord;
            building_coord = {(u8)x, (u8)y, (u8)(z + 1)};
            set_cursor(building_coord);
            break;
        }
    }

    update();
}



void terrain::Sector::generate_terrain_regular(int min_blocks,
                                               int building_count)
{
    int count = 0;


    auto gen = [&](int height_scale, Float freq) {
        Float data[16][16];

        fnl_state noise = fnlCreateState(rng::get(rng::critical_state), freq);
        noise.noise_type = FNL_NOISE_OPENSIMPLEX2;

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                data[x][y] = fnlGetNoise2D(&noise, x, y);
            }
        }

        u8 snowline = size().z * 0.7f;
        if (p_.shape_ == Shape::pillar) {
            snowline = 10;
        }

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                u8 height = height_scale * data[x][y];
                bool first = true;
                for (int z = height - 1; z > -1; --z) {
                    terrain::Type t = terrain::Type::basalt;

                    if (first) {
                        if (z > snowline) {
                            t = terrain::Type::ice;
                        } else if (z == 0) {
                            t = terrain::Type::sand;
                        } else if (z < snowline) {
                            t = terrain::Type::terrain;
                        }
                        first = false;
                    }

                    set_block({(u8)x, (u8)y, (u8)z}, t);
                    ++count;
                }
            }
        }
    };


    while (count < min_blocks) {

        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                for (u8 z = 0; z < size().z; ++z) {
                    set_block({x, y, z}, terrain::Type::air);
                }
            }
        }

        count = 0;

        gen(3, 0.02f);
        gen(size().z, 0.1f);

        PLATFORM_EXTENSION(feed_watchdog);
    }

    for (int x = 1; x < size().x - 1; ++x) {
        for (int y = 1; y < size().y - 1; ++y) {
            for (int z = 0; z < size().z - 1; ++z) {
                auto get_type = [&](int xoff, int yoff, int zoff) {
                    return get_block(
                               {(u8)(x + xoff), (u8)(y + yoff), (u8)(z + zoff)})
                        .type();
                };

                auto is_air = [&](int xoff, int yoff, int zoff) {
                    return get_type(xoff, yoff, zoff) == terrain::Type::air;
                };

                if (not is_air(0, 0, 0) and
                    (z == 0 or (z > 0 and not is_air(0, 0, -1))) and
                    not is_air(0, 0, 1) and not is_air(-1, 0, 0) and
                    not is_air(1, 0, 0) and not is_air(0, -1, 0) and
                    not is_air(0, 1, 0) and
                    get_type(0, 0, -1) not_eq terrain::Type::terrain) {

                    auto hide = rng::choice<8>(rng::critical_state);
                    switch (hide) {
                        break;

                    case 0:
                    case 1:
                        break;

                    default:
                    case 2:
                    case 3:
                    case 5:
                        if (z < 3 and
                            rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                        break;

                    case 4:
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::lava_source);
                        break;
                    }
                    if (z == 0 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3)) {
                        if (rng::choice<3>(rng::critical_state) > 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                    }
                    if (z == 1 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3) and
                        rng::choice<2>(rng::critical_state)) {
                        if (rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                    }
                }
            }
        }
    }

    u8 z = 0;
    int tries = 0;

    while (building_count) {
        auto x = rng::choice(size().x, rng::critical_state);
        auto y = rng::choice(size().y, rng::critical_state);


        auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
        auto above = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        if (t == terrain::Type::terrain and above == terrain::Type::air) {
            Vec3<u8> building_coord;
            building_coord = {(u8)x, (u8)y, (u8)(z + 1)};
            set_block(building_coord, terrain::Type::building);
            if (--building_count == 0) {
                goto PLACED_BUILDING;
            }
        }

        if (++tries == 10000) {
            ++z;
        }
    }
PLACED_BUILDING:

    int water_count = 0;

    Buffer<Vec3<u8>, 10> water_sources;

    const bool water = rng::choice<5>(rng::critical_state) == 0;


    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            for (int z = size().z - 2; z > -1; --z) {
                auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
                if (t == terrain::Type::building) {
                    break;
                }
                if (t == terrain::Type::terrain) {

                    if (rng::choice<4>(rng::critical_state)) {
                        set_block({(u8)x, (u8)y, (u8)(z + 1)},
                                  terrain::Type::lumber);
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::volcanic_soil);
                    } else if (water and water_count < 2 and
                               rng::choice<10>(rng::critical_state)) {
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::water_source);
                        water_sources.push_back({(u8)x, (u8)y, (u8)z});
                        ++water_count;
                    } else if (rng::choice<4>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::tulips);
                    }
                    break;
                }
            }
        }
    }

    if (water_count) {
        for (int i = 0; i < size().z * 2; ++i) {
            update();
        }
        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                for (int z = 0; z < size().z; ++z) {
                    // erosion
                    auto& block = get_block({(u8)x, (u8)y, (u8)(z)});
                    switch (block.type()) {
                    case Type::water_slant_a:
                    case Type::water_slant_b:
                    case Type::water_slant_c:
                    case Type::water_slant_d:
                    case Type::water_source:
                    case Type::water_spread_downwards:
                    case Type::water_spread_laterally_a:
                    case Type::water_spread_laterally_b:
                    case Type::water_spread_laterally_c:
                    case Type::water_spread_laterally_d:
                        set_block({(u8)x, (u8)y, (u8)(z)}, Type::air);
                        if (z > 0) {
                            set_block({(u8)x, (u8)y, (u8)(z - 1)}, Type::air);
                        }
                        break;

                    default:
                        break;
                    }
                }
            }
        }

        for (auto& s : water_sources) {
            set_block(s, Type::water_source);
        }

        for (int i = 0; i < size().z * 2; ++i) {
            update();
        }
    }


    if (water) {
        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                if (get_block({x, y, 0}).type() == Type::air) {
                    if (x == 0 or y == 0 or x == size().x - 1 or
                        y == size().y - 1) {
                        if (rng::choice<3>(rng::critical_state) == 0) {
                            set_block({x, y, 0}, Type::sand);
                        }
                    }
                    if (x == 1 or y == 1 or x == size().x - 2 or
                        y == size().y - 2) {
                        if (rng::choice<5>(rng::critical_state) == 0) {
                            set_block({x, y, 1}, Type::basalt);
                        }
                        set_block({x, y, 0}, Type::sand);
                    } else if (x > 0 and y > 0 and x < size().x - 1 and
                               y < size().y - 1) {
                        set_block({x, y, 0}, Type::water_source);
                    }
                }
            }
        }
        update();
    }
}



void terrain::Sector::generate_terrain_desert(int min_blocks,
                                              int building_count)
{
    int count = 0;


    auto gen = [&](int height_scale, Float freq) {
        Float data[16][16];

        fnl_state noise = fnlCreateState(rng::get(rng::critical_state), freq);
        noise.noise_type = FNL_NOISE_OPENSIMPLEX2;

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                data[x][y] = fnlGetNoise2D(&noise, x, y);
            }
        }

        u8 snowline = size().z * 0.7f;
        if (p_.shape_ == Shape::pillar) {
            snowline = 10;
        }

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                u8 height = height_scale * data[x][y];
                bool first = true;
                bool second = true;
                bool third = true;
                for (int z = height - 1; z > -1; --z) {
                    terrain::Type t = terrain::Type::basalt;

                    if (first or second or third) {
                        if (z > snowline) {
                            t = terrain::Type::basalt;
                        } else if (z == 0) {
                            t = terrain::Type::sand;
                        } else if (z < snowline) {
                            t = terrain::Type::sand;
                        }
                        if (first) {
                            if (rng::choice<8>(rng::critical_state) == 0) {
                                t = terrain::Type::masonry;
                            }
                        }
                        if (not second) {
                            third = false;
                        }
                        if (not first) {
                            second = false;
                        }
                        first = false;
                    }

                    set_block({(u8)x, (u8)y, (u8)z}, t);
                    ++count;
                }
            }
        }
    };


    while (count < min_blocks) {

        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                for (u8 z = 0; z < size().z; ++z) {
                    set_block({x, y, z}, terrain::Type::air);
                }
            }
        }

        count = 0;

        gen(3, 0.02f);
        gen(size().z, 0.1f);

        PLATFORM_EXTENSION(feed_watchdog);
    }

    for (int x = 1; x < size().x - 1; ++x) {
        for (int y = 1; y < size().y - 1; ++y) {
            for (int z = 0; z < size().z - 1; ++z) {
                auto get_type = [&](int xoff, int yoff, int zoff) {
                    return get_block(
                               {(u8)(x + xoff), (u8)(y + yoff), (u8)(z + zoff)})
                        .type();
                };

                auto is_air = [&](int xoff, int yoff, int zoff) {
                    return get_type(xoff, yoff, zoff) == terrain::Type::air;
                };

                if (not is_air(0, 0, 0) and
                    (z == 0 or (z > 0 and not is_air(0, 0, -1))) and
                    not is_air(0, 0, 1) and not is_air(-1, 0, 0) and
                    not is_air(1, 0, 0) and not is_air(0, -1, 0) and
                    not is_air(0, 1, 0) and
                    get_type(0, 0, -1) not_eq terrain::Type::terrain) {

                    auto hide = rng::choice<8>(rng::critical_state);
                    switch (hide) {
                        break;

                    case 0:
                    case 1:
                    case 4:
                        break;

                    default:
                    case 2:
                    case 3:
                    case 5:
                        if (rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                        break;
                    }
                    if (z == 0 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3)) {
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::crystal);
                    }
                    if (z == 1 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3) and
                        rng::choice<2>(rng::critical_state)) {
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::crystal);
                    }
                }
            }
        }
    }

    u8 z = 0;
    int tries = 0;

    while (building_count) {
        auto x = rng::choice(size().x, rng::critical_state);
        auto y = rng::choice(size().y, rng::critical_state);


        auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
        auto above = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        if ((t == terrain::Type::terrain or t == terrain::Type::sand or
             t == terrain::Type::masonry) and
            above == terrain::Type::air) {
            Vec3<u8> building_coord;
            building_coord = {(u8)x, (u8)y, (u8)(z + 1)};
            set_block(building_coord, terrain::Type::building);
            building_coord.z -= 1;
            set_block(building_coord, terrain::Type::masonry);
            if (--building_count == 0) {
                goto PLACED_BUILDING;
            }
        }

        if (++tries == 10000) {
            ++z;
        }
    }
PLACED_BUILDING:

    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            for (int z = size().z - 2; z > -1; --z) {
                auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
                if (t == terrain::Type::building) {
                    break;
                }
                if (t == terrain::Type::sand) {

                    if (rng::choice<7>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z + 1)},
                                  terrain::Type::lumber);
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::volcanic_soil);
                    }

                    break;
                }
            }
        }
    }
}



void terrain::Sector::generate_terrain_tundra(int min_blocks,
                                              int building_count)
{
    int count = 0;


    auto gen = [&](int height_scale, Float freq) {
        Float data[16][16];

        fnl_state noise = fnlCreateState(rng::get(rng::critical_state), freq);
        noise.noise_type = FNL_NOISE_OPENSIMPLEX2;

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                data[x][y] = fnlGetNoise2D(&noise, x, y);
            }
        }

        u8 snowline = size().z * 0.7f;
        if (p_.shape_ == Shape::pillar) {
            snowline = 10;
        }

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                u8 height = height_scale * data[x][y];
                bool first = true;
                bool second = true;
                for (int z = height - 1; z > -1; --z) {
                    terrain::Type t = terrain::Type::basalt;

                    if (first or second) {
                        if (z >= snowline) {
                            t = terrain::Type::ice;
                        } else if (z == 0) {
                            t = terrain::Type::basalt;
                        } else if (z < snowline) {
                            t = terrain::Type::ice;
                        }
                        if (not first) {
                            second = false;
                        }
                        first = false;
                    }

                    set_block({(u8)x, (u8)y, (u8)z}, t);
                    ++count;
                }
            }
        }
    };


    while (count < min_blocks) {

        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                for (u8 z = 0; z < size().z; ++z) {
                    set_block({x, y, z}, terrain::Type::air);
                }
            }
        }

        count = 0;

        gen(3, 0.02f);
        gen(size().z, 0.1f);

        PLATFORM_EXTENSION(feed_watchdog);
    }

    for (int x = 1; x < size().x - 1; ++x) {
        for (int y = 1; y < size().y - 1; ++y) {
            for (int z = 0; z < size().z - 1; ++z) {
                auto get_type = [&](int xoff, int yoff, int zoff) {
                    return get_block(
                               {(u8)(x + xoff), (u8)(y + yoff), (u8)(z + zoff)})
                        .type();
                };

                auto is_air = [&](int xoff, int yoff, int zoff) {
                    return get_type(xoff, yoff, zoff) == terrain::Type::air;
                };

                if (not is_air(0, 0, 0) and
                    (z == 0 or (z > 0 and not is_air(0, 0, -1))) and
                    not is_air(0, 0, 1) and not is_air(-1, 0, 0) and
                    not is_air(1, 0, 0) and not is_air(0, -1, 0) and
                    not is_air(0, 1, 0) and
                    get_type(0, 0, -1) not_eq terrain::Type::terrain) {

                    auto hide = rng::choice<8>(rng::critical_state);
                    switch (hide) {
                        break;

                    case 0:
                    case 1:
                    case 4:
                        break;

                    default:
                    case 2:
                    case 3:
                    case 5:
                        if (rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::marble);
                        }
                        break;
                    }
                    if (z == 0 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3)) {
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::crystal);
                    }
                    if (z == 1 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3) and
                        rng::choice<2>(rng::critical_state)) {
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::crystal);
                    }
                }
            }
        }
    }

    u8 z = 0;
    int tries = 0;

    while (building_count) {
        auto x = rng::choice(size().x, rng::critical_state);
        auto y = rng::choice(size().y, rng::critical_state);


        auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
        auto above = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        if ((t == terrain::Type::terrain or t == terrain::Type::basalt) and
            above == terrain::Type::air) {
            Vec3<u8> building_coord;
            building_coord = {(u8)x, (u8)y, (u8)(z + 1)};
            set_block(building_coord, terrain::Type::building);
            if (--building_count == 0) {
                goto PLACED_BUILDING;
            }
        }

        if (++tries == 10000) {
            ++z;
        }
    }
PLACED_BUILDING:

    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            for (int z = size().z - 2; z > -1; --z) {
                auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
                if (t == terrain::Type::building) {
                    break;
                }
                if (t == terrain::Type::ice and z < size().z - 2) {

                    if (rng::choice<4>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z + 1)},
                                  terrain::Type::lumber);
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::volcanic_soil);
                    }
                    break;
                }
            }
        }
    }
}



void terrain::Sector::generate_terrain_molten(int min_blocks,
                                              int building_count)
{
    int count = 0;

    auto gen = [&](int height_scale, Float freq) {
        Float data[16][16];

        fnl_state noise = fnlCreateState(rng::get(rng::critical_state), freq);
        noise.noise_type = FNL_NOISE_OPENSIMPLEX2;

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                data[x][y] = fnlGetNoise2D(&noise, x, y);
            }
        }

        u8 snowline = size().z * 0.7f;
        if (p_.shape_ == Shape::pillar) {
            snowline = 10;
        }

        for (int x = 0; x < size().x; ++x) {
            for (int y = 0; y < size().y; ++y) {
                u8 height = height_scale * data[x][y];
                bool first = true;
                bool second = true;
                for (int z = height - 1; z > -1; --z) {
                    terrain::Type t = terrain::Type::basalt;

                    if (first or second) {
                        if (z >= snowline) {
                            t = terrain::Type::basalt;
                        } else if (z == 0) {
                            t = terrain::Type::basalt;
                        } else if (z < snowline) {
                            t = terrain::Type::basalt;
                        }
                        if (not first) {
                            second = false;
                        }
                        first = false;
                    }

                    set_block({(u8)x, (u8)y, (u8)z}, t);
                    ++count;
                }
            }
        }
    };


    while (count < min_blocks) {

        for (u8 x = 0; x < size().x; ++x) {
            for (u8 y = 0; y < size().y; ++y) {
                for (u8 z = 0; z < size().z; ++z) {
                    set_block({x, y, z}, terrain::Type::air);
                }
            }
        }

        count = 0;

        gen(3, 0.02f);
        gen(size().z, 0.1f);

        PLATFORM_EXTENSION(feed_watchdog);
    }

    for (int x = 1; x < size().x - 1; ++x) {
        for (int y = 1; y < size().y - 1; ++y) {
            for (int z = 0; z < size().z - 1; ++z) {
                auto get_type = [&](int xoff, int yoff, int zoff) {
                    return get_block(
                               {(u8)(x + xoff), (u8)(y + yoff), (u8)(z + zoff)})
                        .type();
                };

                auto is_air = [&](int xoff, int yoff, int zoff) {
                    return get_type(xoff, yoff, zoff) == terrain::Type::air;
                };

                if (not is_air(0, 0, 0) and
                    (z == 0 or (z > 0 and not is_air(0, 0, -1))) and
                    not is_air(0, 0, 1) and not is_air(-1, 0, 0) and
                    not is_air(1, 0, 0) and not is_air(0, -1, 0) and
                    not is_air(0, 1, 0) and
                    get_type(0, 0, -1) not_eq terrain::Type::terrain) {

                    auto hide = rng::choice<8>(rng::critical_state);
                    switch (hide) {
                        break;

                    case 0:
                    case 1:
                    case 4:
                        break;

                    default:
                    case 2:
                    case 3:
                    case 5:
                        if (rng::choice<2>(rng::critical_state) == 0) {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::crystal);
                        } else {
                            set_block({(u8)x, (u8)y, (u8)z},
                                      terrain::Type::lava_source);
                        }
                        break;
                    }
                    if (z == 0 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3)) {
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::crystal);
                    }
                    if (z == 1 and not is_air(0, 0, 2) and
                        not is_air(0, 0, 3) and
                        rng::choice<2>(rng::critical_state)) {
                        set_block({(u8)x, (u8)y, (u8)z},
                                  terrain::Type::crystal);
                    }
                }
            }
        }
    }

    u8 z = 0;
    int tries = 0;

    while (building_count) {
        auto x = rng::choice(size().x, rng::critical_state);
        auto y = rng::choice(size().y, rng::critical_state);


        auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
        auto above = get_block({(u8)x, (u8)y, (u8)(z + 1)}).type();
        if ((t == terrain::Type::terrain or t == terrain::Type::basalt) and
            above == terrain::Type::air) {
            Vec3<u8> building_coord;
            building_coord = {(u8)x, (u8)y, (u8)(z + 1)};
            set_block(building_coord, terrain::Type::building);
            if (--building_count == 0) {
                goto PLACED_BUILDING;
            }
        }

        if (++tries == 10000) {
            ++z;
        }
    }
PLACED_BUILDING:

    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            for (int z = size().z - 2; z > -1; --z) {
                auto t = get_block({(u8)x, (u8)y, (u8)z}).type();
                if (t == terrain::Type::building) {
                    break;
                }
                if (t == terrain::Type::basalt and z < size().z - 2) {
                    if (rng::choice<7>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::lava_source);
                    } else if (rng::choice<4>(rng::critical_state) == 0) {
                        set_block({(u8)x, (u8)y, (u8)(z)},
                                  terrain::Type::basalt_brick);
                    }

                    break;
                }
            }
        }
    }

    for (int i = 0; i < 100; ++i) {
        update();
    }
}



} // namespace skyland::macro
