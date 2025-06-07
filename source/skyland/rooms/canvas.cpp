////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "canvas.hpp"
#include "base32.hpp"
#include "compression.hpp"
#include "script/listBuilder.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/paintScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static const int slot_count = Tile::canvas_tiles_end - Tile::canvas_tiles_begin;
static Bitvector<slot_count> canvas_texture_slots;



using ImgPool = ObjectPool<img::Image, slot_count>;
Optional<DynamicMemory<ImgPool>> img_pool;



Canvas::ImagePtr alloc_img()
{
    if (not img_pool) {
        img_pool = allocate_dynamic<ImgPool>("canvas-img-pool", "canvas-img");
    }

    return {(*img_pool)->alloc(), [](img::Image* ptr) {
                if (ptr == nullptr) {
                    PLATFORM.fatal("cannot free null image ptr!");
                }
                if (not img_pool) {
                    PLATFORM.fatal("img pool dealloc logic error!");
                }
                (*img_pool)->free(ptr);

                if ((*img_pool)->remaining() == slot_count) {
                    // The image pool is no longer in use. Drop it to free memory.
                    img_pool.reset();
                }
            }};
}



static int alloc_canvas_texture()
{
    for (int i = 0; i < slot_count; ++i) {
        if (not canvas_texture_slots.get(i)) {
            canvas_texture_slots.set(i, true);
            return i;
        }
    }

    return -1;
}



Canvas::Canvas(Island* parent, const RoomCoord& position)
    : Decoration(parent, name(), position)
{
    tile_ = Tile::canvas;
}



void Canvas::format_description(StringBuffer<512>& buffer)
{
    make_format(buffer, SYSTR(description_canvas)->c_str(), slot_count);
}



Canvas::~Canvas()
{
    if (canvas_texture_slot_ > -1) {
        canvas_texture_slots.set(canvas_texture_slot_, false);
    }
}



void Canvas::bind_graphics(const img::Image& img)
{
    if (canvas_texture_slot_ < 0) {
        canvas_texture_slot_ = alloc_canvas_texture();
    }

    if (canvas_texture_slot_ < 0) {
        info("too many canvas textures!");
        apply_damage(Room::health_upper_limit());
    } else {
        tile_ = Tile::canvas_tiles_begin + canvas_texture_slot_;
        if (not img_data_) {
            img_data_ = alloc_img();
        }
        **img_data_ = img;

        publish_tiles();

        parent()->schedule_repaint();
    }
}



void Canvas::record_removed()
{
    if (img_data_) {
        time_stream::event::CanvasBlockDestroyed e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.near_ = is_player_island(parent());
        memcpy(e.data_, &(**img_data_), sizeof **img_data_);
        APP.time_stream().push(APP.level_timer(), e);
    }
}



void Canvas::on_salvage()
{
    record_removed();
}



void Canvas::finalize()
{
    Room::finalize();

    if (health() == 0) {
        record_removed();
    }
}



void Canvas::publish_tiles()
{
    if (not img_data_) {
        return;
    }

    u8 buffer[16][16];
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            buffer[x][y] = (**img_data_).get_pixel(x, y);
        }
    }

    const int sl = canvas_texture_slot_;

    auto enc = PLATFORM.encode_tile(buffer);

    switch (parent()->layer()) {
    case Layer::map_0_ext:
        PLATFORM.overwrite_t0_tile(Tile::canvas_tiles_begin + sl, enc);
        break;

    case Layer::map_1_ext:
        PLATFORM.overwrite_t1_tile(Tile::canvas_tiles_begin + sl, enc);
        break;

    default:
        break;
    }
}



void Canvas::render_interior(App* app, TileId buffer[16][16])
{
    render_exterior(app, buffer);
}



void Canvas::render_exterior(App* app, TileId buffer[16][16])
{
    auto p = position();
    buffer[p.x][p.y] = tile_;
}



ScenePtr Canvas::select_impl(const RoomCoord& cursor)
{
    if (APP.game_mode() not_eq App::GameMode::sandbox) {
        return null_scene();
    }

    auto ret = [this] {
        if (img_data_) {
            return make_scene<PaintScene>(**img_data_);
        } else {
            return make_scene<PaintScene>("/dev/txtr.img", true);
        }
    }();

    ret->save_to_file_ = false;
    ret->backdrop_color_ = custom_color(0x4cb2d5);
    ret->exit_on_overscroll();

    auto isle = parent();
    auto p = position();

    bool found_slot = false;
    for (int i = 0; i < slot_count; ++i) {
        if (not canvas_texture_slots.get(i)) {
            found_slot = true;
        }
    }

    if (not found_slot) {
        apply_damage(Room::health_upper_limit());
        return null_scene();
    }

    ret->next_ = [isle, p](const img::Image& img,
                           const Optional<Key>& overscroll) -> ScenePtr {
        APP.player_island().schedule_repaint();
        if (APP.opponent_island()) {
            APP.opponent_island()->schedule_repaint();
        }
        if (auto room = isle->get_room(p)) {
            auto bind = [&] {
                if (auto canvas = room->cast<Canvas>()) {
                    canvas->bind_graphics(img);
                }
            };

            if (overscroll) {
                auto p = room->position();
                switch (*overscroll) {
                case Key::left:
                    --p.x;
                    break;

                case Key::right:
                    ++p.x;
                    break;

                case Key::up:
                    --p.y;
                    break;

                case Key::down:
                    ++p.y;
                    break;

                default:
                    break;
                }
                if (p.x < 16 and p.y < 16) {
                    if (auto adjacent = isle->get_room(p)) {
                        if (auto c = adjacent->cast<Canvas>()) {
                            auto scn = c->select(p);
                            if (scn) {
                                bind();
                                return scn;
                            }
                        }
                    }
                }
                return null_scene();
            }
            bind();
            if (is_player_island(room->parent())) {
                return make_scene<ReadyScene>();
            } else {
                return make_scene<InspectP2Scene>();
            }
        }
        PLATFORM.fatal("cannot writeback image: origin room missing!");
    };

    return ret;
}



lisp::Value* Canvas::serialize()
{
    lisp::ListBuilder builder;
    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    if (img_data_) {
        Vector<char> output;
        {
            Vector<char> data;
            for (auto& b : (**img_data_).data_) {
                data.push_back(b.data_);
            }
            compress(data, output);
        }
        lisp::ListBuilder out_list;
        out_list.push_back(L_INT(output.size()));

        // Explanation of what's happening below: Numbers in skyland lisp are 32
        // bits. If we push individual bytes, we waste some space. While we have
        // more than 4 bytes remaining to encode, pack 4 bytes into an int
        // value, then pack the remaining trailing bytes into individual
        // integers so that we don't need to pad.

        int rem = output.size();
        u32 i = 0;
        while (rem > 4) {
            out_list.push_back(L_INT(output[i] | (output[i + 1] << 8) |
                                     (output[i + 2] << 16) |
                                     (output[i + 3] << 24)));
            rem -= 4;
            i += 4;
        }
        for (; i < output.size(); ++i) {
            out_list.push_back(L_INT(output[i]));
        }

        builder.push_back(out_list.result());
    }

    return builder.result();
}



void Canvas::deserialize(lisp::Value* v)
{
    auto data_list = get_list(v, 3);

    if (lisp::is_list(data_list)) {

        auto rem = data_list->cons().car()->integer().value_;
        data_list = data_list->cons().cdr();

        Vector<char> input;

        lisp::l_foreach(data_list, [&](lisp::Value* v) {
            if (rem > 4) {
                input.push_back(v->integer().value_ & 0x000000ff);
                input.push_back((v->integer().value_ & 0x0000ff00) >> 8);
                input.push_back((v->integer().value_ & 0x00ff0000) >> 16);
                input.push_back((v->integer().value_ & 0xff000000) >> 24);
                rem -= 4;
            } else {
                input.push_back(v->integer().value_);
                --rem;
            }
        });

        Vector<char> data;
        decompress(input, data);

        img::Image img;
        for (u32 i = 0; i < data.size(); ++i) {
            img.data_[i].data_ = data[i];
        }
        bind_graphics(img);
    }
}



} // namespace skyland
