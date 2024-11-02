////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////

#include "minimap.hpp"
#include "ext_workram_data.hpp"
#include "room_metatable.hpp"
#include "skyland.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/incinerator.hpp"
#include "skyland/rooms/rocketSilo.hpp"



namespace skyland::minimap
{



static const int minimap_start_tile = 290;
static const int minimap_isle_spacing = 3;
static u8 minimap_x_anchor;
static u8 y_anchor = 14;
static bool visible = false;
static bool needs_repaint_ = false;



void schedule_repaint()
{
    needs_repaint_ = true;
}



bool needs_repaint()
{
    return needs_repaint_;
}



EXT_WORKRAM_DATA Bitmatrix<16, 16> player_destroyed_rooms;



static u8 minimap_width()
{
    int pixel_width =
        (3 * (1 + APP.player_island().terrain().size() + minimap_isle_spacing +
              (APP.opponent_island() ? APP.opponent_island()->terrain().size()
                                     : 0) +
              1));
    return pixel_width / 8 + (pixel_width % 8 > 0);
}



static Platform::EncodedTile encode_small_tile(u8 tile_data[16][16])
{
    Platform::EncodedTile t;
    u8* out = t.bytes_;

    u8 temp = 0;
    for (int i = 0; i < 8; ++i) {
        for (int j = 0; j < 8; ++j) {
            if (j % 2) {
                temp |= tile_data[j][i] << 4;
                *(out++) = temp;
            } else {
                temp = tile_data[j][i] & 0xff;
            }
        }
    }

    return t;
}



void move(u8 new_y_anchor)
{
    hide();
    y_anchor = new_y_anchor;
    show();
}



void show()
{
    if (APP.game_mode() == App::GameMode::tutorial) {
        return;
    }

    if (not APP.opponent_island()) {
        hide();
        return;
    }

    const u8 width = minimap_width();
    const u8 anchor = 29 - width;

    hide();

    u16 tile = minimap_start_tile;
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < width; ++x) {
            PLATFORM.set_tile(Layer::overlay, anchor + x, y_anchor + y, tile++);
        }
    }

    minimap_x_anchor = anchor;

    visible = true;
}



void hide()
{
    if (APP.game_mode() == App::GameMode::tutorial) {
        return;
    }

    const u8 width = minimap_width();

    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < width; ++x) {
            PLATFORM.set_tile(
                Layer::overlay, minimap_x_anchor + x, y_anchor + y, 0);
        }
    }

    visible = false;
}



void repaint(const Settings& settings)
{
    needs_repaint_ = false;

    if (APP.game_mode() == App::GameMode::tutorial) {
        return;
    }

    if (not APP.opponent_island()) {
        return;
    }

    [[maybe_unused]] auto before = PLATFORM.delta_clock().sample();


    const u8 width = minimap_width();

    static const int minimap_px_width = 104;
    static const int minimap_px_height = 40;
    using MinimapPixels = u8[minimap_px_width][minimap_px_height];

    // auto pixel_buffer = allocate_dynamic<MinimapPixels>("m-px-buffer");
    MinimapPixels pixel_buffer;

    auto cursor_loc = globals().far_cursor_loc_;

    auto fb_cache = settings.pixel_cache_;

    auto save_pixels = [&]() {
        if (not fb_cache) {
            return;
        }
        if (fb_cache->pixels_.size() == minimap_px_width * minimap_px_height) {
            auto it = fb_cache->pixels_.begin();
            for (int x = 0; x < minimap_px_width; ++x) {
                for (int y = 0; y < minimap_px_height; ++y) {
                    *(it++) = pixel_buffer[x][y];
                }
            }
        } else {
            for (int x = 0; x < minimap_px_width; ++x) {
                for (int y = 0; y < minimap_px_height; ++y) {
                    fb_cache->pixels_.push_back(pixel_buffer[x][y]);
                }
            }
        }
        fb_cache->player_island_checksum_ = APP.player_island().checksum();
        fb_cache->opponent_island_checksum_ = APP.opponent_island()->checksum();
    };

    auto restore_pixels = [&]() {
        if (not fb_cache) {
            return;
        }
        if (fb_cache->pixels_.size() < minimap_px_width * minimap_px_height) {
            Platform::fatal("logic err");
        }
        auto it = fb_cache->pixels_.begin();
        for (int x = 0; x < minimap_px_width; ++x) {
            for (int y = 0; y < minimap_px_height; ++y) {
                pixel_buffer[x][y] = *(it++);
            }
        }
    };

    static const u8 color_red_index = 1;
    static const u8 color_black_index = 3;
    static const u8 color_tan_index = 8;
    static const u8 color_white_index = 4;
    static const u8 color_el_blue_index = 7;
    static const u8 color_gray_index = 10;
    static const u8 color_darkgray_index = 5;
    static const u8 color_burnt_orange_index = 14;
    static const u8 color_green_index = 11;
    static const u8 color_bright_yellow_index = 12;

    const int opp_offset =
        1 + APP.player_island().terrain().size() + minimap_isle_spacing;

    Buffer<Room*, 32> weapons;

    auto is_selected = [&](RoomCoord c) {
        if (settings.weapon_group_selection_) {
            for (auto& oc : settings.weapon_group_selection_->rooms_) {
                if (c == oc) {
                    return true;
                }
            }
        }
        return false;
    };

    if (fb_cache and
        fb_cache->player_island_checksum_ == APP.player_island().checksum() and
        fb_cache->opponent_island_checksum_ ==
            APP.opponent_island()->checksum()) {

        for (u8 y = 4; y < 15; ++y) {
            for (u8 x = 0; x < 13; ++x) {
                if (auto room = APP.player_island().get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                            Room::Category::weapon and
                        ((settings.weapon_loc_ and
                          *settings.weapon_loc_ == Vec2<u8>{x, y}) or
                         (room->group() not_eq Room::Group::none and
                          settings.group_ and
                          room->group() == *settings.group_) or
                         is_selected({x, y}))) {
                        bool found = false;
                        for (auto& wpn : weapons) {
                            if (wpn == room) {
                                found = true;
                                break;
                            }
                        }
                        if (not found) {
                            weapons.push_back(room);
                        }
                    }
                }
            }
        }

        restore_pixels();

    } else {
        memset(pixel_buffer, color_black_index, sizeof pixel_buffer);

        for (u32 x = 0; x < APP.player_island().terrain().size(); ++x) {
            for (int xx = 0; xx < 3; ++xx) {
                for (int yy = 0; yy < 3; ++yy) {
                    pixel_buffer[(x + 1) * 3 + xx][((15 - 3) * 3 + yy) - 2] =
                        (yy == 0) ? color_green_index : color_darkgray_index;
                }
            }
        }

        for (u32 x = 0; x < APP.opponent_island()->terrain().size(); ++x) {
            for (int xx = 0; xx < 3; ++xx) {
                for (int yy = 0; yy < 3; ++yy) {
                    pixel_buffer[(x + opp_offset) * 3 + xx -
                                 2][((15 - 3) * 3 + yy) - 2] =
                        (yy == 0) ? color_green_index : color_darkgray_index;
                }
            }
        }

        for (u8 y = 4; y < 15; ++y) {
            for (u8 x = 0; x < 13; ++x) {
                auto set_pixel = [&](int xo, int yo, int v) {
                    pixel_buffer[(x + 1) * 3 + xo]
                        [((y - 3) * 3 + yo) - 2] = v;
                };
                if (auto room = APP.player_island().get_room({x, y})) {
                    if (APP.player_island().fire_present({x, y})) {
                        set_pixel(0, 0, color_red_index);
                        set_pixel(1, 0, color_red_index);
                        set_pixel(2, 0, color_red_index);

                        set_pixel(0, 1, color_red_index);
                        set_pixel(1, 1, color_bright_yellow_index);
                        set_pixel(2, 1, color_red_index);

                        set_pixel(0, 2, color_bright_yellow_index);
                        set_pixel(1, 2, color_bright_yellow_index);
                        set_pixel(2, 2, color_bright_yellow_index);
                        continue;
                    }
                    if ((*room->metaclass())->category() ==
                            Room::Category::weapon and
                        ((settings.weapon_loc_ and
                          *settings.weapon_loc_ == Vec2<u8>{x, y}) or
                         (room->group() not_eq Room::Group::none and
                          settings.group_ and
                          room->group() == (*settings.group_)) or
                         is_selected({x, y}))) {
                        bool found = false;
                        for (auto& wpn : weapons) {
                            if (wpn == room) {
                                found = true;
                                break;
                            }
                        }
                        if (not found) {
                            weapons.push_back(room);
                        }
                    }
                    for (int xx = 0; xx < 3; ++xx) {
                        for (int yy = 0; yy < 3; ++yy) {
                            u8 clr;
                            switch ((*room->metaclass())->category()) {
                            case Room::Category::wall:
                                if ((*room->metaclass())->properties() &
                                    RoomProperties::accepts_ion_damage) {
                                    clr = color_el_blue_index;
                                } else {
                                    clr = color_gray_index;
                                }
                                break;

                            case Room::Category::weapon:
                                clr = color_burnt_orange_index;
                                break;

                            default:
                                clr = color_tan_index;
                                break;
                            }

                            if (((settings.weapon_loc_ and
                                  *settings.weapon_loc_ == Vec2<u8>{x, y}) or
                                 (room->group() not_eq Room::Group::none and
                                  settings.group_ and
                                  room->group() == *settings.group_)) and
                                not(xx == 1 and yy == 1)) {
                                clr = color_white_index;
                            }

                            pixel_buffer[(x + 1) * 3 + xx]
                                        [((y - 3) * 3 + yy) - 2] = clr;
                        }
                    }
                } else if (settings.show_destroyed_rooms_ and
                           player_destroyed_rooms.get(x, y)) {
                    for (int xx = 0; xx < 3; ++xx) {
                        for (int yy = 0; yy < 3; ++yy) {
                            pixel_buffer[(x + 1) * 3 + xx]
                                        [((y - 3) * 3 + yy) - 2] = 1;
                        }
                    }
                }
            }
        }

        for (u8 y = 4; y < 15; ++y) {
            for (u8 x = 0; x < 13; ++x) {
                if (auto room = APP.opponent_island()->get_room({x, y})) {
                    if (APP.opponent_island()->fire_present({x, y})) {
                        auto set_pixel = [&](int xo, int yo, int v) {
                            pixel_buffer[(x + opp_offset) * 3 + xo - 2]
                                        [((y - 3) * 3 + yo) - 2] = v;
                        };
                        set_pixel(0, 0, color_red_index);
                        set_pixel(1, 0, color_red_index);
                        set_pixel(2, 0, color_red_index);

                        set_pixel(0, 1, color_red_index);
                        set_pixel(1, 1, color_bright_yellow_index);
                        set_pixel(2, 1, color_red_index);

                        set_pixel(0, 2, color_bright_yellow_index);
                        set_pixel(1, 2, color_bright_yellow_index);
                        set_pixel(2, 2, color_bright_yellow_index);
                        continue;
                    }
                    for (int xx = 0; xx < 3; ++xx) {
                        for (int yy = 0; yy < 3; ++yy) {
                            u8 clr;
                            switch ((*room->metaclass())->category()) {
                            case Room::Category::wall:
                                if ((*room->metaclass())->properties() &
                                    RoomProperties::accepts_ion_damage) {
                                    clr = color_el_blue_index;
                                } else {
                                    clr = color_gray_index;
                                }
                                break;

                            case Room::Category::weapon:
                                clr = color_burnt_orange_index;
                                break;

                            default:
                                clr = color_tan_index;
                                break;
                            }

                            pixel_buffer[(x + opp_offset) * 3 + xx - 2]
                                        [((y - 3) * 3 + yy) - 2] = clr;
                        }
                    }
                }
            }
        }

        save_pixels();
    }

    const u8 cursor_center_px_x = (cursor_loc.x + opp_offset) * 3 + 1 - 2;
    const u8 cursor_center_px_y = ((cursor_loc.y - 3) * 3) - 2 + 1;

    auto plot = [&](int x, int y, auto intersection) {
        if (pixel_buffer[x][y] == color_black_index or
            pixel_buffer[x][y] == color_white_index or
            pixel_buffer[x][y] == 12) {
            pixel_buffer[x][y] = color_white_index;
        } else {
            if (pixel_buffer[x][y] == color_gray_index) {
                pixel_buffer[x][y] = 13;
            } else {
                pixel_buffer[x][y] = 1;
            }

            intersection(x, y);
        }
    };

    auto highlight_block = [&](int x, int y, bool center) {
        if (x > 13 or y > 14) {
            return;
        }
        const int x_offset = 3 * opp_offset - 2;
        const int y_offset = 4 * -3 + 1;
        for (int xx = 0; xx < 3; ++xx) {
            for (int yy = 0; yy < 3; ++yy) {
                const int xt = x_offset + x * 3 + xx;
                const int yt = y_offset + y * 3 + yy;
                if (not center and xx == 1 and yy == 1) {
                    pixel_buffer[xt][yt] = color_black_index;
                    continue;
                }
                if (center and xx == 1 and yy == 1) {
                    pixel_buffer[xt][yt] = 12;
                    continue;
                }
                if (pixel_buffer[xt][yt] not_eq color_black_index and
                    pixel_buffer[xt][yt] not_eq color_white_index) {
                    pixel_buffer[xt][yt] = 1;
                }
            }
        }
    };

    auto plot_line = [&](Room* wpn, int x0, int y0, int x1, int y1) {
        int dx = abs(x1 - x0);
        int sx = x0 < x1 ? 1 : -1;
        int dy = -abs(y1 - y0);
        int sy = y0 < y1 ? 1 : -1;
        int error = dx + dy;

        bool intersection = false;
        auto intersection_fn = [&](int x, int y) {
            if (not intersection and x >= opp_offset * 3) {
                intersection = true;
                u8 ib_x = (x / 3 - opp_offset) + (x % 3 > 0);
                u8 ib_y = ((y - 3) / 3 + 4) + (y % 3 > 0);

                highlight_block(ib_x, ib_y, true);

                if (wpn and wpn->cast<FlakGun>()) {
                    highlight_block(ib_x, ib_y - 2, false);
                    highlight_block(ib_x, ib_y - 1, false);
                    highlight_block(ib_x, ib_y + 1, false);
                    highlight_block(ib_x, ib_y + 2, false);
                    highlight_block(ib_x + 1, ib_y, false);
                    highlight_block(ib_x + 2, ib_y, false);
                    highlight_block(ib_x + 1, ib_y + 1, false);
                    highlight_block(ib_x + 1, ib_y - 1, false);
                }
            }
        };

        while (true) {
            plot(x0, y0, intersection_fn);
            if (x0 == x1 && y0 == y1)
                break;
            int e2 = 2 * error;
            if (e2 >= dy) {
                if (x0 == x1)
                    break;
                error = error + dy;
                x0 = x0 + sx;
            }
            if (e2 <= dx) {
                if (y0 == y1)
                    break;
                error = error + dx;
                y0 = y0 + sy;
            }
        }
    };

    if (settings.weapon_loc_) {
        auto weapon_loc = *settings.weapon_loc_;
        if (settings.target_near_ and *settings.target_near_) {
            if (auto drone = APP.player_island().get_drone(weapon_loc)) {
                int drone_emit_px_x = ((*drone)->position().x + 1) * 3;
                int drone_emit_px_y =
                    (((*drone)->position().y - 3) * 3 + 1) - 2;

                plot_line(nullptr,
                          drone_emit_px_x,
                          drone_emit_px_y,
                          cursor_center_px_x,
                          cursor_center_px_y);
            }
        } else {
            if (auto drone = APP.opponent_island()->get_drone(weapon_loc)) {
                int drone_emit_px_x = ((*drone)->position().x) * 3;
                int drone_emit_px_y =
                    (((*drone)->position().y - 3) * 3 + 1) - 2;

                plot_line(nullptr,
                          drone_emit_px_x + opp_offset * 3 - 1,
                          drone_emit_px_y,
                          cursor_center_px_x,
                          cursor_center_px_y);
            }
        }
    }


    for (auto wpn : weapons) {
        auto emit_pos = wpn->position();
        emit_pos.x += wpn->size().x;

        int wpn_emit_px_x = (emit_pos.x + 1) * 3;
        int wpn_emit_px_y = ((emit_pos.y - 3) * 3 + 1) - 2;

        if ((*wpn->metaclass())->weapon_orientation() ==
            Room::WeaponOrientation::horizontal) {
            plot_line(wpn,
                      wpn_emit_px_x - 2,
                      wpn_emit_px_y,
                      cursor_center_px_x,
                      cursor_center_px_y);
            plot_line(wpn,
                      wpn_emit_px_x - 2,
                      wpn_emit_px_y,
                      cursor_center_px_x,
                      cursor_center_px_y - 1);
            plot_line(wpn,
                      wpn_emit_px_x - 2,
                      wpn_emit_px_y,
                      cursor_center_px_x,
                      cursor_center_px_y + 1);
        } else if ((*wpn->metaclass())->weapon_orientation() ==
                   Room::WeaponOrientation::vertical) {

            bool intersection = false;
            auto intersection_fn = [&](int x, int y) {
                if (not intersection and x >= opp_offset * 3) {
                    intersection = true;
                    u8 ib_x = (x / 3 - opp_offset) + (x % 3 > 0);
                    u8 ib_y = ((y - 3) / 3 + 4) + (y % 3 > 0);

                    highlight_block(ib_x, ib_y, true);

                    if (wpn->cast<RocketSilo>()) {
                        highlight_block(ib_x + 1, ib_y, false);
                        highlight_block(ib_x, ib_y + 1, false);
                        highlight_block(ib_x - 1, ib_y, false);
                    }
                }
            };

            wpn_emit_px_y -= 2;
            wpn_emit_px_x -= 2;
            for (int y = wpn_emit_px_y; y > 0; --y) {
                plot(wpn_emit_px_x, y, [](int, int) {});
            }
            for (int x = cursor_center_px_x - 2; x < cursor_center_px_x + 3;
                 ++x) {
                int y;
                for (y = 1; y < minimap_px_height - 6; ++y) {
                    plot(x, y, intersection_fn);
                }

                intersection = false;
            }
        }
    }

    if (settings.weapon_loc_ or settings.group_) {
        auto& pb = pixel_buffer;
        auto xoff = cursor_loc.x + opp_offset;
        auto yoff = cursor_loc.y - 3;

        pb[xoff * 3 - 2][(yoff * 3) - 2] = color_white_index;
        pb[xoff * 3 + 1 - 2][(yoff * 3) - 2 + 1] = color_white_index;
        pb[xoff * 3 + 2 - 2][(yoff * 3) - 2 + 2] = color_white_index;
        pb[xoff * 3 + 2 - 2][(yoff * 3) - 2] = color_white_index;
        pb[xoff * 3 - 2][(yoff * 3) - 2 + 2] = color_white_index;

        pb[xoff * 3 - 2 + 1 - 1][(yoff * 3) + 1 - 2] = 13;
        pb[xoff * 3 - 2 + 2][(yoff * 3) + 1 - 2] = 13;
        pb[xoff * 3 + 1 - 2][(yoff * 3) + 1 - 2 - 1] = 13;
        pb[xoff * 3 + 1 - 2][(yoff * 3) - 2 + 2] = 13;
    }


    u16 tile = minimap_start_tile;
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < width; ++x) {
            Platform::TilePixels td;
            for (int xx = 0; xx < 8; ++xx) {
                for (int yy = 0; yy < 8; ++yy) {
                    td.data_[xx][yy] = pixel_buffer[x * 8 + xx][y * 8 + yy];
                }
            }
            PLATFORM.overwrite_overlay_tile(tile, encode_small_tile(td.data_));
            tile++;
        }
    }

    [[maybe_unused]] auto after = PLATFORM.delta_clock().sample();

    if (not PLATFORM.network_peer().is_connected()) {
        if (Platform::DeltaClock::duration(before, after) > milliseconds(16)) {
            // FIXME: repaint function has large overhead. Optimize and remove
            // clock reset.
            PLATFORM.delta_clock().reset();
        }
    }
}



void draw_cursor(bool near)
{
    Sprite spr;
    spr.set_size(Sprite::Size::w8_h8);
    spr.set_tidx_8x8(34, 1);
    spr.set_priority(0);

    auto view_center = PLATFORM.screen().get_view().int_center();
    auto pos = Vec2<Fixnum>{Fixnum::from_integer(view_center.x),
                            Fixnum::from_integer(view_center.y)};
    pos.x += Fixnum::from_integer(minimap_x_anchor * 8 - 1);
    pos.y += Fixnum::from_integer(y_anchor * 8);

    if (not near) {
        pos.x += 7.0_fixed +
                 Fixnum::from_integer(APP.player_island().terrain().size() * 3);
    }

    auto& cursor_loc =
        near ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    pos.x += 3.0_fixed * Fixnum::from_integer(cursor_loc.x + 1);
    pos.y += 3.0_fixed * Fixnum::from_integer(cursor_loc.y - 4);

    spr.set_position(pos);

    PLATFORM.screen().draw(spr);
}



void draw_weapon_targets(const Weapon& weapon)
{
    if (not visible) {
        return;
    }

    Sprite spr;
    spr.set_size(Sprite::Size::w8_h8);
    spr.set_tidx_8x8(34, 2);
    spr.set_priority(0);

    auto view_center = PLATFORM.screen().get_view().int_center();
    auto pos = Vec2<Fixnum>{Fixnum::from_integer(view_center.x),
                            Fixnum::from_integer(view_center.y)};
    pos.x += Fixnum::from_integer(minimap_x_anchor * 8 - 1);
    pos.y += Fixnum::from_integer(y_anchor * 8);

    pos.x += 7.0_fixed +
             Fixnum::from_integer(APP.player_island().terrain().size() * 3);

    for (int i = 0; i < weapon.target_queue().size(); ++i) {
        auto tc = weapon.target_queue()[i].coord();
        auto p = pos;
        p.x += 3.0_fixed * Fixnum::from_integer(tc.x + 1);
        p.y += 3.0_fixed * Fixnum::from_integer(tc.y - 4);
        spr.set_position(p);
        PLATFORM.screen().draw(spr);
    }
}



} // namespace skyland::minimap
