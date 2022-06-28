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

#include "containers/vector.hpp"
#include "fileBrowserModule.hpp"
#include "graphics/overlay.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene.hpp"
#include "userContext.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



class HexViewerModule : public Scene
{
public:
    HexViewerModule(Platform& pfrm,
                    UserContext&& user_context,
                    const char* file_path,
                    bool rom_file)
        : path_(file_path), user_context_(std::move(user_context))
    {
        if (rom_file) {
            // In practice, I'll never bundle binary blobs with the game in the
            // rom filesystem. There's no reason, really.
        } else {
            flash_filesystem::read_file_data_binary(pfrm, file_path, data_);
        }
    }


    void enter(Platform& pfrm, App&, Scene&) override
    {
        Text title(pfrm, OverlayCoord{1, 1});
        auto colors =
            FontColors{custom_color(0xc2d9a9), custom_color(0x110731)};
        title.append(path_.c_str(), colors);
        title.__detach();


        repaint(pfrm, row_offset_);

        Text sz(pfrm, OverlayCoord{1, 18});
        sz.append("size ", colors);
        sz.append(data_.size(), colors);
        sz.__detach();

        Text t(pfrm, "abcdef0123456789.", OverlayCoord{0, 20});
        t.__detach();

        pfrm.screen().schedule_fade(1.f, custom_color(0x110731));
    }


    void repaint(Platform& pfrm, u32 row_offset)
    {
        pfrm.system_call("vsync", 0);

        Text offset(pfrm, OverlayCoord{14, 18});
        auto colors =
            FontColors{custom_color(0xc2d9a9), custom_color(0x110731)};
        offset.append("offset ", colors);
        offset.append(row_offset * 8, colors);
        offset.append("       ", colors);
        offset.__detach();


        const char* hex = "0123456789abcdef";

        auto draw_row = [&](int row, int y) {
            StringBuffer<32> line;
            bool space = false;

            const u32 start = row * 8;
            const u32 end = row * 8 + 8;

            for (u32 i = start; i < end; ++i) {
                if (i >= data_.size()) {
                    line.push_back('?');
                    line.push_back('?');
                } else {
                    u8 byte = data_[i];
                    line.push_back(hex[(byte & 0xf0) >> 4]);
                    line.push_back(hex[(byte & 0x0f)]);
                }


                if (space) {
                    line.push_back(' ');
                    space = false;
                } else {
                    space = true;
                }
            }

            for (u32 i = 0; i < line.length(); ++i) {
                auto mapping_info = locale_texture_map()(line[i]);
                const u16 t = pfrm.map_glyph(line[i], *mapping_info);
                pfrm.set_tile(
                    1 + i,
                    y,
                    t,
                    FontColors{custom_color(0xc2d9a9), custom_color(0x110731)});
            }

            for (u32 i = start; i < end; ++i) {
                char c = '?';
                if (i < data_.size()) {
                    c = data_[i];
                }
                auto mapping_info = locale_texture_map()(c);
                if (not mapping_info) {
                    mapping_info = locale_texture_map()('.');
                }

                const u16 t = pfrm.map_glyph(c, *mapping_info);
                pfrm.set_tile(
                    21 + (i - start),
                    y,
                    t,
                    FontColors{custom_color(0xc2d9a9), custom_color(0x110731)});
            }
        };

        draw_row(row_offset++, 3);
        draw_row(row_offset++, 4);
        draw_row(row_offset++, 5);
        draw_row(row_offset++, 6);
        draw_row(row_offset++, 7);
        draw_row(row_offset++, 8);
        draw_row(row_offset++, 9);
        draw_row(row_offset++, 10);
        draw_row(row_offset++, 11);
        draw_row(row_offset++, 12);
        draw_row(row_offset++, 13);
        draw_row(row_offset++, 14);
        draw_row(row_offset++, 15);
        draw_row(row_offset++, 16);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        player(app).update(pfrm, app, delta);

        auto test_key = [&](Key k) {
            return player(app).test_key(
                pfrm, k, milliseconds(500), milliseconds(100));
        };

        if (test_key(Key::down)) {
            ++row_offset_;
            repaint(pfrm, row_offset_);
        }

        if (test_key(Key::up) and row_offset_ > 0) {
            --row_offset_;
            repaint(pfrm, row_offset_);
        }

        if (test_key(Key::right)) {
            row_offset_ += 64;
            repaint(pfrm, row_offset_);
        }

        if (test_key(Key::left) and row_offset_ >= 64) {
            row_offset_ -= 64;
            repaint(pfrm, row_offset_);
        }


        if (pfrm.keyboard().down_transition<Key::action_2>()) {
            pfrm.fill_overlay(0);
            return scene_pool::alloc<FileBrowserModule>(
                std::move(user_context_), path_.c_str(), false);
        }

        return null_scene();
    }


private:
    StringBuffer<86> path_;
    Vector<char> data_;

    UserContext user_context_;

    u32 row_offset_ = 0;
};



} // namespace skyland
