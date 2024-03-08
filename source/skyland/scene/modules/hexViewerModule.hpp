////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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
    HexViewerModule(UserContext&& user_context,
                    const char* file_path,
                    bool rom_file)
        : path_(file_path), user_context_(std::move(user_context))
    {
        if (rom_file) {
            // In practice, I'll never bundle binary blobs with the game in the
            // rom filesystem. There's no reason, really.
        } else {
            flash_filesystem::read_file_data_binary(file_path, data_);
        }
    }


    void enter(Scene&) override
    {
        Text title(OverlayCoord{1, 1});
        auto colors =
            FontColors{custom_color(0xffe763), custom_color(0x00210f)};
        title.append(path_.c_str(), colors);
        title.__detach();


        repaint(row_offset_);

        Text sz(OverlayCoord{1, 18});
        sz.append("size ", colors);
        sz.append(data_.size(), colors);
        sz.__detach();

        Text t("abcdef0123456789.", OverlayCoord{0, 20});
        t.__detach();

        PLATFORM.screen().schedule_fade(1.f, custom_color(0x00210f));
    }


    void repaint(u32 row_offset)
    {
        PLATFORM.system_call("vsync", 0);

        Text offset(OverlayCoord{14, 18});
        auto colors =
            FontColors{custom_color(0xffe763), custom_color(0x00210f)};
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
                const u16 t = PLATFORM.map_glyph(line[i], *mapping_info);
                PLATFORM.set_tile(
                    1 + i,
                    y,
                    t,
                    FontColors{custom_color(0xffe763), custom_color(0x00210f)});
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

                const u16 t = PLATFORM.map_glyph(c, *mapping_info);
                PLATFORM.set_tile(
                    21 + (i - start),
                    y,
                    t,
                    FontColors{custom_color(0xffe763), custom_color(0x00210f)});
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


    ScenePtr update(Time delta) override
    {
        player().update(delta);

        auto test_key = [&](Key k) {
            return player().test_key(k, milliseconds(500), milliseconds(100));
        };

        if (test_key(Key::down)) {
            ++row_offset_;
            repaint(row_offset_);
        }

        if (test_key(Key::up) and row_offset_ > 0) {
            --row_offset_;
            repaint(row_offset_);
        }

        if (test_key(Key::right)) {
            row_offset_ += 64;
            repaint(row_offset_);
        }

        if (test_key(Key::left) and row_offset_ >= 64) {
            row_offset_ -= 64;
            repaint(row_offset_);
        }


        if (PLATFORM.keyboard().down_transition<Key::action_2>()) {
            PLATFORM.fill_overlay(0);
            return make_scene<FileBrowserModule>(
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
