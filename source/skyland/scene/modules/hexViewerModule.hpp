////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    HexViewerModule(ScratchBufferPtr sbr)
        : path_(format<86>("<sbr:'%'>", sbr->tag_))
    {
        for (char c : sbr->data_) {
            data_.push_back(c);
        }
    }


    HexViewerModule(UserContext&& user_context,
                    const char* file_path,
                    bool rom_file)
        : path_(file_path), user_context_(std::move(user_context))
    {
        if (rom_file) {
            // In practice, I'll never bundle binary blobs with the game in the
            // rom filesystem. There's no reason, really.  P.S.: Hah! This is no
            // longer true. Oh well. But I also have no reason to look at a
            // large binary blob from within a GBA game.
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
        PLATFORM_EXTENSION(force_vsync);

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

            if (next_scene_) {
                PLATFORM.screen().schedule_fade(0);
                return (*next_scene_)();
            }

            return make_scene<FileBrowserModule>(
                std::move(user_context_), path_.c_str(), false);
        }

        return null_scene();
    }


    Optional<DeferredScene> next_scene_;


private:
    StringBuffer<86> path_;
    Vector<char> data_;

    UserContext user_context_;

    u32 row_offset_ = 0;
};



} // namespace skyland
