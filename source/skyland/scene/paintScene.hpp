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

#include "modules/fileBrowserModule.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/img.hpp"
#include "skyland/paint.hpp"
#include "skyland/scene.hpp"
#include "skyland/skyland.hpp"
#include "string.hpp"



namespace skyland
{



class PaintScene : public Scene, public Paint
{
public:
    static const int canvas_width = 16;
    static const int canvas_height = 16;


    PaintScene(const char* file_path, bool create)
        : Paint(canvas_width, canvas_height, 1, 1), file_path_(file_path),
          create_(create)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        app.effects().clear();

        if (not create_) {
            Vector<char> data;
            ram_filesystem::read_file_data_binary(
                pfrm, file_path_.c_str(), data);

            if (data.size() >= sizeof texture_) {
                auto it = data.begin();
                for (u32 i = 0; i < sizeof texture_; ++i) {
                    ((u8*)&texture_)[i] = *it;
                    ++it;
                }
            }

        } else {
            for (int x = 0; x < canvas_width; ++x) {
                for (int y = 0; y < canvas_height; ++y) {
                    set_pixel(app, x, y, 1);
                }
            }
        }

        Paint::init(pfrm, app);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        pfrm.screen().schedule_fade(0.f);

        if (app.player().key_down(pfrm, Key::action_2)) {
            Vector<char> output;
            for (u32 i = 0; i < sizeof texture_; ++i) {
                output.push_back(((u8*)&texture_)[i]);
            }
            output.push_back('\0');

            ram_filesystem::store_file_data_binary(
                pfrm, file_path_.c_str(), output);

            return scene_pool::alloc<FileBrowserModule>();
        }

        return Paint::update(pfrm, app, delta);
    }


    void display(Platform& pfrm, App& app)
    {
        Paint::display(pfrm, app);
    }


    u8 get_pixel(App& app, u8 x, u8 y) override
    {
        return texture_.get_pixel(x, y);
    }


    void set_pixel(App& app, u8 x, u8 y, u8 tile) override
    {
        texture_.set_pixel(x, y, tile);
    }


private:
    img::Image texture_;
    StringBuffer<64> file_path_;
    bool create_;
};



} // namespace skyland
