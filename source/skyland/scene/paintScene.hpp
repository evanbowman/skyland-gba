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

#include "modules/fileBrowserModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/img.hpp"
#include "skyland/paint.hpp"
#include "skyland/scene.hpp"
#include "skyland/skyland.hpp"
#include "string.hpp"



namespace skyland
{



// TODO: fix the horrible spaghetti-code in this file!



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


    PaintScene(const img::Image& img)
        : Paint(canvas_width, canvas_height, 1, 1), create_(false)
    {
        texture_ = img;
        initialized_ = true;
        save_to_file_ = false;
    }


    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
    }


    void enter(Scene& prev) override
    {
        APP.effects().clear();

        PLATFORM.fill_overlay(0);

        if (not create_) {
            Vector<char> data;
            flash_filesystem::read_file_data_binary(file_path_.c_str(), data);

            if (data.size() >= sizeof texture_) {
                auto it = data.begin();
                for (u32 i = 0; i < sizeof texture_; ++i) {
                    ((u8*)&texture_)[i] = *it;
                    ++it;
                }
            }

        } else if (not initialized_) {
            for (int x = 0; x < canvas_width; ++x) {
                for (int y = 0; y < canvas_height; ++y) {
                    set_pixel(x, y, 1);
                }
            }
        }

        preview_ = true;

        Paint::init();

        if (backdrop_color_) {
            draw_world_ = false;
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 16; ++y) {
                    PLATFORM.set_tile(APP.player_island().layer(), x, y, 0);
                    if (APP.opponent_island()) {
                        PLATFORM.set_tile(
                            APP.opponent_island()->layer(), x, y, 0);
                    }
                }
            }

            PLATFORM.screen().schedule_fade(
                1, *backdrop_color_, false, false, true, false);
        }
    }


    using OverscrollDir = Key;

    Optional<ColorConstant> backdrop_color_;
    Optional<
        Function<sizeof(void*) * 4,
                 ScenePtr(const img::Image&, const Optional<OverscrollDir>&)>>
        next_;
    bool save_to_file_ = true;


    ScenePtr update(Time delta) override
    {
        APP.player().update(delta);

        if (not backdrop_color_) {
            PLATFORM.screen().schedule_fade(0);
        }

        Optional<OverscrollDir> overscroll;

        auto prev_cursor = cursor_;

        if (exit_on_overscroll_ and mode_ == Mode::draw) {
            // Allow scrolling to adjacent canvases:
            if (APP.player().key_down(Key::right) and
                cursor_.x == (width_ - 1)) {
                overscroll = Key::right;
                cursor_.x = 0;
            }
            if (APP.player().key_down(Key::left) and cursor_.x == 0) {
                overscroll = Key::left;
                cursor_.x = width_ - 1;
            }
            if (APP.player().key_down(Key::up) and cursor_.y == 0) {
                overscroll = Key::up;
                cursor_.y = height_ - 1;
            }
            if (APP.player().key_down(Key::down) and
                cursor_.y == (height_ - 1)) {
                overscroll = Key::down;
                cursor_.y = 0;
            }
        }

        if (overscroll or
            (tool_ == Tool::exit and APP.player().key_down(Key::action_1))) {

            if (next_) {
                auto ret = (*next_)(texture_, overscroll);
                if (not ret) {
                    cursor_ = prev_cursor;
                } else {
                    PLATFORM.screen().schedule_fade(0);
                }
                return ret;
            } else if (save_to_file_) {
                Vector<char> output;
                for (u32 i = 0; i < sizeof texture_; ++i) {
                    output.push_back(((u8*)&texture_)[i]);
                }
                output.push_back('\0');

                flash_filesystem::store_file_data_binary(file_path_.c_str(),
                                                         output);
            }

            return make_scene<FileBrowserModule>();
        }

        return Paint::update(delta);
    }


    void display() override
    {
        Paint::display();
    }


    u8 get_pixel(u8 x, u8 y) override
    {
        return texture_.get_pixel(x, y);
    }


    void set_pixel(u8 x, u8 y, u8 tile) override
    {
        texture_.set_pixel(x, y, tile);
    }


    void exit_on_overscroll()
    {
        exit_on_overscroll_ = true;
    }


private:
    img::Image texture_;
    StringBuffer<64> file_path_;
    bool create_;
    bool initialized_ = false;
    bool exit_on_overscroll_ = false;
};



} // namespace skyland
