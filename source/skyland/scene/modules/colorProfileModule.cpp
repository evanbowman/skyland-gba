////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "colorProfileModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



StringBuffer<64> ColorProfileModule::load_current_profile()
{
    Vector<char> color_mode;
    StringBuffer<64> ret;
    flash_filesystem::read_file_data("/save/color.txt", color_mode);
    if (color_mode.size()) {
        for (char c : color_mode) {
            ret.push_back(c);
        }
    }
    return ret;
}



void ColorProfileModule::enter(Scene& prev)
{
    options_ = APP.invoke_script("/scripts/data/color/color.lisp");

    auto prof = load_current_profile();

    int i = 0;

    PLATFORM.load_overlay_chunk(258, 0, 72 + 12, "colorProfiles");
    draw_image(258, 17, 1, 12, 7, Layer::overlay);

    title_.emplace(OverlayCoord{1, 1});
    title_->assign("Color Profiles");

    lisp::l_foreach(*options_, [this, prof, &i](lisp::Value* v) {
        if (text_.full()) {
            return;
        }
        text_.emplace_back(OverlayCoord{3, u8(5 + text_.size() * 2)});
        text_.back().assign(v->cons().car()->string().value());
        auto file = v->cons().cdr();
        if (file->type() == lisp::Value::Type::string) {
            if (file->string().value() == prof) {
                sel_ = i;
            }
        }
        ++i;
    });

    PLATFORM.screen().schedule_fade(0);
    PLATFORM.screen().schedule_fade(1);
}



void ColorProfileModule::exit(Scene& next)
{
    title_.reset();
    text_.clear();

    PLATFORM.fill_overlay(0);
}



void ColorProfileModule::bind_selected_profile()
{
    auto v = lisp::get_list(*options_, sel_);
    auto fname = v->cons().cdr();
    if (auto cm = PLATFORM.get_extensions().apply_color_correction) {
        if (fname not_eq L_NIL) {
            cm(fname->string().value());
        } else {
            cm(nullptr);
        }
        PLATFORM.screen().set_shader_argument(0);
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1);
    }
}



ScenePtr ColorProfileModule::update(Time delta)
{
    if (key_down<Key::action_1>()) {
        auto v = lisp::get_list(*options_, sel_);
        auto fname = v->cons().cdr();
        if (fname not_eq L_NIL) {
            StringBuffer<128> color_name = fname->string().value();
            Vector<char> cname;
            for (char c : color_name) {
                cname.push_back(c);
            }
            flash_filesystem::store_file_data("/save/color.txt", cname);
        } else {
            flash_filesystem::unlink_file("/save/color.txt");
        }

        bind_selected_profile();

        return make_scene<TitleScreenScene>(3);
    }

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::down)) {
        if (sel_ < (int)text_.size() - 1) {
            ++sel_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
            bind_selected_profile();
        }
    }
    if (test_key(Key::up)) {
        if (sel_ > 0) {
            --sel_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
            bind_selected_profile();
        }
    }

    if (sel_ not_eq last_sel_) {
        for (int y = 3; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 0);
        }
        PLATFORM.set_tile(Layer::overlay, 1, 5 + sel_ * 2, 396);
        last_sel_ = sel_;
    }

    return null_scene();
}



ColorProfileModule::Factory ColorProfileModule::factory_;



} // namespace skyland
