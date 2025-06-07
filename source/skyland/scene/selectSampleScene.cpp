////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "selectSampleScene.hpp"
#include "compression.hpp"
#include "fadeInScene.hpp"
#include "macro/selectorScene.hpp"
#include "skyland/macrocosmFreebuildSector.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"



namespace skyland
{



static const Float default_fade = 0.75f;



void SelectSampleScene::enter(Scene& prev)
{
    PLATFORM.screen().pixelate(128, false, true, false);

    PLATFORM.load_overlay_texture("overlay_challenges");

    samples_ = APP.invoke_script("/scripts/config/macro_samples/index.lisp");

    const auto sample_count = lisp::length(*samples_);

    page_count_ = sample_count / 5 + (sample_count % 5 ? 1 : 0);

    show_options();

    PLATFORM.screen().schedule_fade(
        default_fade, ColorConstant::rich_black, {}, false);
}



void SelectSampleScene::show_options()
{
    PLATFORM.screen().clear();
    text_.clear();
    PLATFORM.screen().display();

    PLATFORM.fill_overlay(0);

    PLATFORM.set_tile(Layer::overlay, 1, 2, 90);
    PLATFORM.set_tile(Layer::overlay, 28, 2, 92);
    PLATFORM.set_tile(Layer::overlay, 1, 15, 94);
    PLATFORM.set_tile(Layer::overlay, 28, 15, 96);
    for (int x = 2; x < 28; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 2, 91);
        PLATFORM.set_tile(Layer::overlay, x, 15, 95);
    }
    for (int y = 3; y < 15; ++y) {
        PLATFORM.set_tile(Layer::overlay, 1, y, 93);
        PLATFORM.set_tile(Layer::overlay, 28, y, 97);
    }

    if (not samples_) {
        return;
    }

    int index = 0;
    int start_index = page_ * 5;

    lisp::l_foreach(*samples_, [&](lisp::Value* val) {
        if (val->type() not_eq lisp::Value::Type::cons) {
            PLATFORM.fatal("sample list format invalid");
        }

        auto name = val->cons().car();
        if (name->type() not_eq lisp::Value::Type::string) {
            PLATFORM.fatal("sample list format invalid");
        }

        if (index++ < start_index) {
            return;
        }

        if (index > start_index + 5) {
            return;
        }

        text_.emplace_back(name->string().value(),
                           OverlayCoord{4, u8(4 + text_.size() * 2)});
    });


    if (page_count_ > 1) {
        int margin = (calc_screen_tiles().x - page_count_ * 2) / 2;
        for (int i = 0; i < page_count_; ++i) {
            if (i == page_) {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 83);
            } else {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 82);
            }
        }
    }
}



void prep_level();



void SelectSampleScene::exit(Scene& next)
{
    PLATFORM.screen().pixelate(0);
    text_.clear();
    PLATFORM.fill_overlay(0);
    PLATFORM.load_overlay_texture("overlay");
}



void SelectSampleScene::display()
{
    if (state_ not_eq State::idle) {
        return;
    }
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(59);

    Vec2<Fixnum> origin;

    auto ambient_movement = 2 * float(sine(4 * 3.14f * 0.004f * timer_ + 180)) /
                            std::numeric_limits<s16>::max();

    origin.x += Fixnum(16 + ambient_movement);
    origin.y += Fixnum(32 + cursor_ * 16 - 1);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);
}



ScenePtr SelectSampleScene::update(Time delta)
{
    if (exit_) {
        page_ = 0;
        cursor_ = 0;
        return make_scene<StartMenuScene>(-1, 3);
    }

    timer_ += delta;

    switch (state_) {
    case State::fade_in:
        state_ = State::idle;
        break;

    case State::idle: {
        if (not samples_) {
            return null_scene();
        }

        if (APP.player().key_down(Key::down)) {
            if ((u32)cursor_ < text_.size() - 1) {
                cursor_++;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }

        if (APP.player().key_down(Key::up)) {
            if (cursor_) {
                cursor_--;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }

        if (APP.player().key_down(Key::right)) {
            if (page_ < page_count_ - 1) {
                ++page_;
                show_options();
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (APP.player().key_down(Key::left)) {
            if (page_ > 0) {
                --page_;
                show_options();
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::fade_out;
            timer_ = 0;
            text_.clear();
            PLATFORM.fill_overlay(0);
        } else if (APP.player().key_down(Key::action_2)) {
            text_.clear();
            PLATFORM.fill_overlay(0);
            exit_ = true;
        }
        break;
    }

    case State::fade_out: {

        auto& m = macrocosm();

        auto index = page_ * 5 + cursor_;
        auto choice = lisp::get_list(*samples_, index);

        auto file_name = lisp::get_list(choice, 1);
        if (file_name->type() not_eq lisp::Value::Type::string) {
            PLATFORM.fatal("sample list format invalid");
        }

        s8 type_override = -1;
        if (lisp::length(choice) >= 2) {
            type_override = lisp::get_list(choice, 2)->integer().value_;
        }

        APP.set_coins(0);

        const char* base_path = "scripts/config/macro_samples";
        const char* fname = file_name->string().value();

        auto file = PLATFORM.load_file(base_path, fname);
        if (file.second) {

            PLATFORM.speaker().play_sound("cursor_tick", 0);

            using TranslationBuffer = Buffer<char, 2000>;
            auto inp = allocate_dynamic<TranslationBuffer>("inp-buffer");
            auto outp = allocate_dynamic<TranslationBuffer>("outp-buffer");

            for (u32 i = 0; i < file.second; ++i) {
                inp->push_back(file.first[i]);
            }

            decompress(*inp, *outp);

            // NOTE: see terrain::Sector::qr_encode for data format.
            int pos = 0;
            u8 sh = (*outp)[pos++];
            if (type_override not_eq -1) {
                sh = type_override;
            }
            const auto shape = (macro::terrain::Sector::Shape)sh;

            m.bind_sector({0, 0});
            m.erase_sector({0, 1});
            m.make_sector({0, 1}, shape);
            auto bound = m.bind_sector({0, 1});

            for (u8 z = 0; z < bound->size().z; ++z) {
                for (u8 x = 0; x < bound->size().x; ++x) {
                    for (u8 y = 0; y < bound->size().y; ++y) {
                        auto bl = (macro::terrain::Type)(*outp)[pos++];
                        if (bl not_eq macro::terrain::Type::selector) {
                            bound->set_block({x, y, z}, bl);
                        }
                    }
                }
            }
            auto sz = bound->size();
            bound->set_cursor({u8(sz.x / 2), u8(sz.y / 2), u8(sz.z / 2)});
        }

        PLATFORM.load_overlay_texture("overlay");

        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.screen().schedule_fade(0.f);

        auto next = make_scene<macro::SelectorScene>();
        next->show_island_size();
        return next;
    }
    }

    APP.update_parallax(delta);

    return null_scene();
}



} // namespace skyland
