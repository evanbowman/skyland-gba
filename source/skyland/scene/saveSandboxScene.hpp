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

#include "fadeInScene.hpp"
#include "graphics/overlay.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "startMenuScene.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



class SaveSandboxScene : public Scene
{
public:
    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
        text_.clear();
    }

    void enter(Scene& prev) override
    {
        const auto st = calc_screen_tiles();

        for (int x = 1; x < st.x - 1; ++x) {
            for (int y = 0; y < 4; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, 4 + y, 498);
            }

            for (int y = 0; y < 4; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, 9 + y, 498);
            }

            for (int y = 0; y < 4; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, 14 + y, 498);
            }
        }

        const auto colors =
            FontColors{ColorConstant::indigo_tint, custom_color(0xd9dee6)};

        text_.emplace_back(OverlayCoord{2, 5});
        text_.back().assign("sandbox 1", colors);

        text_.emplace_back(OverlayCoord{2, 10});
        text_.back().assign("sandbox 2", colors);

        text_.emplace_back(OverlayCoord{2, 15});
        text_.back().assign("sandbox 3", colors);
    }



    void display() override
    {
        Sprite sprite;
        sprite.set_tidx_16x16(28, 1);
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_origin({8, 8});
        sprite.set_priority(0);
        sprite.set_mix({ColorConstant::silver_white, 1});


        auto view = PLATFORM.screen().get_view().get_center();

        Vec2<Fixnum> origin;
        origin.x = 5;
        origin.y = (4 + 5 * cursor_) * 8 - 3;
        origin.x += Fixnum(view.x);
        origin.y += Fixnum(view.y);

        sprite.set_position(origin);
        PLATFORM.screen().draw(sprite);

        origin.x += Fixnum::from_integer((PLATFORM.screen().size().x - 16) + 7);
        sprite.set_position(origin);
        sprite.set_flip({true, false});
        PLATFORM.screen().draw(sprite);

        origin.y += Fixnum::from_integer(23 + 16);
        sprite.set_position(origin);
        sprite.set_flip({true, true});
        PLATFORM.screen().draw(sprite);

        origin.x -= Fixnum::from_integer((PLATFORM.screen().size().x - 16) + 7);
        sprite.set_position(origin);
        sprite.set_flip({false, true});
        PLATFORM.screen().draw(sprite);
    }



    ScenePtr<Scene> update(Microseconds delta)
    {
        if (player().key_down(Key::action_1)) {
            return on_selected();
        }

        if (player().key_down(Key::action_2)) {
            return scene_pool::alloc<StartMenuScene>(1);
        }

        if (player().key_down(Key::down)) {
            if (cursor_ < 2) {
                ++cursor_;
            }
        }

        if (player().key_down(Key::up)) {
            if (cursor_ > 0) {
                --cursor_;
            }
        }

        return null_scene();
    }



    int cursor() const
    {
        return cursor_;
    }



    struct VectorPrinter : lisp::Printer
    {
        void put_str(const char* c) override
        {
            while (*c not_eq '\0') {
                data_.push_back(*c);
                ++c;
            }
        }

        Vector<char> data_;
    };



    virtual ScenePtr<Scene> on_selected()
    {
        VectorPrinter p;
        auto val = APP.invoke_script("/scripts/sandbox/save.lisp");
        lisp::format(val, p);

        p.data_.push_back('\0');

        // For debugging purposes: don't compress sandbox data if select key
        // pressed.
        const bool compress_output =
            not PLATFORM.keyboard().pressed<Key::select>();

        flash_filesystem::store_file_data_text(

            format("/save/sb%.lisp", cursor_).c_str(),
            p.data_,
            {.use_compression_ = compress_output});

        synth_notes_store(APP.player_island(),
                          format("/save/sb%_p_synth.dat", cursor()).c_str());

        speaker_data_store(APP.player_island(),
                           format("/save/sb%_p_speaker.dat", cursor()).c_str());


        synth_notes_store(*APP.opponent_island(),
                          format("/save/sb%_o_synth.dat", cursor()).c_str());

        speaker_data_store(*APP.opponent_island(),
                           format("/save/sb%_o_speaker.dat", cursor()).c_str());


        PLATFORM.fill_overlay(0);

        return scene_pool::alloc<FadeInScene>();
    }



private:
    Buffer<Text, 3> text_;
    int cursor_ = 0;
};



void set_island_positions(Island& left_island, Island& right_island);



class LoadSandboxScene : public SaveSandboxScene
{
public:
    ScenePtr<Scene> on_selected() override
    {
        Vector<char> data;

        auto bytes = flash_filesystem::read_file_data_text(
            format("/save/sb%.lisp", cursor()).c_str(), data);

        if (bytes == 0) {
            return null_scene();
        }

        lisp::VectorCharSequence seq(data);
        lisp::read(seq);             // (0)
        lisp::eval(lisp::get_op(0)); // (1)

        auto arg = lisp::get_op(0); // result of eval()

        auto fn = APP.invoke_script("/scripts/sandbox/restore.lisp");
        if (fn->type() == lisp::Value::Type::function) {
            lisp::push_op(arg); // pass save data buffer on stack
            safecall(fn, 1);    // one argument (the save data)
            lisp::pop_op();     // funcall result
        } else {
            PLATFORM.fatal("restore.lisp does not return a function!");
        }

        lisp::pop_op(); // result of eval() (1)
        lisp::pop_op(); // result of read() (0)


        synth_notes_load(APP.player_island(),
                         format("/save/sb%_p_synth.dat", cursor()).c_str());

        speaker_data_load(APP.player_island(),
                          format("/save/sb%_p_speaker.dat", cursor()).c_str());


        synth_notes_load(*APP.opponent_island(),
                         format("/save/sb%_o_synth.dat", cursor()).c_str());

        speaker_data_load(*APP.opponent_island(),
                          format("/save/sb%_o_speaker.dat", cursor()).c_str());


        PLATFORM.fill_overlay(0);

        set_island_positions(APP.player_island(), *APP.opponent_island());
        APP.opponent_island()->set_drift(Fixnum(-0.000025f));

        APP.time_stream().clear();

        return scene_pool::alloc<FadeInScene>();
    }
};



} // namespace skyland
