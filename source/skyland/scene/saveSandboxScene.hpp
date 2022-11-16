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
    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        pfrm.fill_overlay(0);
    }

    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        const auto st = calc_screen_tiles(pfrm);

        for (int x = 1; x < st.x - 1; ++x) {
            for (int y = 0; y < 4; ++y) {
                pfrm.set_tile(Layer::overlay, x, 4 + y, 498);
            }

            for (int y = 0; y < 4; ++y) {
                pfrm.set_tile(Layer::overlay, x, 9 + y, 498);
            }

            for (int y = 0; y < 4; ++y) {
                pfrm.set_tile(Layer::overlay, x, 14 + y, 498);
            }
        }

        const auto colors =
            FontColors{ColorConstant::indigo_tint, custom_color(0xd9dee6)};

        text_.emplace_back(pfrm, OverlayCoord{2, 5});
        text_.back().assign("sandbox 1", colors);

        text_.emplace_back(pfrm, OverlayCoord{2, 10});
        text_.back().assign("sandbox 2", colors);

        text_.emplace_back(pfrm, OverlayCoord{2, 15});
        text_.back().assign("sandbox 3", colors);
    }



    void display(Platform& pfrm, App& app) override
    {
        Sprite sprite;
        sprite.set_tidx_16x16(28, 1);
        sprite.set_size(Sprite::Size::w16_h16);
        sprite.set_origin({8, 8});
        sprite.set_priority(0);
        sprite.set_mix({ColorConstant::silver_white, 1});


        auto view = pfrm.screen().get_view().get_center();

        Vec2<Fixnum> origin;
        origin.x = 5;
        origin.y = (4 + 5 * cursor_) * 8 - 3;
        origin.x += view.x;
        origin.y += view.y;

        sprite.set_position(origin);
        pfrm.screen().draw(sprite);

        origin.x += ((pfrm.screen().size().x - 16) + 7);
        sprite.set_position(origin);
        sprite.set_flip({true, false});
        pfrm.screen().draw(sprite);

        origin.y += 23 + 16;
        sprite.set_position(origin);
        sprite.set_flip({true, true});
        pfrm.screen().draw(sprite);

        origin.x -= ((pfrm.screen().size().x - 16) + 7);
        sprite.set_position(origin);
        sprite.set_flip({false, true});
        pfrm.screen().draw(sprite);
    }



    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta)
    {
        if (player(app).key_down(pfrm, Key::action_1)) {
            return on_selected(pfrm, app);
        }

        if (player(app).key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<StartMenuScene>(1);
        }

        if (player(app).key_down(pfrm, Key::down)) {
            if (cursor_ < 2) {
                ++cursor_;
            }
        }

        if (player(app).key_down(pfrm, Key::up)) {
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



    virtual ScenePtr<Scene> on_selected(Platform& pfrm, App& app)
    {
        VectorPrinter p;
        auto val = app.invoke_script(pfrm, "/scripts/sandbox/save.lisp");
        lisp::format(val, p);

        p.data_.push_back('\0');

        flash_filesystem::store_file_data_text(
            pfrm, format("/save/sb%.lisp", cursor_).c_str(), p.data_);

        synth_notes_store(pfrm,
                          app.player_island(),
                          format("/save/sb%_p_synth.dat", cursor()).c_str());

        speaker_data_store(pfrm,
                           app.player_island(),
                           format("/save/sb%_p_speaker.dat", cursor()).c_str());


        synth_notes_store(pfrm,
                          *app.opponent_island(),
                          format("/save/sb%_o_synth.dat", cursor()).c_str());

        speaker_data_store(pfrm,
                           *app.opponent_island(),
                           format("/save/sb%_o_speaker.dat", cursor()).c_str());


        pfrm.fill_overlay(0);

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
    ScenePtr<Scene> on_selected(Platform& pfrm, App& app) override
    {
        Vector<char> data;

        auto bytes = flash_filesystem::read_file_data_text(
            pfrm, format("/save/sb%.lisp", cursor()).c_str(), data);

        if (bytes == 0) {
            return null_scene();
        }

        lisp::VectorCharSequence seq(data);
        lisp::read(seq);             // (0)
        lisp::eval(lisp::get_op(0)); // (1)

        auto arg = lisp::get_op(0); // result of eval()

        auto fn = app.invoke_script(pfrm, "/scripts/sandbox/restore.lisp");
        if (fn->type() == lisp::Value::Type::function) {
            lisp::push_op(arg); // pass save data buffer on stack
            funcall(fn, 1);     // one argument (the save data)
            lisp::pop_op();     // funcall result
        } else {
            pfrm.fatal("restore.lisp does not return a function!");
        }

        lisp::pop_op(); // result of eval() (1)
        lisp::pop_op(); // result of read() (0)


        synth_notes_load(pfrm,
                         app.player_island(),
                         format("/save/sb%_p_synth.dat", cursor()).c_str());

        speaker_data_load(pfrm,
                          app.player_island(),
                          format("/save/sb%_p_speaker.dat", cursor()).c_str());


        synth_notes_load(pfrm,
                         *app.opponent_island(),
                         format("/save/sb%_o_synth.dat", cursor()).c_str());

        speaker_data_load(pfrm,
                          *app.opponent_island(),
                          format("/save/sb%_o_speaker.dat", cursor()).c_str());


        pfrm.fill_overlay(0);

        set_island_positions(app.player_island(), *app.opponent_island());
        app.opponent_island()->set_drift(pfrm, app, -0.000025f);

        app.time_stream().clear();

        return scene_pool::alloc<FadeInScene>();
    }
};



} // namespace skyland
