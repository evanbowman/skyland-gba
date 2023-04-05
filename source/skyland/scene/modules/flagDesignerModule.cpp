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


#include "flagDesignerModule.hpp"
#include "platform/platform.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void load_flag(Platform& pfrm, App& app, u16 tile);



class FlagTemplateScene : public Scene
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        pfrm.screen().schedule_fade(0);
        pfrm.screen().schedule_fade(1);

        Text::print(pfrm, SYS_CSTR(choose_flag), {1, 1});
        Text::print(pfrm, SYS_CSTR(flag_default), {3, 4});
        Text::print(pfrm, SYS_CSTR(flag_alt1), {3, 6});
        Text::print(pfrm, SYS_CSTR(flag_alt2), {3, 8});
        Text::print(pfrm, SYS_CSTR(flag_alt3), {3, 10});
        Text::print(pfrm, SYS_CSTR(flag_alt4), {3, 12});
        Text::print(pfrm, SYS_CSTR(flag_alt5), {3, 14});
        // Text::print(pfrm, SYS_CSTR(flag_alt6), {3, 16});

        pfrm.set_tile(Layer::overlay, 1, 4, 475);
    }



    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        pfrm.fill_overlay(0);
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta)
    {
        if (player(app).key_down(pfrm, Key::action_2) or
            player(app).key_down(pfrm, Key::select)) {
            return scene_pool::alloc<FlagDesignerModule>();
        }

        if (player(app).key_down(pfrm, Key::down)) {
            if (sel_ < 5) {
                ++sel_;
            }
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, 4 + y, 0);
            }
            pfrm.set_tile(Layer::overlay, 1, 4 + sel_ * 2, 475);
        }
        if (player(app).key_down(pfrm, Key::up)) {
            if (sel_ > 0) {
                --sel_;
            }
            for (int y = 0; y < 20; ++y) {
                pfrm.set_tile(Layer::overlay, 1, 4 + y, 0);
            }
            pfrm.set_tile(Layer::overlay, 1, 4 + sel_ * 2, 475);
        }

        if (player(app).key_down(pfrm, Key::action_1)) {
            switch (sel_) {
            case 0:
                load_default_flag(pfrm, app);
                break;

            case 1:
                load_flag(pfrm, app, 381);
                break;

            case 2:
                load_flag(pfrm, app, 379);
                break;

            case 6:
                load_flag(pfrm, app, 376);
                break;

            case 3:
                load_flag(pfrm, app, 378);
                break;

            case 5:
                load_flag(pfrm, app, 377);
                break;

            case 4:
                load_flag(pfrm, app, 380);
                break;
            }
            auto next = scene_pool::alloc<FlagDesignerModule>();
            next->changed_ = true;
            return next;
        }


        return null_scene();
    }

private:
    int sel_ = 0;
};



void FlagDesignerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);
    pfrm.screen().schedule_fade(1.f);

    app.player_island().show_flag(true);


    pfrm.load_tile0_texture("tilesheet");


    Paint::init(pfrm, app);


    app.player_island().init_terrain(pfrm, 4);
    configure_island_from_codestring(
        pfrm, app, app.player_island(), "'((power-core 1 13))");

    app.player_island().render_exterior(pfrm, app);
    app.player_island().set_position(
        {Fixnum::from_integer(152), Fixnum::from_integer(370)});


    GenericBird::spawn(
        pfrm, app, app.player_island(), rng::choice<3>(rng::utility_state));


    show(pfrm, app);

    pfrm.screen().schedule_fade(0);

    static const Text::OptColors colors{
        {ColorConstant::silver_white, custom_color(0x5aadef)}};

    Text::print(pfrm, SYS_CSTR(flag_designer_presets), {17, 1}, colors);
}



void FlagDesignerModule::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
    pfrm.system_call("vsync", nullptr); // FIXME
    pfrm.screen().clear();
    pfrm.screen().display();
    pfrm.screen().fade(1.f);
}



void FlagDesignerModule::show(Platform& pfrm, App& app)
{
    Paint::show(pfrm, app);

    vram_write_flag(pfrm, app.custom_flag_image_, Layer::map_0_ext);
}



ScenePtr<Scene>
FlagDesignerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::select)) {
        return scene_pool::alloc<FlagTemplateScene>();
        // load_default_flag(pfrm, app);
        // app.player_island().render_exterior(pfrm, app);
        // show(pfrm, app);
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (changed_) {
            app.custom_flag_image_.save(pfrm);
        }
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    app.player_island().update(pfrm, app, delta);

    update_entities(pfrm, app, delta, app.effects());

    return Paint::update(pfrm, app, delta);
}



void FlagDesignerModule::display(Platform& pfrm, App& app)
{
    return Paint::display(pfrm, app);
}



u8 FlagDesignerModule::get_pixel(App& app, u8 x, u8 y)
{
    if (x >= width() or y >= height()) {
        return 111;
    }
    return app.custom_flag_image_.pixels[x][y];
}



void FlagDesignerModule::set_pixel(App& app, u8 x, u8 y, u8 value)
{
    if (x >= width() or y >= height()) {
        return;
    }
    app.custom_flag_image_.pixels[x][y] = value;
    changed_ = true;
}



FlagDesignerModule::Factory FlagDesignerModule::factory_;



} // namespace skyland
