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



void FlagDesignerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);
    pfrm.screen().fade(1.f);

    app.player_island().show_flag(true);


    pfrm.load_tile0_texture("tilesheet");


    Paint::init(pfrm, app);


    app.player_island().init_terrain(pfrm, 4);
    configure_island_from_codestring(
        pfrm, app, app.player_island(), "'((power-core 1 13))");

    app.player_island().render_exterior(pfrm, app);
    app.player_island().set_position({152, 370});


    GenericBird::spawn(
        pfrm, app, app.player_island(), rng::choice<3>(rng::utility_state));

    pfrm.screen().fade(0);

    show(pfrm, app);
}



void FlagDesignerModule::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
    pfrm.screen().fade(1.f);
}



void FlagDesignerModule::show(Platform& pfrm, App& app)
{
    Paint::show(pfrm, app);

    vram_write_flag(pfrm, app.gp_.flag_img_);
}



ScenePtr<Scene>
FlagDesignerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::action_2)) {
        save::store_global_data(pfrm, app.gp_);
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
    return app.gp_.flag_img_.pixels[x][y];
}



void FlagDesignerModule::set_pixel(App& app, u8 x, u8 y, u8 value)
{
    if (x >= width() or y >= height()) {
        return;
    }
    app.gp_.flag_img_.pixels[x][y] = value;
    changed_ = true;
}



FlagDesignerModule::Factory FlagDesignerModule::factory_;



} // namespace skyland
