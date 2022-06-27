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


#include "developerModeModule.hpp"
#include "containers/vector.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/dlc.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);


static const FontColors text_colors{custom_color(0xcec6ef),
                                    custom_color(0x163061)};


static const FontColors dev_opt_colors = {
    custom_color(0x163061),
    custom_color(0x45c496),
};



void DeveloperModeModule::set_opt(Platform& pfrm, bool value)
{
    option_ = value;

    if (option_) {
        option_text_->assign("< ", text_colors);
        option_text_->append(SYSTR(yes)->c_str(), dev_opt_colors);
        option_text_->append(" >", text_colors);
    } else {
        option_text_->assign("< ", text_colors);
        option_text_->append(SYSTR(no)->c_str(), dev_opt_colors);
        option_text_->append(" >", text_colors);
    }
}



void DeveloperModeModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    message_.emplace(pfrm);
    message_overflow_.emplace(pfrm);

    was_developer_mode_ = app.is_developer_mode();

    option_text_.emplace(pfrm, OverlayCoord{3, 16});
    set_opt(pfrm, was_developer_mode_);

    auto str = SYSTR(developer_mode_msg);

    pfrm.speaker().set_music_volume(8);


    message_->assign(str->c_str(), {1, 1}, {28, 6}, 0, text_colors);

    message_overflow_->assign(
        str->c_str() + message_->parsed(), {1, 7}, {12, 9}, 0, text_colors);


    pfrm.screen().set_shader(passthrough_shader);
    pfrm.load_tile0_texture("developer_mode_flattened");
    pfrm.screen().schedule_fade(0.f, custom_color(0x163061));

    for (int y = 0; y < 16; ++y) {
        for (int x = 0; x < 16; ++x) {
            pfrm.set_tile(Layer::map_0_ext, x, y, 1);
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }
    __draw_image(pfrm, 31, 0, 6, 30, 13, Layer::map_0);
}



void DeveloperModeModule::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.fill_overlay(0);

    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);

    show_island_exterior(pfrm, app, &player_island(app));
}



ScenePtr<Scene>
DeveloperModeModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (exit_) {
        pfrm.screen().schedule_fade(1.f);
        return scene_pool::alloc<TitleScreenScene>(3);
    }


    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().key_down(pfrm, Key::action_2)) {

        if (was_developer_mode_ not_eq option_) {
            app.set_developer_mode(not app.is_developer_mode());
            save::store_global_data(pfrm, app.gp_);
        }

        message_.reset();
        message_overflow_.reset();
        option_text_.reset();
        exit_ = true;
        pfrm.screen().schedule_fade(1.f);
    }

    if (app.player().key_down(pfrm, Key::right) or
        app.player().key_down(pfrm, Key::left)) {
        set_opt(pfrm, not option_);
    }

    return null_scene();
}



DeveloperModeModule::Factory DeveloperModeModule::factory_;



} // namespace skyland
