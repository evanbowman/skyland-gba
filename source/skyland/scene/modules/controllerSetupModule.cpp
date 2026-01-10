////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "controllerSetupModule.hpp"
#include "containers/vector.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/dlc.hpp"
#include "skyland/scene/fullscreenDialogChainScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr ControllerSetupModuleInit::update(Time delta)
{
    APP.invoke_script("/scripts/config/controller-setup.lisp");
    return make_scene<FullscreenDialogChainScene>();
}



ControllerSetupModuleInit::Factory ControllerSetupModuleInit::factory_;




void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void ControllerSetupModule::enter(Scene& prev)
{
    PLATFORM.speaker().set_music_volume(8);


    PLATFORM.screen().set_shader(passthrough_shader);
    PLATFORM.load_tile0_texture("button_mapping_flattened");
    PLATFORM.screen().schedule_fade(0.f, {custom_color(0x163061)});

    for (int y = 0; y < 16; ++y) {
        for (int x = 0; x < 16; ++x) {
            PLATFORM.set_tile(Layer::map_0_ext, x, y, 1);
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }
    __draw_image(31, 0, 2, 30, 13, Layer::map_0);

    settings::load(settings_);
    repaint();

    auto title = loadstr(SystemString::module_button_mapping);
    auto mg = centered_text_margins(utf8::len(title->c_str()));
    Text::print(title->c_str(), {(u8)mg, 1});
}



static const char* const key_names[] = {
    "key_alt1",
    "key_up",
    "key_left",
    "key_down",
    "key_right",
    "key_start",
    "key_select",
    "key_alt2",
    "key_action1",
    "key_action2"
};



void ControllerSetupModule::repaint()
{
    PLATFORM.fill_overlay(0);

    __draw_image(31, 0, 2, 30, 13, Layer::map_0);

    static constexpr const Text::OptColors text_colors{
        {custom_color(0xffffff), ColorConstant::spanish_crimson}};


#define GET_S(STR) (settings_.get(STR).c_str())
    auto print_align_left = [&](const char* str, OverlayCoord c, int idx) {
        auto clr = text_colors;
        if (idx not_eq key_index_) {
            clr.reset();
        }
        auto len = utf8::len(str);
        c.x -= len;
        Text::print(str, c, clr);
    };
    print_align_left(GET_S(key_names[0]), {8, 4}, 0);
    print_align_left(GET_S(key_names[1]), {8, 6}, 1);
    print_align_left(GET_S(key_names[2]), {8, 8}, 2);
    print_align_left(GET_S(key_names[3]), {8, 10}, 3);
    print_align_left(GET_S(key_names[4]), {8, 12}, 4);

    auto print_align_right = [&](const char* str, OverlayCoord c, int idx) {
        auto clr = text_colors;
        if (idx not_eq key_index_) {
            clr.reset();
        }
        Text::print(str, c, clr);
    };

    print_align_right(GET_S(key_names[5]), {13, 13}, 5);
    print_align_right(GET_S(key_names[6]), {11, 15}, 6);
    print_align_right(GET_S(key_names[7]), {22, 4}, 7);
    print_align_right(GET_S(key_names[8]), {22, 6}, 8);
    print_align_right(GET_S(key_names[9]), {22, 9}, 9);
}



void ControllerSetupModule::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);

    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);

    show_island_exterior(&player_island());
}



ScenePtr process_script_menu_request();



ScenePtr ControllerSetupModule::update(Time delta)
{
    if (exit_) {
        PLATFORM.screen().schedule_fade(1.f);
        if (next_) {
            return (*next_)();
        } else {
            return make_scene<TitleScreenScene>(3);
        }
    }

    if (key_index_ > -1) {
        while (true) {
            PLATFORM.screen().clear();
            if (auto k = PLATFORM.keyboard().check_key()) {
                if (contains(used_scancodes_, k)) {
                    PLATFORM.speaker().play_sound("beep_error", 3);
                } else {
                    settings_.set(key_names[key_index_], k);
                    ++key_index_;
                    repaint();
                    used_scancodes_.push_back(k);
                    if (key_index_ == 10) {
                        exit_ = true;
                        PLATFORM.screen().schedule_fade(1.f);
                        if (on_select_) {
                            (*on_select_)(settings_.data_);
                        }
                        break;
                    }
                }
            }
            PLATFORM.screen().display();
        }
    }

    if (key_index_ > -1 or PLATFORM.keyboard().down_transition<Key::action_2>()) {
        PLATFORM.screen().schedule_fade(1.f);
        if (next_) {
            return (*next_)();
        } else if (auto scn = process_script_menu_request()) {
            return scn;
        }
    }

    return null_scene();
}



} // namespace skyland
