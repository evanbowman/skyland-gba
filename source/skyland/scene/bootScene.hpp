#pragma once

#include "skyland/skyland.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/scene/introCreditsScene.hpp"
#include "version.hpp"



namespace skyland
{



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class BootScene : public Scene
{
public:

// clang-format off

static constexpr const char* console_header =
"\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n"
"|  Skyland Console                                                             |\r\n"
"*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\r\n";

// clang-format on



    static void init(Platform& pfrm)
    {
        pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);
        pfrm.screen().clear();
        pfrm.screen().display();


        pfrm.system_call("vsync", 0);
        pfrm.enable_glyph_mode(true);
        pfrm.load_overlay_texture("overlay");
        pfrm.load_tile1_texture("boot_img_flattened");


        __draw_image(pfrm, 1, 0, 3, 30, 12, Layer::map_1);

        pfrm.screen().schedule_fade(0.f);

        const auto st = calc_screen_tiles(pfrm);

        for (int x = 0; x < st.x; ++x) {
            pfrm.set_tile(Layer::overlay, x, 0, 112);
            pfrm.set_tile(Layer::overlay, x, 1, 112);
            pfrm.set_tile(Layer::overlay, x, 2, 112);

            pfrm.set_tile(Layer::overlay, x, st.y - 1, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 2, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 3, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 4, 112);
            pfrm.set_tile(Layer::overlay, x, st.y - 5, 112);
        }

        __draw_image(pfrm, 1, 0, 3, 30, 12, Layer::map_1);
        pfrm.screen().clear();
        pfrm.screen().display();


        pfrm.screen().schedule_fade(0.f);

        Text text(pfrm, {4, 1});
        text.append("_\\\\ SKYLAND engine //_",
                    FontColors{ColorConstant::silver_white,
                                   ColorConstant::rich_black});
        text.__detach();

        pfrm.screen().clear();
        pfrm.screen().display();

        pfrm.speaker().start();
        pfrm.speaker().play_sound("click_digital_1", 1);


        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        pfrm.sleep(2);
        Text version(pfrm, vn.c_str(), {1, u8(st.y - 4)});
        version.__detach();

        pfrm.screen().clear();
        pfrm.screen().display();

        pfrm.sleep(1);
    }


    static void message(Platform& pfrm, const char* text)
    {
        const auto st = calc_screen_tiles(pfrm);

        pfrm.system_call("vsync", 0);
        Text msg(pfrm, text, {1, u8(st.y - 2)});
        auto len = msg.len();
        for (int x = 0; x < st.x - len; ++x) {
            msg.append(" ");
        }
        msg.__detach();

        pfrm.screen().clear();
        pfrm.screen().display();
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta)
    {
        pfrm.load_background_texture(app.environment().background_texture());


        const auto st = calc_screen_tiles(pfrm);

        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        Text boot(pfrm, "booting...", {1, u8(st.y - 2)});
        pfrm.screen().clear();
        pfrm.screen().display();


        app.init_scripts(pfrm, [&](const char* text) {
                                   message(pfrm, text);
                               });

        message(pfrm, "doing stuff...");
        skyland::achievements::init(pfrm, app);

        message(pfrm, "lisp gc sweep...");
        lisp::gc();

        StringBuffer<320> msg(console_header);

        msg += "engine ";
        msg += vn;

        pfrm.remote_console().printline(msg.c_str());

        info(pfrm, vn.c_str());

        pfrm.fill_overlay(0);
        pfrm.screen().schedule_fade(1.f);
        pfrm.screen().clear();
        pfrm.screen().display();
        pfrm.sleep(10);

        return scene_pool::alloc<IntroCreditsScene>();
    }


};



}
