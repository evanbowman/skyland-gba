#include "checkersModule.hpp"
#include "skyland/macroCamera.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/macrocosmFreebuildSector.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/macro/helpScene.hpp"
#include "skyland/scene/macro/selectorScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/storm.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



namespace macro
{
ColorConstant
fluid_shader(ShaderPalette p, ColorConstant k, int var, int index);
}



void CheckersModule::enter(Scene& prev)
{
}



void CheckersModule::exit(Scene& next)
{
}



void CheckersModule::init()
{
    PLATFORM.speaker().stream_music(APP.environment().music()->c_str(), 0);


    APP.camera().emplace<macro::Camera>();

    PLATFORM.load_background_texture("background_macro");

    PLATFORM.load_sprite_texture("spritesheet_macro");
    PLATFORM.load_tile0_texture("macro_rendertexture");
    PLATFORM.load_tile1_texture("macro_rendertexture");

    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 32; ++y) {
            PLATFORM.set_raw_tile(Layer::map_0, x, y, 0);
            PLATFORM.set_raw_tile(Layer::map_1, x, y, 0);
        }
    }


    macro::background_init();


    PLATFORM.screen().set_view({});


    __draw_image(0, 0, 0, 30, 16, Layer::map_0);
    __draw_image(0, 0, 17, 30, 16, Layer::map_1);


    APP.macrocosm().emplace();
    APP.macrocosm()->emplace<macro::EngineImpl>(&APP);


    PLATFORM.screen().set_shader(macro::fluid_shader);
    PLATFORM.load_tile0_texture("macro_rendertexture");
    PLATFORM.load_tile1_texture("macro_rendertexture");

    auto& m = macrocosm();
    m.data_->checkers_mode_ = true;
    m.data_->checkers_ai_moved_ = false;
    m.newgame();
    APP.game_mode() = App::GameMode::macro;

    m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild);
    auto bound = m.bind_sector({0, 1});

    bound->set_cursor({0, 0, 1});

    // FIXME: I was using dynamic_cast previously... needs to be checked cast,
    // or method call.
    if (auto s = static_cast<macro::terrain::FreebuildSector*>(bound)) {
        s->reset();

        for (u8 x = 0; x < 10; ++x) {
            for (u8 y = 0; y < 10; ++y) {
                if (x == 0 or y == 0 or x == 9 or y == 9) {
                    s->set_block({x, y, 0}, macro::terrain::Type::arch);
                } else {
                    if (x % 2 == 0) {
                        if (y % 2 == 0) {
                            s->set_block({x, y, 0},
                                         macro::terrain::Type::scaffolding);
                            if (x < 4) {
                                s->set_block({x, y, 1},
                                             macro::terrain::Type::checker_red);
                            } else if (x >= 6) {
                                s->set_block(
                                    {x, y, 1},
                                    macro::terrain::Type::checker_black);
                            } else {
                                s->set_block({x, y, 1},
                                             macro::terrain::Type::air);
                            }
                        } else {
                            s->set_block({x, y, 0},
                                         macro::terrain::Type::masonry);
                            s->set_block({x, y, 1}, macro::terrain::Type::air);
                        }
                    } else {
                        if (y % 2 == 0) {
                            s->set_block({x, y, 0},
                                         macro::terrain::Type::masonry);
                            s->set_block({x, y, 1}, macro::terrain::Type::air);
                        } else {
                            s->set_block({x, y, 0},
                                         macro::terrain::Type::scaffolding);
                            if (x < 4) {
                                s->set_block({x, y, 1},
                                             macro::terrain::Type::checker_red);
                            } else if (x >= 6) {
                                s->set_block(
                                    {x, y, 1},
                                    macro::terrain::Type::checker_black);
                            } else {
                                s->set_block({x, y, 1},
                                             macro::terrain::Type::air);
                            }
                        }
                    }
                }
            }
        }

        s->set_block({0, 0, 0}, macro::terrain::Type::madder);
        s->set_block({0, 9, 0}, macro::terrain::Type::madder);

        s->set_block({9, 0, 0}, macro::terrain::Type::indigo);
        s->set_block({9, 9, 0}, macro::terrain::Type::indigo);

        s->set_cursor({5, 5, 2});

        s->rotate();
        s->rotate();
        s->rotate();
        s->rotate();
    }


    PLATFORM_EXTENSION(force_vsync);

    m.sector().render();

    PLATFORM.sleep(1);
    PLATFORM.screen().schedule_fade(0.7f, {custom_color(0x102447)});
    PLATFORM.screen().schedule_fade(0.f);

    PLATFORM_EXTENSION(update_parallax_macro, macrocosm().data_->cloud_scroll_);
}



ScenePtr CheckersModule::update(Time delta)
{

    init();

    return make_scene<macro::SelectorScene>();
}



} // namespace skyland
