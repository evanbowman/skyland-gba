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



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
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



static const auto sel_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};



void CheckersModule::enter(Platform& pfrm, App& app, Scene& prev)
{
}



void CheckersModule::exit(Platform& pfrm, App& app, Scene& next)
{
}



void CheckersModule::init(Platform& pfrm, App& app)
{
    pfrm.speaker().play_music(app.environment().music(), 0);


    app.camera().emplace<macro::Camera>(pfrm);

    pfrm.load_background_texture("background_macro");
    // pfrm.system_call("parallax-clouds", false);

    pfrm.load_sprite_texture("spritesheet_macro");
    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 32; ++y) {
            pfrm.set_raw_tile(Layer::map_0, x, y, 0);
            pfrm.set_raw_tile(Layer::map_1, x, y, 0);
        }
    }


    macro::background_init(pfrm);


    pfrm.screen().set_view({});


    __draw_image(pfrm, 0, 0, 0, 30, 16, Layer::map_0);
    __draw_image(pfrm, 0, 0, 17, 30, 16, Layer::map_1);


    app.macrocosm().emplace();
    app.macrocosm()->emplace<macro::EngineImpl>(pfrm, app);


    pfrm.screen().set_shader(macro::fluid_shader);
    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    auto& m = macrocosm(app);
    m.data_->checkers_mode_ = true;
    m.data_->checkers_ai_moved_ = false;
    m.newgame(pfrm, app);
    app.game_mode() = App::GameMode::macro;

    m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild);
    auto bound = m.bind_sector({0, 1});

    if (auto s = dynamic_cast<macro::terrain::FreebuildSector*>(bound)) {
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
    }


    pfrm.system_call("vsync", nullptr);

    m.sector().render(pfrm);

    pfrm.sleep(1);
    pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
    pfrm.screen().schedule_fade(0.f);

    pfrm.system_call("_prlx_macro",
                     (void*)(intptr_t)(int)m.data_->cloud_scroll_);
}



ScenePtr<Scene>
CheckersModule::update(Platform& pfrm, App& app, Microseconds delta)
{

    init(pfrm, app);

    return scene_pool::alloc<macro::SelectorScene>();
}



CheckersModule::Factory CheckersModule::factory_;



} // namespace skyland
