#include "macrocosmLoaderModule.hpp"
#include "skyland/macroCamera.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/macro/selectorScene.hpp"
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



void MacrocosmLoaderModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.speaker().play_music(app.environment().music(), 0);


    app.camera().emplace<macro::Camera>(pfrm);

    pfrm.load_background_texture("background_macro");
    // pfrm.system_call("parallax-clouds", false);

    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 20; ++y) {
            pfrm.set_tile(Layer::background, x, y, 72);
        }
        for (int y = 0; y < 2; ++y) {
            pfrm.set_tile(Layer::background, x, y, 74);
        }
        for (int y = 2; y < 4; ++y) {
            pfrm.set_tile(Layer::background, x, y, 75);
        }
    }
    draw_image(pfrm, 4, 14, 17, 6, 3, Layer::background);
    draw_image(pfrm, 22, 9, 8, 2, 1, Layer::background);
    draw_image(pfrm, 24, 12, 5, 2, 1, Layer::background);
    pfrm.set_tile(Layer::background, 3, 4, 26);


    pfrm.screen().set_view({});


    __draw_image(pfrm, 0, 0, 0, 30, 16, Layer::map_0);
    __draw_image(pfrm, 0, 0, 17, 30, 16, Layer::map_1);


    app.macrocosm().emplace();
    auto& sector = app.macrocosm()->sector();


    sector.set_block({3, 3, 0}, macro::terrain::Type::terrain);
    sector.set_block({3, 2, 0}, macro::terrain::Type::terrain);
    sector.set_block({2, 3, 0}, macro::terrain::Type::terrain);
    sector.set_block({3, 4, 0}, macro::terrain::Type::terrain);
    sector.set_block({4, 3, 0}, macro::terrain::Type::terrain);
    sector.set_block({4, 4, 0}, macro::terrain::Type::terrain);
    sector.set_block({2, 2, 0}, macro::terrain::Type::masonry);
    sector.set_block({4, 2, 0}, macro::terrain::Type::masonry);
    sector.set_block({3, 3, 1}, macro::terrain::Type::building);

    sector.set_cursor({3, 3, 1});
    sector.population_ = 8;

    app.macrocosm()->data_->coins_ = 160;


    pfrm.screen().schedule_fade(0.f);
}



ScenePtr<Scene>
MacrocosmLoaderModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    return scene_pool::alloc<macro::SelectorScene>();
}



MacrocosmLoaderModule::Factory MacrocosmLoaderModule::factory_(false);



} // namespace skyland
