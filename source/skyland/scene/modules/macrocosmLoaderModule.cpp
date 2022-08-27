#include "macrocosmLoaderModule.hpp"
#include "skyland/macroCamera.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/macro/helpScene.hpp"
#include "skyland/scene/macro/macroverseScene.hpp"
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



void background_init(Platform& pfrm)
{
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

    // Stars, for night view.
    pfrm.set_tile(Layer::background, 0, 1, 77);
    pfrm.set_tile(Layer::background, 16, 1, 78);
    pfrm.set_tile(Layer::background, 23, 0, 79);

    pfrm.set_tile(Layer::background, 11, 2, 80);
    pfrm.set_tile(Layer::background, 12, 3, 81);
    pfrm.set_tile(Layer::background, 21, 3, 82);
    pfrm.set_tile(Layer::background, 27, 2, 83);
}



} // namespace macro



void MacrocosmLoaderModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.speaker().play_music(app.environment().music(), 0);

    pfrm.load_overlay_texture("overlay_challenges");

    loading_text_.emplace(pfrm, SYSTR(loading)->c_str(), OverlayCoord{1, 1});
}



ScenePtr<Scene>
MacrocosmLoaderModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (skip_) {
        skip_ = false;
        return null_scene();
    }

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
    app.macrocosm()->emplace<macro::EngineImpl>(pfrm, &app);

    auto& m = macrocosm(app);
    auto& sector = m.sector();

    m.load(pfrm, app);
    app.game_mode() = App::GameMode::macro;

    pfrm.system_call("vsync", nullptr);
    sector.render(pfrm);

    loading_text_.reset();

    return scene_pool::alloc<macro::MacroverseScene>();
}



} // namespace skyland
