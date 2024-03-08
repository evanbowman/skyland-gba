#include "macrocosmLoaderModule.hpp"
#include "skyland/macroCamera.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/macro/helpScene.hpp"
#include "skyland/scene/macro/macroverseScene.hpp"
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



void background_init()
{
    for (int x = 0; x < 32; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 72);
        }
        for (int y = 0; y < 2; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 74);
        }
        for (int y = 2; y < 4; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 75);
        }
    }
    draw_image(4, 14, 17, 6, 3, Layer::background);
    draw_image(22, 9, 8, 2, 1, Layer::background);
    draw_image(24, 12, 5, 2, 1, Layer::background);
    PLATFORM.set_tile(Layer::background, 3, 4, 26);

    // Stars, for night view.
    PLATFORM.set_tile(Layer::background, 0, 1, 77);
    PLATFORM.set_tile(Layer::background, 16, 1, 78);
    PLATFORM.set_tile(Layer::background, 23, 0, 79);

    PLATFORM.set_tile(Layer::background, 11, 2, 80);
    PLATFORM.set_tile(Layer::background, 12, 3, 81);
    PLATFORM.set_tile(Layer::background, 21, 3, 82);
    PLATFORM.set_tile(Layer::background, 27, 2, 83);
}



} // namespace macro



void MacrocosmLoaderModule::enter(Scene& prev)
{
    PLATFORM.speaker().play_music(APP.environment().music(), 0);

    PLATFORM.load_overlay_texture("overlay_challenges");

    loading_text_.emplace(SYSTR(loading)->c_str(), OverlayCoord{1, 1});
}



ScenePtr MacrocosmLoaderModule::update(Time delta)
{
    if (skip_) {
        skip_ = false;
        return null_scene();
    }

    APP.camera().emplace<macro::Camera>();

    PLATFORM.load_background_texture("background_macro");
    // PLATFORM.system_call("parallax-clouds", false);

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

    auto& m = macrocosm();
    auto& sector = m.sector();

    m.load();
    APP.game_mode() = App::GameMode::macro;

    PLATFORM.system_call("vsync", nullptr);
    sector.render();

    loading_text_.reset();

    return make_scene<macro::MacroverseScene>();
}



} // namespace skyland
