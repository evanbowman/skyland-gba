#include "macrocosmLoaderModule.hpp"
#include "skyland/player/player.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/macro/selectorScene.hpp"



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

    scroll_ = 42;
    pfrm.set_scroll(Layer::map_0, 0, scroll_);
    pfrm.set_scroll(Layer::map_1, 0, scroll_ + 8);

    pfrm.load_background_texture("background_macro");
    // pfrm.system_call("parallax-clouds", false);

    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    pfrm.screen().set_view({});


    __draw_image(pfrm, 0, 0, 0, 30, 16, Layer::map_0);
    __draw_image(pfrm, 0, 0, 17, 30, 16, Layer::map_1);


    app.macrocosm().emplace();
    auto& sector = app.macrocosm()->data_->sector_;

    sector.set_block({1, 3, 1}, macro::terrain::Type::building);
    sector.set_block({1, 4, 1}, macro::terrain::Type::building);

    sector.set_block({0, 6, 1}, macro::terrain::Type::building);
    sector.set_block({0, 1, 1}, macro::terrain::Type::rock_stacked);
    sector.set_block({0, 1, 2}, macro::terrain::Type::rock_stacked);

    sector.set_block({0, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({0, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({0, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({0, 4, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({0, 5, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({0, 6, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({0, 7, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({1, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({1, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({1, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({1, 4, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({1, 5, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({1, 6, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({1, 7, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({2, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 4, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 5, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 6, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({2, 7, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({3, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 4, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 5, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 6, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({3, 7, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({4, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 4, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 5, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 6, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({4, 7, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({5, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 4, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 5, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 6, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({5, 7, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({6, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({6, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({6, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({6, 3, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({7, 0, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({7, 1, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({7, 2, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({7, 3, 0}, macro::terrain::Type::rock_edge);
    sector.set_block({7, 4, 0}, macro::terrain::Type::rock_edge);

    sector.set_block({7, 1, 5}, macro::terrain::Type::rock_edge);
    sector.set_block({7, 2, 5}, macro::terrain::Type::rock_edge);
    sector.set_block({7, 2, 1}, macro::terrain::Type::building);
    sector.set_block({7, 3, 5}, macro::terrain::Type::rock_edge);
    sector.set_block({6, 1, 5}, macro::terrain::Type::rock_edge);
    sector.set_block({6, 2, 5}, macro::terrain::Type::rock_edge);

    sector.set_block({7, 0, 1}, macro::terrain::Type::rock_stacked);
    sector.set_block({7, 0, 2}, macro::terrain::Type::rock_stacked);
    sector.set_block({7, 0, 3}, macro::terrain::Type::rock_stacked);
    sector.set_block({7, 0, 4}, macro::terrain::Type::rock_stacked);
    sector.set_block({7, 0, 5}, macro::terrain::Type::rock_stacked);
    sector.set_block({7, 0, 6}, macro::terrain::Type::rock_stacked);
    sector.set_block({7, 0, 7}, macro::terrain::Type::rock_stacked);

    sector.set_cursor({3, 2, 1});
    sector.population_ = 8;


    pfrm.screen().schedule_fade(0.f);
}






ScenePtr<Scene>
MacrocosmLoaderModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    return scene_pool::alloc<macro::SelectorScene>();
}



MacrocosmLoaderModule::Factory MacrocosmLoaderModule::factory_(false);



} // namespace skyland
