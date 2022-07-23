#include "macrocosmFreebuildModule.hpp"
#include "skyland/macroCamera.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/macrocosmFreebuildSector.hpp"
#include "skyland/macrocosmFreebuildWideSector.hpp"
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



void MacrocosmFreebuildModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().schedule_fade(0.f);
    pfrm.screen().schedule_fade(1.f);

    auto prompt_str = SYSTR(freebuild_terrain_size);
    u8 margin = centered_text_margins(pfrm, utf8::len(prompt_str->c_str()));

    prompt_.emplace(pfrm, prompt_str->c_str(), OverlayCoord{margin, 4});

    t1_.emplace(pfrm, SYSTR(macro_deep)->c_str(), OverlayCoord{3, 7});
    t1_->append(" 10x10x8");

    t2_.emplace(pfrm, SYSTR(macro_wide)->c_str(), OverlayCoord{3, 9});
    t2_->append(" 12x12x5");
}



void MacrocosmFreebuildModule::exit(Platform& pfrm, App& app, Scene& prev)
{
    prompt_.reset();
    t1_.reset();
    t2_.reset();

    pfrm.fill_overlay(0);
}



void MacrocosmFreebuildModule::init(Platform& pfrm, App& app)
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
    app.macrocosm()->emplace<macro::EngineImpl>(pfrm, &app);


    pfrm.screen().set_shader(macro::fluid_shader);
    pfrm.load_tile0_texture("macro_rendertexture");
    pfrm.load_tile1_texture("macro_rendertexture");

    auto& m = macrocosm(app);
    m.data_->freebuild_mode_ = true;
    m.newgame(pfrm, app);
    app.game_mode() = App::GameMode::macro;

    switch (size_sel_) {
    case 0: {
        m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild);
        auto bound = m.bind_sector({0, 1});

        if (auto s = dynamic_cast<macro::terrain::FreebuildSector*>(bound)) {
            s->reset();
        }
        break;
    }

    case 1: {
        m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild_wide);
        auto bound = m.bind_sector({0, 1});

        if (auto s =
                dynamic_cast<macro::terrain::FreebuildWideSector*>(bound)) {
            s->reset();
        }
        break;
    }
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
MacrocosmFreebuildModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.gp_.stateflags_.get(GlobalPersistentData::freebuild_unlocked)) {
        auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
        *buffer = SYSTR(freebuild_locked_text)->c_str();
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer), [] {
            return scene_pool::alloc<TitleScreenScene>(3);
        });
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        pfrm.speaker().play_sound("button_wooden", 3);

        init(pfrm, app);

        auto next = scene_pool::alloc<macro::SelectorScene>();
        next->show_island_size();
        return next;
    }

    if (app.player().key_down(pfrm, Key::up)) {
        if (size_sel_ > 0) {
            --size_sel_;
            pfrm.speaker().play_sound("click_wooden", 2);
        }
    }

    if (app.player().key_down(pfrm, Key::down)) {
        if (size_sel_ < 1) {
            ++size_sel_;
            pfrm.speaker().play_sound("click_wooden", 2);
        }
    }

    switch (size_sel_) {
    case 0:
        pfrm.set_tile(Layer::overlay, 1, 7, 396);
        pfrm.set_tile(Layer::overlay, 1, 9, 0);
        break;

    case 1:
        pfrm.set_tile(Layer::overlay, 1, 7, 0);
        pfrm.set_tile(Layer::overlay, 1, 9, 396);
        break;
    }


    return null_scene();
}



MacrocosmFreebuildModule::Factory MacrocosmFreebuildModule::factory_;



} // namespace skyland
