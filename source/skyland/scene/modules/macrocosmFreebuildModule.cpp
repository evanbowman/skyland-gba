#include "macrocosmFreebuildModule.hpp"
#include "skyland/macroCamera.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/macrocosmFreebuildFlatSector.hpp"
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



void MacrocosmFreebuildModule::enter(Scene& prev)
{
    PLATFORM.screen().schedule_fade(0.f);
    PLATFORM.screen().schedule_fade(1.f);

    auto prompt_str = SYSTR(freebuild_terrain_size);
    u8 margin = centered_text_margins(utf8::len(prompt_str->c_str()));

    prompt_.emplace(prompt_str->c_str(), OverlayCoord{margin, 4});

    t1_.emplace(SYSTR(macro_deep)->c_str(), OverlayCoord{3, 7});
    t1_->append(" 10x10x8");

    t2_.emplace(SYSTR(macro_wide)->c_str(), OverlayCoord{3, 9});
    t2_->append(" 12x12x5");

    t3_.emplace(SYSTR(macro_superflat)->c_str(), OverlayCoord{3, 11});
    t3_->append(" 14x14x4");
}



void MacrocosmFreebuildModule::exit(Scene& prev)
{
    prompt_.reset();
    t1_.reset();
    t2_.reset();
    t3_.reset();

    PLATFORM.fill_overlay(0);
}



void MacrocosmFreebuildModule::init()
{
    auto fd = PLATFORM.load_file("scripts/data", "environment.ini");
    if (not fd.second) {
        PLATFORM.fatal("missing env config file!");
    }

    Conf c;
    auto v = c.get(fd.first, "macrocosm", "music");

    PLATFORM.speaker().stream_music((*std::get_if<Conf::String>(&v))->c_str(),
                                    0);


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
    m.data_->freebuild_mode_ = true;
    //m.newgame();
    APP.game_mode() = App::GameMode::macro;

    switch (size_sel_) {
    case 0: {
        m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild);
        auto bound = m.bind_sector({0, 1});

        // FIXME: had to eliminate RTTI, add back checked cast.
        if (auto s = static_cast<macro::terrain::FreebuildSector*>(bound)) {
            s->reset();
        }
        break;
    }

    case 1: {
        m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild_wide);
        auto bound = m.bind_sector({0, 1});

        if (auto s = static_cast<macro::terrain::FreebuildWideSector*>(bound)) {
            s->reset();
        }
        break;
    }

    case 2: {
        m.make_sector({0, 1}, macro::terrain::Sector::Shape::freebuild_flat);
        auto bound = m.bind_sector({0, 1});

        if (auto s = static_cast<macro::terrain::FreebuildFlatSector*>(bound)) {
            s->reset();
        }
        break;
    }
    }

    PLATFORM_EXTENSION(force_vsync);

    m.sector().render();

    PLATFORM.sleep(1);
    PLATFORM.screen().schedule_fade(0.7f, {custom_color(0x102447)});
    PLATFORM.screen().schedule_fade(0.f);

    PLATFORM_EXTENSION(update_parallax_macro, macrocosm().data_->cloud_scroll_);
}



ScenePtr MacrocosmFreebuildModule::update(Time delta)
{
    // if (not APP.gp_.stateflags_.get(GlobalPersistentData::freebuild_unlocked)) {
    //     auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
    //     *buffer = SYSTR(freebuild_locked_text)->c_str();
    //     return make_scene<FullscreenDialogScene>(std::move(buffer), [] {
    //         return make_scene<TitleScreenScene>(3);
    //     });
    // }

    if (PLATFORM.device_name() == "MacroDesktopDemo" or
        APP.player().key_down(Key::action_1)) {

        if (APP.player().key_down(Key::action_1)) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
        } else {
            size_sel_ = 2;
        }

        init();

        auto next = make_scene<macro::SelectorScene>();
        next->show_island_size();
        return next;
    }

    if (APP.player().key_down(Key::up)) {
        if (size_sel_ > 0) {
            --size_sel_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
        }
    }

    if (APP.player().key_down(Key::down)) {
        if (size_sel_ < 2) {
            ++size_sel_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
        }
    }

    switch (size_sel_) {
    case 0:
        PLATFORM.set_tile(Layer::overlay, 1, 7, 396);
        PLATFORM.set_tile(Layer::overlay, 1, 9, 0);
        PLATFORM.set_tile(Layer::overlay, 1, 11, 0);
        break;

    case 1:
        PLATFORM.set_tile(Layer::overlay, 1, 7, 0);
        PLATFORM.set_tile(Layer::overlay, 1, 9, 396);
        PLATFORM.set_tile(Layer::overlay, 1, 11, 0);
        break;

    case 2:
        PLATFORM.set_tile(Layer::overlay, 1, 7, 0);
        PLATFORM.set_tile(Layer::overlay, 1, 9, 0);
        PLATFORM.set_tile(Layer::overlay, 1, 11, 396);
        break;
    }


    return null_scene();
}



// MacrocosmFreebuildModule::Factory MacrocosmFreebuildModule::factory_;



} // namespace skyland
