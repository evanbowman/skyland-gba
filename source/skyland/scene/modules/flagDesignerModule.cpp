#include "flagDesignerModule.hpp"
#include "platform/platform.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void FlagDesignerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);
    pfrm.screen().fade(1.f);

    app.player_island().show_flag(true);


    pfrm.load_tile0_texture("tilesheet");


    Paint::init(pfrm, app);


    app.player_island().init_terrain(pfrm, 4);
    configure_island_from_codestring(
        pfrm, app, app.player_island(), "'((power-core 1 13))");

    app.player_island().repaint(pfrm, app);
    app.player_island().set_position({152, 370});


    pfrm.screen().fade(0);
}



void FlagDesignerModule::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
    pfrm.screen().fade(1.f);
}



void FlagDesignerModule::show(Platform& pfrm, App& app)
{
    Paint::show(pfrm, app);

    vram_write_flag(pfrm, app.gp_.flag_img_);
}



ScenePtr<Scene>
FlagDesignerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::action_2)) {
        save::store_global_data(pfrm, app.gp_);
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    return Paint::update(pfrm, app, delta);
}



void FlagDesignerModule::display(Platform& pfrm, App& app)
{
    return Paint::display(pfrm, app);
}



u8 FlagDesignerModule::get_pixel(App& app, u8 x, u8 y)
{
    if (x >= width() or y >= height()) {
        return 111;
    }
    return app.gp_.flag_img_.pixels[x][y];
}



void FlagDesignerModule::set_pixel(App& app, u8 x, u8 y, u8 value)
{
    if (x >= width() or y >= height()) {
        return;
    }
    app.gp_.flag_img_.pixels[x][y] = value;
}



FlagDesignerModule::Factory FlagDesignerModule::factory_;



} // namespace skyland
