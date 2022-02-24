#include "modifierKeyHintScene.hpp"
#include "assignWeaponGroupScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



ScenePtr<Scene> update_modifier_keys(Platform& pfrm, App& app);



ScenePtr<Scene>
ModifierKeyHintScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (not player(app).key_pressed(pfrm, Key::start)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (auto scene = update_modifier_keys(pfrm, app)) {
        return scene;
    }

    return null_scene();
}



void ModifierKeyHintScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    const auto st = calc_screen_tiles(pfrm);

    for (int x = 4; x < st.x - 4; ++x) {
        for (int y = 3; y < st.y - 3; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 112);
        }
    }

    pfrm.set_tile(Layer::overlay, 5, 7, 392);
    pfrm.set_tile(Layer::overlay, 5, 9, 393);
    pfrm.set_tile(Layer::overlay, 5, 11, 394);
    pfrm.set_tile(Layer::overlay, 5, 13, 395);

    auto title = SYSTR(modifier_keys_title);
    title_.emplace(
        pfrm,
        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(title->c_str())),
                     4});

    text_.emplace_back(
        pfrm, SYSTR(modifier_keys_opt_1)->c_str(), OverlayCoord{7, 7});

    text_.emplace_back(
        pfrm, SYSTR(modifier_keys_opt_2)->c_str(), OverlayCoord{7, 9});

    text_.emplace_back(
        pfrm, SYSTR(modifier_keys_opt_3)->c_str(), OverlayCoord{7, 11});

    text_.emplace_back(
        pfrm, SYSTR(modifier_keys_opt_4)->c_str(), OverlayCoord{7, 13});

    pfrm.screen().schedule_fade(0.5f);
}



void ModifierKeyHintScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    pfrm.screen().schedule_fade(0.f);

    pfrm.fill_overlay(0);
}



} // namespace skyland
