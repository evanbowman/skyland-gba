#include "modifierKeyHintScene.hpp"
#include "assignWeaponGroupScene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {



ScenePtr<Scene>
ModifierKeyHintScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    if (not player(app).key_pressed(pfrm, Key::start)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (player(app).key_down(pfrm, Key::down)) {
        player(app).key_held_reset(Key::down, milliseconds(1500));
        return scene_pool::alloc<AssignWeaponGroupScene>();
    } else if (player(app).key_down(pfrm, Key::up)) {
        for (auto& room : player_island(app).rooms()) {
            if (room->group() == Room::Group::one) {
                if (auto scene = room->select(pfrm,
                                              app,
                                              room->position())) {
                    player(app).key_held_reset(Key::up, milliseconds(500));
                    return scene;
                }
            }
        }
    } else if (player(app).key_down(pfrm, Key::right)) {
        for (auto& room : player_island(app).rooms()) {
            if (room->group() == Room::Group::two) {
                if (auto scene = room->select(pfrm,
                                              app,
                                              room->position())) {
                    player(app).key_held_reset(Key::right, milliseconds(500));
                    return scene;
                }
            }
        }
    } else if (player(app).key_down(pfrm, Key::left)) {
        for (auto& room : player_island(app).rooms()) {
            if (room->group() == Room::Group::three) {
                if (auto scene = room->select(pfrm,
                                              app,
                                              room->position())) {
                    player(app).key_held_reset(Key::left, milliseconds(500));
                    return scene;
                }
            }
        }
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

    const char* title = "modifier keys";
    title_.emplace(
        pfrm,
        title,
        OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(title)), 4});

    text_.emplace_back(pfrm, "set weapon groups", OverlayCoord{7, 7});
    text_.emplace_back(pfrm, "weapon group 1", OverlayCoord{7, 9});
    text_.emplace_back(pfrm, "weapon group 2", OverlayCoord{7, 11});
    text_.emplace_back(pfrm, "weapon group 3", OverlayCoord{7, 13});

    pfrm.screen().schedule_fade(0.5f);
}



void ModifierKeyHintScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    pfrm.screen().schedule_fade(0.f);

    pfrm.fill_overlay(0);
}



} // namespace skyland
