#include "salvageRoomScene.hpp"
#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene>
SalvageRoomScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    if (pfrm.keyboard().down_transition<Key::action_1>()) {
        if (auto room = app.player_island().get_room(cursor_loc)) {
            if (auto mt = room->metaclass()) {
                app.coins() += (*mt)->cost() * 0.75f;
            }
            app.player_island().destroy_room(pfrm, cursor_loc);
        }
        return scene_pool::alloc<ReadyScene>();
    }

    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
