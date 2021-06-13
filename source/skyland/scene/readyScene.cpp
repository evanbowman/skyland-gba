#include "readyScene.hpp"
#include "constructionScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "worldScene.hpp"
#include "skyland/skyland.hpp"
#include "globals.hpp"
#include "salvageRoomScene.hpp"



namespace skyland {



ScenePtr<Scene> ReadyScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto scene = WorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (pfrm.keyboard().down_transition<Key::alt_2>()) {
        return scene_pool::alloc<ConstructionScene>();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    if (pfrm.keyboard().down_transition<Key::left>()) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
        }
    }

    if (pfrm.keyboard().down_transition<Key::right>()) {
        if (cursor_loc.x < app.player_island().terrain().size()) {
            ++cursor_loc.x;
        }
    }

    if (pfrm.keyboard().down_transition<Key::up>()) {
        if (cursor_loc.y > 0) {
            --cursor_loc.y;
        }
    }

    if (pfrm.keyboard().down_transition<Key::down>()) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
        }
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }

    if (pfrm.keyboard().down_transition<Key::action_1>()) {
        if (auto room = app.player_island().get_room(cursor_loc)) {
            pfrm.sleep(30);
            return room->select();
        }
    }

    if (pfrm.keyboard().down_transition<Key::action_2>()) {
        if (auto room = app.player_island().get_room(cursor_loc)) {
            room->set_injured(pfrm);
            // return scene_pool::alloc<SalvageRoomScene>();
        }
    }

    return null_scene();
}



void ReadyScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(15 + cursor_anim_frame_);

    auto origin = app.player_island().origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    origin.x += cursor_loc.x * 16;
    origin.y += cursor_loc.y * 16;

    cursor.set_position(origin);

    // pfrm.screen().draw(cursor);
}



} // namespace skyland
