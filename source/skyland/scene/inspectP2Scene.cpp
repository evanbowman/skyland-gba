#include "inspectP2Scene.hpp"
#include "constructionScene.hpp"
#include "keyComboScene.hpp"
#include "globals.hpp"
#include "lispReplScene.hpp"
#include "readyScene.hpp"
#include "salvageDroneScene.hpp"
#include "salvageRoomScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void InspectP2Scene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    far_camera();
}



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



void InspectP2Scene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    clear_room_description(pfrm, room_description_);
}



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description);



ScenePtr<Scene>
InspectP2Scene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }


    if (not app.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    cursor_anim_timer_ += delta;
    if (cursor_anim_timer_ > milliseconds(200)) {
        cursor_anim_timer_ -= milliseconds(200);
        cursor_anim_frame_ = not cursor_anim_frame_;
    }


    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).far_cursor_loc_;


    if (app.player().key_down(pfrm, Key::left)) {
        if (cursor_loc.x > 0) {
            --cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        } else {
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y =
                cursor_loc.y;
            return scene_pool::alloc<ReadyScene>();
        }
    }

    if (app.player().key_down(pfrm, Key::right)) {
        if (cursor_loc.x < app.opponent_island()->terrain().size()) {
            ++cursor_loc.x;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }
    }

    if (app.player().key_down(pfrm, Key::up)) {
        if (cursor_loc.y > 6) {
            --cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }
    }

    if (app.player().key_down(pfrm, Key::down)) {
        if (cursor_loc.y < 14) {
            ++cursor_loc.y;
            clear_room_description(pfrm, room_description_);
            describe_room_timer_ = milliseconds(300);
        }
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (auto room = app.opponent_island()->get_room(cursor_loc)) {
            return room->select(pfrm, app);
        } else if (auto drone = app.opponent_island()->get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                return (*drone)->select(pfrm, app);
            }
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        if (auto drone = app.opponent_island()->get_drone(cursor_loc)) {
            if ((*drone)->parent() == &app.player_island()) {
                return scene_pool::alloc<SalvageDroneScene>(*drone);
            }
        } else if (app.game_mode() == App::GameMode::sandbox and
                   app.opponent_island()->get_room(cursor_loc)) {
            return scene_pool::alloc<SalvageRoomScene>(false);
        }
    }

    if (not is_far_camera()) {
        return scene_pool::alloc<ReadyScene>();
    }


    if (app.player().key_down(pfrm, Key::start)) {
        return scene_pool::alloc<KeyComboScene>(false);
    }

    if (app.game_mode() == App::GameMode::sandbox and
        app.player().key_down(pfrm, Key::alt_2)) {
        return scene_pool::alloc<ConstructionScene>(false);
    }

    if (not pfrm.network_peer().is_connected() and app.launch_repl()) {
        app.launch_repl() = false;
        return scene_pool::alloc<LispReplScene>(pfrm);
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(pfrm,
                          app,
                          &*app.opponent_island(),
                          cursor_loc,
                          room_description_);
        }
    }

    return null_scene();
}



void InspectP2Scene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (app.opponent_island()) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_texture_index(15 + cursor_anim_frame_);

        auto origin = app.opponent_island()->visual_origin();

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

        origin.x += cursor_loc.x * 16;
        origin.y += cursor_loc.y * 16;

        cursor.set_position(origin);

        pfrm.screen().draw(cursor);
    }
}


} // namespace skyland
