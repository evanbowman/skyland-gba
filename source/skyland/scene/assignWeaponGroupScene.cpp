#include "assignWeaponGroupScene.hpp"
#include "skyland/skyland.hpp"
#include "readyScene.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



void describe_room(Platform& pfrm,
                   App& app,
                   Island* island,
                   const Vec2<u8>& cursor_loc,
                   std::optional<Text>& room_description);



void clear_room_description(Platform& pfrm,
                            std::optional<Text>& room_description);



ScenePtr<Scene>
AssignWeaponGroupScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    app.player().key_held_distribute(pfrm);

    switch (state_) {
    case State::select_group:
        if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }
        break;

    case State::assign_rooms:
        // if (app.player().key_down(pfrm, Key::action_2)) {
        //     state_ = State::select_group;
        //     break;
        // }

        if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<ReadyScene>();
        }


        if (app.player().key_down(pfrm, Key::action_1)) {
            if (auto room = app.player_island().get_room(cursor_loc)) {
                if ((*room->metaclass())->category() == Room::Category::weapon) {
                    // TODO: select category first in select_group scene, then
                    // assign groups in bulk, rather than cycling through.
                    // if (room->group() == current_group_) {
                    //     room->set_group(Room::Group::none);
                    // } else {
                    //     room->set_group(current_group_);
                    // }
                    auto group = room->group();
                    if ((int)group < (int)Room::Group::three) {
                        group = (Room::Group)((int)group + 1);
                    } else {
                        group = Room::Group::none;
                    }
                    room->set_group(group);
                    app.player_island().repaint(pfrm, app);
                }
            }
        }

        if (test_key(Key::right)) {
            if (cursor_loc.x < app.opponent_island()->terrain().size()) {
                ++cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (test_key(Key::up)) {
            if (cursor_loc.y > 6) {
                --cursor_loc.y;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
                clear_room_description(pfrm, room_description_);
                describe_room_timer_ = milliseconds(300);
            }
        }
        break;
    }

    if (describe_room_timer_ > 0) {
        describe_room_timer_ -= delta;
        if (describe_room_timer_ <= 0) {
            describe_room_timer_ = milliseconds(500);

            describe_room(pfrm,
                          app,
                          app.opponent_island(),
                          cursor_loc,
                          room_description_);
        }
    }

    return null_scene();
}



void AssignWeaponGroupScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    msg_.emplace(pfrm, "assign weapon groups:", OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});


    app.player_island().show_groups(true);
    app.player_island().repaint(pfrm, app);
}



void AssignWeaponGroupScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    if (not app.player_island().interior_visible()) {
        app.player_island().show_groups(false);
    }
    app.player_island().repaint(pfrm, app);
}



void AssignWeaponGroupScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    auto origin = app.player_island().visual_origin();

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

    origin.x += cursor_loc.x * 16 + 3;
    origin.y += cursor_loc.y * 16;

    Sprite sprite;
    sprite.set_position(origin);
    sprite.set_priority(0);
    sprite.set_texture_index(62);
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_flip({true, false});

    pfrm.screen().draw(sprite);
}



}
