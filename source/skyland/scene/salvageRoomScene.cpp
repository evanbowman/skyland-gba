////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "salvageRoomScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static Coins salvage_value(Room& room)
{
    return ((*room.metaclass())->cost() * salvage_factor) *
           (Float(room.health()) / (*room.metaclass())->full_health());
}



Island* SalvageRoomScene::island(App& app)
{
    if (near_) {
        return &app.player_island();
    } else if (app.opponent_island()) {
        return app.opponent_island();
    } else {
        return nullptr;
    }
}



void SalvageRoomScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    if (not island(app)) {
        return;
    }

    if (not near_) {
        far_camera();
    }

    auto st = calc_screen_tiles(pfrm);
    StringBuffer<30> text(SYSTR(salvage_prompt)->c_str());

    auto& cursor_loc =
        near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
              : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;

    if (auto room = island(app)->get_room(cursor_loc)) {
        if (auto mt = room->metaclass()) {
            if ((*mt)->category() == Room::Category::power and
                island(app)->core_count() == 1) {
                // That would be suicide! You can't salvage your island's
                // only power core.
                exit_countdown_ = 1;
            }
            text += stringify(salvage_value(*room));
        } else {
            text += "0";
        }
    }

    text += "@";

    text_.emplace(pfrm, text.c_str(), OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    for (int i = 23; i < st.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
    pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 3, 128);

    persist_ui();
}



void SalvageRoomScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    text_.reset();
    yes_text_.reset();
    no_text_.reset();

    const auto st = calc_screen_tiles(pfrm);
    for (int x = 0; x < st.x; ++x) {
        pfrm.set_tile(Layer::overlay, x, st.y - 1, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 2, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 3, 0);
        pfrm.set_tile(Layer::overlay, x, st.y - 4, 0);
    }
}



ScenePtr<Scene>
SalvageRoomScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }


    auto exit_scene = [near = near_]() -> ScenePtr<Scene> {
        if (near) {
            return scene_pool::alloc<ReadyScene>();
        } else {
            return scene_pool::alloc<InspectP2Scene>();
        }
    };


    auto& cursor_loc =
        near_ ? std::get<SkylandGlobalData>(globals()).near_cursor_loc_
              : std::get<SkylandGlobalData>(globals()).far_cursor_loc_;


    if (auto room = island(app)->get_room(cursor_loc)) {
        if (length(room->characters()) > 0) {
            auto future_scene = [exit_scene]() { return exit_scene(); };
            auto msg = SYSTR(salvage_error_populated);
            pfrm.speaker().play_sound("beep_error", 2);
            return scene_pool::alloc<NotificationScene>(msg->c_str(),
                                                        future_scene);
        }
    } else {
        return exit_scene();
    }



    if (island(app) == nullptr) {
        return exit_scene();
    }


    if (exit_countdown_) {
        exit_countdown_ -= delta;
        if (exit_countdown_ <= 0) {
            return exit_scene();
        }
    } else {

        if (app.player().key_down(pfrm, Key::action_1)) {
            if (auto room = island(app)->get_room(cursor_loc)) {

                // You cannot salvage an occupied room, doing so would destroy
                // all of the characters inside.
                if (length(room->characters()) == 0) {

                    pfrm.speaker().play_sound("coin", 2);
                    app.set_coins(pfrm, app.coins() + salvage_value(*room));

                    if (auto room = island(app)->get_room(cursor_loc)) {
                        auto mt_index =
                            metaclass_index((*room->metaclass())->name());
                        auto setup = [&](time_stream::event::RoomSalvaged& e) {
                            e.x_ = room->position().x;
                            e.y_ = room->position().y;
                            e.type_ = mt_index;
                        };
                        if (island(app) == &app.player_island()) {
                            time_stream::event::PlayerRoomSalvaged e;
                            setup(e);
                            app.time_stream().push(app.level_timer(), e);
                        } else {
                            time_stream::event::OpponentRoomSalvaged e;
                            setup(e);
                            app.time_stream().push(app.level_timer(), e);
                        }
                    }

                    island(app)->destroy_room(pfrm, app, cursor_loc);
                    exit_countdown_ = milliseconds(500);

                    network::packet::RoomSalvaged packet;
                    packet.x_ = cursor_loc.x;
                    packet.y_ = cursor_loc.y;
                    network::transmit(pfrm, packet);
                }
            } else {
                return exit_scene();
            }
        }
    }



    if (app.player().key_down(pfrm, Key::action_2)) {
        return exit_scene();
    }

    return null_scene();
}



} // namespace skyland
