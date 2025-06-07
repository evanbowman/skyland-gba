////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "salvageRoomScene.hpp"
#include "globals.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/minimap.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Coins get_room_cost(Island* island, const RoomMeta& meta);



static Coins salvage_value(Room& room)
{
    if (str_eq(room.name(), "gold")) {
        return (*room.metaclass())->cost();
    }

    Coins sv = ((*room.metaclass())->cost() *
                (not APP.opponent_island() ? 1.f : salvage_factor)) *
               (Float(room.health()) / (*room.metaclass())->full_health());

    return std::min(sv, get_room_cost(room.parent(), *room.metaclass()));
}



Island* SalvageRoomScene::island()
{
    if (near_) {
        return &APP.player_island();
    } else if (APP.opponent_island()) {
        return APP.opponent_island();
    } else {
        return nullptr;
    }
}



void SalvageRoomScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    if (not island()) {
        return;
    }

    if (not near_) {
        far_camera();
    }

    auto st = calc_screen_tiles();
    StringBuffer<30> text(SYSTR(salvage_prompt)->c_str());

    auto& cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    if (auto room = island()->get_room(cursor_loc)) {
        if (auto mt = room->metaclass()) {
            if ((*mt)->category() == Room::Category::power and
                island()->core_count() == 1) {
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

    text_.emplace(text.c_str(), OverlayCoord{0, u8(st.y - 1)});

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    for (int i = 23; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

    persist_ui();

    PLATFORM.set_tile(Layer::overlay, 0, st.y - 3, 160);
    PLATFORM.set_tile(Layer::overlay, 1, st.y - 3, 161);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - 2, 162);
    PLATFORM.set_tile(Layer::overlay, 1, st.y - 2, 163);

    PLATFORM.set_tile(Layer::overlay, 2, st.y - 2, 418);
    PLATFORM.set_tile(Layer::overlay, 2, st.y - 3, 433);
    PLATFORM.set_tile(Layer::overlay, 0, st.y - 4, 425);
    PLATFORM.set_tile(Layer::overlay, 1, st.y - 4, 425);
}



void SalvageRoomScene::exit(Scene& next)
{
    WorldScene::exit(next);

    text_.reset();
    yes_text_.reset();
    no_text_.reset();

    const auto st = calc_screen_tiles();
    for (int x = 0; x < st.x; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, st.y - 1, 0);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 2, 0);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 3, 0);
        PLATFORM.set_tile(Layer::overlay, x, st.y - 4, 0);
    }

    auto& cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;

    if (APP.game_mode() == App::GameMode::co_op) {
        if (auto room = island()->get_room(cursor_loc)) {
            room->co_op_release_lock();
        }
    }
}



ScenePtr SalvageRoomScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }


    auto exit_scene = [near = near_, this]() -> ScenePtr {
        if (next_) {
            return (*next_)();
        } else if (near) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<InspectP2Scene>();
        }
    };


    auto& cursor_loc =
        near_ ? globals().near_cursor_loc_ : globals().far_cursor_loc_;


    if (auto room = island()->get_room(cursor_loc)) {
        if (length(room->characters()) > 0) {
            auto future_scene = [exit_scene]() { return exit_scene(); };
            auto msg = SYSTR(salvage_error_populated);
            PLATFORM.speaker().play_sound("beep_error", 2);
            if (next_) {
                return (*next_)();
            } else {
                return make_scene<NotificationScene>(msg->c_str(),
                                                     future_scene);
            }
        }
    } else {
        return exit_scene();
    }



    if (island() == nullptr) {
        return exit_scene();
    }


    if (exit_countdown_) {
        exit_countdown_ -= delta;
        if (exit_countdown_ <= 0) {
            return exit_scene();
        }
    } else {

        if (APP.player().key_down(Key::action_1)) {
            if (auto room = island()->get_room(cursor_loc)) {

                // You cannot salvage an occupied room, doing so would destroy
                // all of the characters inside.
                if (length(room->characters()) == 0) {

                    PLATFORM.speaker().play_sound("coin", 2);
                    APP.set_coins(APP.coins() + salvage_value(*room));

                    minimap::player_destroyed_rooms.set(
                        cursor_loc.x, cursor_loc.y, false);

                    u16 mt_index = 0;
                    if (auto room = island()->get_room(cursor_loc)) {
                        mt_index =
                            metaclass_index((*room->metaclass())->name());

                        const auto room_x = room->position().x;
                        const auto room_y = room->position().y;

                        auto setup = [&](time_stream::event::RoomSalvaged& e) {
                            e.x_ = room_x;
                            e.y_ = room_y;
                            e.type_ = mt_index;
                            e.group_ = (u8)room->group();
                            e.health_.set(room->health());
                        };

                        room->on_salvage();
                        island()->destroy_room(cursor_loc);

                        if (is_player_island(island())) {
                            if (room->group() not_eq Room::Group::none) {
                                time_stream::event::WeaponSetGroup e;
                                e.room_x_ = cursor_loc.x;
                                e.room_y_ = cursor_loc.y;
                                e.prev_group_ = (u8)room->group();
                                APP.time_stream().push(APP.level_timer(), e);
                            }
                            time_stream::event::PlayerRoomSalvaged e;
                            setup(e);
                            APP.time_stream().push(APP.level_timer(), e);
                        } else {
                            time_stream::event::OpponentRoomSalvaged e;
                            setup(e);
                            APP.time_stream().push(APP.level_timer(), e);
                        }
                    }

                    exit_countdown_ = milliseconds(500);

                    network::packet::RoomSalvaged packet;
                    packet.x_ = cursor_loc.x;
                    packet.y_ = cursor_loc.y;
                    packet.metaclass_index_.set(mt_index);

                    network::transmit(packet);
                }
            } else {
                return exit_scene();
            }
        }
    }



    if (APP.player().key_down(Key::action_2)) {
        return exit_scene();
    }

    return null_scene();
}



} // namespace skyland
