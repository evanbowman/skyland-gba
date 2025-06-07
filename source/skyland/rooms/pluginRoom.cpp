////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "pluginRoom.hpp"
#include "script/lisp.hpp"
#include "skyland/roomPluginInfo.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



PluginRoom::PluginRoom(Island* parent,
                       const RoomCoord& position,
                       RoomMeta* metaclass)
    : Room(parent, (*metaclass)->name(), position)
{
    // FIXME: I had to banish dynamic_cast from the codebase.
    // if (not dynamic_cast<RoomPluginInfo*>(this->metaclass()->box())) {
    //     // By checking things upon creation, we can skip the slow cast
    //     // elsewhere, as Room::metaclass_ cannont change (private var).
    //     Platform::fatal("Plugin room assigned a non-plugin metaclass");
    // }
}



void PluginRoom::render_interior(App* app, TileId buffer[16][16])
{
    auto b = static_cast<RoomPluginInfo*>(this->metaclass()->box());

    auto& l =
        b->fetch_info<RoomPluginInfo::FieldTag::graphics_list, lisp::Cons>();

    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            auto v = lisp::get_list((lisp::Value*)&l, x + y * size().x);
            if (v->type() == lisp::Value::Type::integer) {
                buffer[position().x + x][position().y + y] =
                    v->integer().value_;
            }
        }
    }
}



void PluginRoom::render_exterior(App* app, TileId buffer[16][16])
{
    auto b = static_cast<RoomPluginInfo*>(this->metaclass()->box());

    auto& l =
        b->fetch_info<RoomPluginInfo::FieldTag::graphics_list, lisp::Cons>();

    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            auto v = lisp::get_list((lisp::Value*)&l, x + y * size().x);
            if (v->type() == lisp::Value::Type::integer) {
                buffer[position().x + x][position().y + y] =
                    v->integer().value_;
            }
        }
    }
}



void PluginRoom::update(Time delta)
{
    Room::update(delta);

    Room::ready();

    if (parent()->power_supply() < parent()->power_drain()) {
        return;
    }

    auto b = static_cast<RoomPluginInfo*>(this->metaclass()->box());

    auto& v = b->fetch_info<RoomPluginInfo::FieldTag::update_frequency,
                            lisp::Integer>();

    timer_ += delta;
    if (timer_ >= seconds(v.value_)) {
        timer_ -= seconds(v.value_);

        auto& fn =
            b->fetch_info<RoomPluginInfo::FieldTag::update, lisp::Function>();

        lisp::push_op(
            lisp::make_userdata(parent(), parent()->script_userdata_tag()));
        lisp::push_op(lisp::make_integer(position().x));
        lisp::push_op(lisp::make_integer(position().y));

        if (target_) {
            lisp::Protected x(lisp::make_integer(target_->x));
            lisp::Protected y(lisp::make_integer(target_->y));
            lisp::push_op(lisp::make_cons(x, y));
        } else {
            lisp::push_op(L_NIL);
        }

        lisp::funcall((lisp::Value*)&fn, 4);

        auto result = lisp::get_op(0);

        if (result->type() == lisp::Value::Type::error) {
            lisp::DefaultPrinter p;
            lisp::format(result, p);
            PLATFORM.fatal(p.data_.c_str());
        }

        lisp::pop_op(); // funcall result
    }
}



void PluginRoom::rewind(Time delta)
{
    Room::rewind(delta);

    auto b = static_cast<RoomPluginInfo*>(this->metaclass()->box());

    auto& v = b->fetch_info<RoomPluginInfo::FieldTag::update_frequency,
                            lisp::Integer>();

    if (timer_ > 0) {
        timer_ -= delta;
    } else {
        timer_ += seconds(v.value_);
    }
}



ScenePtr PluginRoom::select_impl(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return make_scene<ReadyScene>(); };
        return make_scene<NotificationScene>("power outage!", future_scene);
    }

    if (is_player_island(parent())) {
        return make_scene<WeaponSetTargetScene>(position(), true, target_);
    }
    return null_scene();
}



void PluginRoom::set_target(const RoomCoord& target, bool pinned)
{
    if (target_ and *target_ == target) {
        // No need to waste space in rewind memory if the target does not
        // change.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = is_player_island(parent());

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    APP.time_stream().push(APP.level_timer(), e);

    target_ = target;
}



void PluginRoom::unset_target()
{
    if (not target_) {
        // Already uninitialized.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = is_player_island(parent());

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    APP.time_stream().push(APP.level_timer(), e);

    target_.reset();
}



} // namespace skyland
