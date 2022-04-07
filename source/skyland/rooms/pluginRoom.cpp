////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "pluginRoom.hpp"
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
                       const Vec2<u8>& position,
                       RoomMeta* metaclass)
    : Room(parent, (*metaclass)->name(), position)
{
    if (not dynamic_cast<RoomPluginInfo*>(this->metaclass()->box())) {
        // By checking things upon creation, we can skip the slow cast
        // elsewhere, as Room::metaclass_ cannont change (private var).
        Platform::fatal("Plugin room assigned a non-plugin metaclass");
    }
}



void PluginRoom::render_interior(App& app, u8 buffer[16][16])
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



void PluginRoom::render_exterior(App& app, u8 buffer[16][16])
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



void PluginRoom::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

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

        lisp::push_op(lisp::make_userdata(parent()));
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
            pfrm.fatal(p.fmt_.c_str());
        }

        lisp::pop_op(); // funcall result
    }
}



void PluginRoom::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

    auto b = static_cast<RoomPluginInfo*>(this->metaclass()->box());

    auto& v = b->fetch_info<RoomPluginInfo::FieldTag::update_frequency,
                            lisp::Integer>();

    if (timer_ > 0) {
        timer_ -= delta;
    } else {
        timer_ += seconds(v.value_);
    }
}



ScenePtr<Scene>
PluginRoom::select(Platform& pfrm, App& app, const Vec2<u8>& cursor)
{
    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        return scene_pool::alloc<NotificationScene>("power outage!",
                                                    future_scene);
    }

    if (parent() == &app.player_island()) {
        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), true, target_);
    }
    return null_scene();
}



void PluginRoom::set_target(Platform& pfrm, App& app, const Vec2<u8>& target)
{
    if (target_ and *target_ == target) {
        // No need to waste space in rewind memory if the target does not
        // change.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = parent() == &app.player_island();

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    app.time_stream().push(app.level_timer(), e);

    target_ = target;
}



void PluginRoom::unset_target(Platform& pfrm, App& app)
{
    if (not target_) {
        // Already uninitialized.
        return;
    }

    time_stream::event::WeaponSetTarget e;
    e.room_x_ = position().x;
    e.room_y_ = position().y;

    e.near_ = parent() == &app.player_island();

    if (target_) {
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
        e.has_previous_target_ = true;
    } else {
        e.previous_target_x_ = 0;
        e.previous_target_y_ = 0;
        e.has_previous_target_ = false;
    }

    app.time_stream().push(app.level_timer(), e);

    target_.reset();
}



} // namespace skyland
