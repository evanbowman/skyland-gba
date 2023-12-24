////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



ScenePtr<Scene> PluginRoom::select_impl(const RoomCoord& cursor)
{
    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return null_scene();
    }

    if (parent()->power_supply() < parent()->power_drain()) {
        auto future_scene = []() { return scene_pool::alloc<ReadyScene>(); };
        return scene_pool::alloc<NotificationScene>("power outage!",
                                                    future_scene);
    }

    if (is_player_island(parent())) {
        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), true, target_);
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
