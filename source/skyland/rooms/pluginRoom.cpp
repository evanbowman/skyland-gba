#include "pluginRoom.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



PluginRoom::PluginRoom(Island* parent,
                       const Vec2<u8>& position,
                       RoomMeta* metaclass)
    : Room(parent, (*metaclass)->name(), position)
{
    if (not dynamic_cast<RoomMeta::PluginBox*>(this->metaclass()->box())) {
        // By checking things upon creation, we can skip the slow cast
        // elsewhere, as Room::metaclass_ cannont change (private var).
        Platform::fatal("Plugin room assigned a non-plugin metaclass");
    }
}



void PluginRoom::render_interior(App& app, u8 buffer[16][16])
{
    auto b = static_cast<RoomMeta::PluginBox*>(this->metaclass()->box());

    auto& l = b->fetch_info<RoomMeta::PluginBox::PluginInfo::graphics_list,
                            lisp::Cons>();

    if (l.car()->type() == lisp::Value::Type::integer) {
        buffer[position().x][position().y] = l.car()->integer().value_;
    }
}



void PluginRoom::render_exterior(App& app, u8 buffer[16][16])
{
    auto b = static_cast<RoomMeta::PluginBox*>(this->metaclass()->box());

    auto& l = b->fetch_info<RoomMeta::PluginBox::PluginInfo::graphics_list,
                            lisp::Cons>();

    if (l.car()->type() == lisp::Value::Type::integer) {
        buffer[position().x][position().y] = l.car()->integer().value_;
    }
}



void PluginRoom::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    ready();

    auto b = static_cast<RoomMeta::PluginBox*>(this->metaclass()->box());

    auto& v = b->fetch_info<RoomMeta::PluginBox::PluginInfo::update_frequency,
                            lisp::Integer>();

    timer_ += delta;
    if (timer_ >= seconds(v.value_)) {
        timer_ -= seconds(v.value_);

        auto& fn = b->fetch_info<RoomMeta::PluginBox::PluginInfo::update,
                                 lisp::Function>();

        lisp::push_op(lisp::make_integer(position().y));
        lisp::push_op(lisp::make_integer(position().x));
        lisp::push_op(lisp::make_userdata(parent()));

        lisp::funcall((lisp::Value*)&fn, 3);
        lisp::pop_op(); // funcall result
    }
}



void PluginRoom::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

    auto b = static_cast<RoomMeta::PluginBox*>(this->metaclass()->box());

    auto& v = b->fetch_info<RoomMeta::PluginBox::PluginInfo::update_frequency,
                            lisp::Integer>();

    if (timer_ > 0) {
        timer_ -= delta;
    } else {
        timer_ += seconds(v.value_);
    }
}



} // namespace skyland
