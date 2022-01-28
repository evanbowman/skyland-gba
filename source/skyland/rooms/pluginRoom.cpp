#include "pluginRoom.hpp"
#include "skyland/room_metatable.hpp"



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



void PluginRoom::update(Platform&, App&, Microseconds delta)
{
    // ...
}



} // namespace skyland
