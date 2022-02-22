#include "bulkhead.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



void Bulkhead::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_bulkhead_door)->c_str();
}



Bulkhead::Bulkhead(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Bulkhead::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Bulkhead::render_interior(App& app, u8 buffer[16][16])
{
    if (open_) {
        buffer[position().x][position().y] = InteriorTile::bulkhead_open_1;
        buffer[position().x][position().y + 1] = InteriorTile::plain_floor;
    } else {
        buffer[position().x][position().y] = InteriorTile::bulkhead_closed_1;
        buffer[position().x][position().y + 1] =
            InteriorTile::bulkhead_closed_2;
    }

    interior_visible_ = true;
}



void Bulkhead::___rewind___finished_reload(Platform& pfrm, App& app)
{
    set_open(pfrm, app, not open_);
}



void Bulkhead::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_plain_1;
    buffer[position().x][position().y + 1] = Tile::wall_plain_2;

    interior_visible_ = false;
}



void Bulkhead::set_open(Platform& pfrm, App& app, bool open)
{
    if (open_ not_eq open) {
        if (parent() == &app.player_island()) {
            time_stream::event::PlayerRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            app.time_stream().push(pfrm, app.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomReloadComplete e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            app.time_stream().push(pfrm, app.level_timer(), e);
        }
    }

    open_ = open;

    if (parent()->interior_visible()) {
        parent()->repaint(pfrm, app);
    }
    parent()->on_layout_changed(app, {position().x, u8(position().y + 1)});
}


ScenePtr<Scene>
Bulkhead::select(Platform& pfrm, App& app, const Vec2<u8>& cursor)
{
    if (length(characters())) {
        return Room::select(pfrm, app, cursor);
    }

    set_open(pfrm, app, not open_);

    if (&parent()->owner() == &app.player()) {
        network::packet::OpponentBulkheadChanged packet;
        packet.room_x_ = position().x;
        packet.room_y_ = position().y;
        packet.open_ = open_;
        network::transmit(pfrm, packet);
    }

    return null_scene();
}



} // namespace skyland
