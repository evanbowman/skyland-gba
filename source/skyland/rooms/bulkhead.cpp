#include "bulkhead.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void Bulkhead::format_description(StringBuffer<512>& buffer)
{
    buffer += "An openable/closable door. "
              "Used for restricting the movement of enemies who may board your "
              "island.";
}



Bulkhead::Bulkhead(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
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



void Bulkhead::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_plain_1;
    buffer[position().x][position().y + 1] = Tile::wall_plain_2;

    interior_visible_ = false;
}



void Bulkhead::set_open(Platform& pfrm, App& app, bool open)
{
    open_ = open;

    if (parent()->interior_visible()) {
        parent()->repaint(pfrm, app);
    }
    parent()->on_layout_changed({position().x, u8(position().y + 1)});
}


ScenePtr<Scene> Bulkhead::select(Platform& pfrm, App& app)
{
    if (length(characters())) {
        return Room::select(pfrm, app);
    }

    open_ = not open_;

    if (&parent()->owner() == &app.player()) {
        network::packet::OpponentBulkheadChanged packet;
        packet.room_x_ = position().x;
        packet.room_y_ = position().y;
        packet.open_ = open_;
        network::transmit(pfrm, packet);

        set_open(pfrm, app, open_);
    }

    return null_scene();
}



} // namespace skyland
