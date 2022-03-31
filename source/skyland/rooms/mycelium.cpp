#include "mycelium.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Mycelium::Mycelium(Island* parent, const Vec2<u8>& position, const char* n)
    : Room(parent, n, position)
{
}



void Mycelium::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
    Room::ready();

    flood_timer_ += delta;

    if (flood_timer_ > seconds(6)) {
        flood_timer_ -= seconds(6);

        const auto x = position().x;
        const auto y = position().y;


        auto substrate = [&](u8 x, u8 y) {
            if (auto room = parent()->get_room({x, y})) {
                // Mycelium substrate must be non-mycelium room.
                return room->metaclass() not_eq metaclass() and
                       not(room->properties() & RoomProperties::fluid);
            }

            return false;
        };

        if (not substrate(x + 1, y) and not substrate(x - 1, y) and
            not substrate(x, y - 1) and not substrate(x, y + 1) and
            y not_eq 14) {
            this->apply_damage(pfrm, app, 9999);
            return;
        }

        auto slot_valid = [&](u8 x, u8 y) {
            if (x > parent()->terrain().size() - 1 or y > 14) {
                return false;
            }
            if (not parent()->get_room({x, y})) {
                if (y == 14) {
                    return true;
                }


                return substrate(x + 1, y) or substrate(x - 1, y) or
                       substrate(x, y - 1) or substrate(x, y + 1);
            }
            return false;
        };

        auto spread = [&](u8 x, u8 y) {
            (*metaclass())->create(pfrm, app, parent(), {x, y});

            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomCreated p;
                p.x_ = x;
                p.y_ = y;
                app.time_stream().push(pfrm, app.level_timer(), p);
            } else {
                time_stream::event::OpponentRoomCreated p;
                p.x_ = x;
                p.y_ = y;
                app.time_stream().push(pfrm, app.level_timer(), p);
            }
        };

        auto try_spread = [&](u8 x, u8 y) {
            if (slot_valid(x, y)) {
                spread(x, y);
            }
        };

        try_spread(x + 1, y);
        try_spread(x - 1, y);
        try_spread(x, y + 1);
        try_spread(x, y - 1);
        try_spread(x + 1, y + 1);
        try_spread(x - 1, y - 1);
        try_spread(x - 1, y + 1);
        try_spread(x + 1, y - 1);
    }
}



void Mycelium::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::mycelium;
}



void Mycelium::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::mycelium;
}



} // namespace skyland
