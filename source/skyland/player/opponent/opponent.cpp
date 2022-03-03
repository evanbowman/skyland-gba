#include "opponent.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void Opponent::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : app.birds()) {
        if (bird->island(app) == island) {
            bird->signal(pfrm, app);
        }
    }
}



} // namespace skyland
