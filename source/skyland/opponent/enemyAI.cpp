#include "enemyAI.hpp"
#include "skyland/skyland.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/cannon.hpp"



namespace skyland {



void EnemyAI::update(Platform& pfrm, App& app, Microseconds delta)
{
    next_action_timer_ -= delta;

    if (next_action_timer_ <= 0) {
        next_action_timer_ = next_action_timeout;


        if (app.encountered_island()) {
            for (auto& room : app.encountered_island()->rooms()) {
                if (auto cannon = dynamic_cast<Cannon*>(&*room)) {
                    set_cannon_target(pfrm, app, *cannon);
                }
            }
        }
    }
}



void EnemyAI::set_cannon_target(Platform&, App& app, Cannon& cannon)
{
    // TODO...

    // just some stupid code as a placeholder...

    Vec2<u8> target;
    for (auto& room : app.player_island().rooms()) {
        if (dynamic_cast<Core*>(&*room)) {
            target = room->position();
        }
    }

    cannon.set_target(target);
}



void EnemyAI::on_room_damaged(Platform& pfrm, Room& room)
{
    // TODO...
    // When we take damage, we may want to adjust our strategy in some way...
}



}
