#include "enemyAI.hpp"
#include "skyland/skyland.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/room_metatable.hpp"
#include "number/random.hpp"



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



void EnemyAI::set_cannon_target(Platform& pfrm, App& app, Cannon& cannon)
{
    // Ok, lets start by finding all of the line-of-sight targets:
    u8 matrix[16][16];
    app.player_island().plot_rooms(matrix);


    // (FIXME: actually draw the line-of-sight correctly...)
    Buffer<Room*, 32> visible_rooms;

    for (u8 y = 0; y < 16; ++y) {
        for (int x = 15; x > 0; --x) {
            if (matrix[x][y]) {
                if (auto room = app.player_island().get_room({u8(x), y})) {
                    visible_rooms.push_back(room);
                }
                break;
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    Float highest_weight = 3E-5;

    for (auto room : visible_rooms) {
        auto meta_c = room->metaclass();
        auto w = (*meta_c)->ai_base_weight();

        if (w > highest_weight) {
            highest_weighted_room = room;
            highest_weight = w;
        }
    }

    // Potentially attack the second highest weighted visible room, just to keep
    // things interesting.
    if (visible_rooms.size() > 1 and
        rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1];
    }

    if (highest_weighted_room) {
        highest_weighted_room->set_injured(pfrm);
        cannon.set_target(highest_weighted_room->position());
    }
}



void EnemyAI::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    if (app.encountered_island()) {

        // bool weapons_remaining = false;
        // for (auto& room : app.encountered_island()->rooms()) {
        //     if (dynamic_cast<Cannon*>(&*room)) {
        //         weapons_remaining = true;
        //         break;
        //     }
        // }

        // if (not weapons_remaining) {
        //     // Ok, all of our weapons have been destroyed. I.E. we cannot
        //     // possibly win in this state. Now let's see what we can do...

        //     // Maybe we can build a cannons?
        //     auto cannon_mt = load_metaclass("cannon");
        //     if ((*cannon_mt)->cost() < coins_) {
        //         // Ok, so we actually have enough coins lying around to buy a
        //         // cannon!
        //         // Do we have a spot to place it?
        //         bool matrix[16][16];
        //         app.encountered_island()->plot_construction_zones(matrix);
        //         std::optional<Vec2<u8>> target;

        //         [&] {
        //             for (u8 y = 0; y < 16; ++y) {
        //                 for (u8 x = 0; x < 16; ++x) {
        //                     if (matrix[x][y]) {
        //                         target = {x, y};
        //                         return;
        //                     }
        //                 }
        //             }
        //         }();

        //         if (target) {
        //             (*cannon_mt)->create(pfrm, &*app.encountered_island(), *target);
        //             coins_ -= (*cannon_mt)->cost();
        //         }

        //     }

        // }

        // Things that we might want to do here:
        // 1) Initiate repairs
        // 2) Salvage the room (if it's about to be destroyed anyway)
        // 3) Build weapons or fortifications
    }
}



}
