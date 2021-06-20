#include "enemyAI.hpp"
#include "number/random.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void EnemyAI::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player_island().is_destroyed()) {
        return;
    }

    next_action_timer_ -= delta;


    if (next_action_timer_ <= 0) {
        next_action_timer_ = next_action_timeout;

        u8 matrix[16][16];
        app.player_island().plot_rooms(matrix);

        if (app.opponent_island()) {
            for (auto& room : app.opponent_island()->rooms()) {
                if (auto cannon = dynamic_cast<Cannon*>(&*room)) {
                    set_target(pfrm, app, matrix, *cannon);
                } else if (auto silo = dynamic_cast<MissileSilo*>(&*room)) {
                    set_target(pfrm, app, matrix, *silo);
                }
            }
        }
    }
}



void EnemyAI::set_target(Platform& pfrm,
                         App& app,
                         const u8 matrix[16][16],
                         MissileSilo& silo)
{
    if (silo.parent()->get_drift()) {
        // Wait until we've stopped moving
        return;
    }

    Buffer<Room*, 32> visible_rooms;

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if (matrix[x][y]) {
                if (auto room = app.player_island().get_room({u8(x), u8(y)})) {
                    visible_rooms.push_back(room);
                }
                break;
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    Float highest_weight = 3E-5;

    auto cannon_metac = load_metaclass("cannon");

    bool cannons_remaining = false;

    for (auto& room : silo.parent()->rooms()) {
        if (room->metaclass() == cannon_metac) {
            cannons_remaining = true;
            break;
        }
    }

    for (auto room : visible_rooms) {
        auto meta_c = room->metaclass();
        auto w = (*meta_c)->ai_base_weight();

        // // If the player has a missile silo, and remaining missiles, we may want
        // // to think about destroying it.
        // if (str_cmp((*meta_c)->name(), "missile silo") == 0) {
        //     if (app.player().missile_ammo() == 0) {
        //         // OK, so there's no urgent need to attack this missile silo,
        //         // because the player doesn't even have any missiles.
        //         w = 50.f;
        //     } else {
        //         w += 500.f;
        //     }
        // }

        // Give the room some extra weight, if firing a missile into it would be
        // really destructive.
        if (w > 400 and room->health() <= Missile::deals_damage) {
            w += 300.f;
        }

        // We don't have any cannons left, but the other player does. Try to
        // take out some of those cannons with missiles.
        if (meta_c == cannon_metac and not cannons_remaining) {
            w += 200.f;
        }

        if (w > highest_weight) {
            highest_weighted_room = room;
            highest_weight = w;
        }
    }

    if (highest_weighted_room) {
        silo.set_target(highest_weighted_room->position());
    }
}



void EnemyAI::set_target(Platform& pfrm,
                         App& app,
                         const u8 matrix[16][16],
                         Cannon& cannon)
{
    // Ok, lets start by finding all of the line-of-sight targets:
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
    if (visible_rooms.size() > 1 and rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1];
    }

    if (highest_weighted_room) {
        cannon.set_target(highest_weighted_room->position());
    }
}



void EnemyAI::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    if (app.opponent_island()) {

        // bool weapons_remaining = false;
        // for (auto& room : app.opponent_island()->rooms()) {
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
        //         app.opponent_island()->plot_construction_zones(matrix);
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
        //             (*cannon_mt)->create(pfrm, &*app.opponent_island(), *target);
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



} // namespace skyland
