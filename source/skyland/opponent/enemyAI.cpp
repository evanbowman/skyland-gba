#include "enemyAI.hpp"
#include "number/random.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/ionCannon.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/skyland.hpp"



// I wrote this AI code frantically for a game jam. I know it's kind of a mess.



namespace skyland {



void EnemyAI::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player_island().is_destroyed()) {
        return;
    }

    total_time_ += delta;

    if (app.opponent_island()) {
        if (app.opponent_island()->power_supply() <
            app.opponent_island()->power_drain()) {
            resolve_insufficient_power(pfrm, app);
        }


        if (not app.opponent_island()->is_destroyed() and
            not app.player_island().is_destroyed()) {

            score_subtract_timer_ += delta;
            if (score_subtract_timer_ > seconds(1)) {
                // For level score calculation. The player earns score after
                // destroying an AI's rooms, and loses score for each second
                // spent in the level.
                score_subtract_timer_ -= seconds(1);
                app.score().set(app.score().get() - 2);
            }
        }
    }


    next_action_timer_ -= delta;
    character_reassign_timer_ -= delta;

    if (next_action_timer_ <= 0) {
        next_action_timer_ = next_action_timeout;

        u8 matrix[16][16];
        app.player_island().plot_rooms(matrix);


        // NOTE: there should never be as many as 8 boarded ai characters at
        // once! I doubt that the game can realistically run AI code and
        // pathfinding for all of those entities. Of course, the game _can_
        // handle that many entities, but doing so would result in periodic long
        // pauses.
        Buffer<std::pair<BasicCharacter*, Room*>, 8> boarded_ai_characters;
        for (auto& room : app.player_island().rooms()) {
            for (auto& character : room->characters()) {
                if (character->owner() == this) {
                    boarded_ai_characters.push_back(
                        {character.get(), room.get()});
                }
            }
        }


        if (app.opponent_island()) {
            for (auto& room : app.opponent_island()->rooms()) {
                if (auto cannon = dynamic_cast<Cannon*>(&*room)) {
                    set_target(pfrm, app, matrix, *cannon);
                } else if (auto silo = dynamic_cast<MissileSilo*>(&*room)) {
                    set_target(pfrm, app, matrix, *silo);
                } else if (auto flak_gun = dynamic_cast<FlakGun*>(&*room)) {
                    set_target(pfrm, app, matrix, *flak_gun);
                } else if (auto ion_cannon = dynamic_cast<IonCannon*>(&*room)) {
                    set_target(pfrm, app, matrix, *ion_cannon);
                } else if (auto db = dynamic_cast<DroneBay*>(&*room)) {
                    // Don't spawn drones until the level's been running for a
                    // bit.
                    if (total_time_ > seconds(8)) {
                        if (app.game_speed() not_eq GameSpeed::stopped) {
                            update_room(pfrm, app, matrix, *db);
                        }
                    }
                } else if (auto transporter =
                               dynamic_cast<Transporter*>(&*room)) {
                    if (length(transporter->characters()) and
                        transporter->ready()) {
                        auto transport_chr = transporter->characters().begin();
                        if ((*transport_chr)->state() not_eq
                                BasicCharacter::State::repair_room and
                            (*transport_chr)->owner() == this) {


                            // Justification for particularly nasty looking
                            // if-statement (below):
                            //
                            // Ok, so we only want to transport a character if
                            // another one of the characters isn't busy walking
                            // through the room.
                            //
                            // This fixes a bug where a character would be
                            // standing idle in the transporter, another
                            // character would be walking through, on the way to
                            // a different room, and the walking character,
                            // rather than the idle character, would be
                            // transported, creating a whole bunch of glitches.
                            if (not((*transport_chr)->has_movement_path() and
                                    not(*transport_chr)
                                           ->get_movement_path()
                                           ->empty() and
                                    (not((*(*transport_chr)
                                               ->get_movement_path())[0] ==
                                         (*transport_chr)->grid_position())))) {

                                transporter->random_transport_occupant(pfrm,
                                                                       app);
                            }
                        }
                    } else if (transporter->ready()) {
                        // If we have an infirmary, potentially transport some
                        // of our injured AIs back to heal.

                        bool found_infirmary = false;

                        auto metac = load_metaclass("infirmary");
                        for (auto& room : app.opponent_island()->rooms()) {
                            if (room->metaclass() == metac) {
                                found_infirmary = true;
                            }
                        }

                        if (found_infirmary) {
                            auto recover_pos =
                                [&]() -> std::optional<Vec2<u8>> {
                                for (auto it = boarded_ai_characters.begin();
                                     it not_eq boarded_ai_characters.end();) {
                                    if ((*it).first->health() < 25) {
                                        it = boarded_ai_characters.erase(it);
                                        return (*it).first->grid_position();
                                    } else {
                                        ++it;
                                    }
                                }
                                return {};
                            }();

                            if (not recover_pos) {
                                for (auto it = boarded_ai_characters.begin();
                                     it not_eq boarded_ai_characters.end();) {
                                    if (str_cmp((*(*it).second->metaclass())
                                                    ->name(),
                                                "plundered-room") == 0 and
                                        (*it).first->idle_count() > 600) {
                                        // If we've plundered a room, and we've
                                        // been waiting a while, i.e. we have
                                        // nowhere to go, then we should recover
                                        // the character.
                                        it = boarded_ai_characters.erase(it);
                                        recover_pos =
                                            (*it).first->grid_position();
                                    } else {
                                        ++it;
                                    }
                                }
                            }

                            if (recover_pos) {
                                transporter->recover_character(
                                    pfrm, app, *recover_pos);
                            }
                        }
                    }
                }
            }

            const auto combat_drone_index = DroneMeta::index("combat-drone");
            const auto cannon_drone_index = DroneMeta::index("cannon-drone");
            const auto flak_drone_index = DroneMeta::index("flak-drone");

            for (auto& drone : app.player_island().drones()) {
                if (auto drone_sp = drone.promote()) {
                    if ((*drone_sp)->parent() == &*app.opponent_island()) {
                        if ((*drone_sp)->metaclass_index() ==
                                cannon_drone_index or
                            (*drone_sp)->metaclass_index() ==
                                flak_drone_index) {

                            offensive_drone_set_target(
                                pfrm, app, matrix, **drone_sp);
                        } else if ((*drone_sp)->metaclass_index() ==
                                   combat_drone_index) {
                            combat_drone_set_target(
                                pfrm, app, matrix, **drone_sp);
                        }
                    }
                }
            }

            for (auto& drone : app.opponent_island()->drones()) {
                if (auto drone_sp = drone.promote()) {
                    if ((*drone_sp)->parent() == &*app.opponent_island()) {
                        if ((*drone_sp)->metaclass_index() ==
                                cannon_drone_index or
                            (*drone_sp)->metaclass_index() ==
                                flak_drone_index) {

                            offensive_drone_set_target(
                                pfrm, app, matrix, **drone_sp);
                        } else if ((*drone_sp)->metaclass_index() ==
                                   combat_drone_index) {
                            combat_drone_set_target(
                                pfrm, app, matrix, **drone_sp);
                        }
                    }
                }
            }
        }
    }

    if (character_reassign_timer_ <= 0) {
        character_reassign_timer_ = character_reassign_timeout;

        for (auto& room : app.player_island().rooms()) {
            for (auto& character : room->characters()) {
                if (character->owner() == this) {
                    assign_boarded_character(pfrm, app, *character);
                }
            }
        }

        if (app.opponent_island()) {
            for (auto& room : app.opponent_island()->rooms()) {
                for (auto& character : room->characters()) {
                    if (character->owner() == this) {
                        assign_local_character(pfrm, app, *character);
                    }
                }
            }
        }
    }
}



void EnemyAI::resolve_insufficient_power(Platform& pfrm, App& app)
{
    // When we have more power drain than power supply, we need to salvage a
    // bunch of structures to rebalance our power usage.

    // TODO: Make the AI less stupid.

    Room* lowest_weighted_room = nullptr;
    Float lowest_weight = 2000000.f;

    for (auto& room : app.opponent_island()->rooms()) {
        auto pwr = (*room->metaclass())->consumes_power();
        if (pwr > 0) {
            auto w = (*room->metaclass())->ai_base_weight();
            if (w < lowest_weight) {
                lowest_weighted_room = room.get();
                lowest_weight = w;
            }
        }
    }

    if (lowest_weighted_room) {
        // TODO: give coins back to the AI, so that it can potentially construct
        // something else. For the time being, we just want to salvage the
        // lowest-weighted rooms, until we have a stable balance of power, to
        // bring our systems back online.
        lowest_weighted_room->apply_damage(pfrm, app, 9999);
    }
}



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void EnemyAI::assign_local_character(Platform& pfrm,
                                     App& app,
                                     BasicCharacter& character)
{
    // This code is so cluttered and sprawling. I had to write an AI and only
    // had two weeks for this project, so it's kind of a mess.

    // Basically, we first want to collect all of the reachable slots from the
    // character's position, via a breadth-first search. Then, we assign a
    // weight to each destination slot based on a series of heuristics. The
    // heursitics are the most convoluted and messy part. Then, we sort the
    // reachable slots by the assigned weights, and run an implementation of
    // Dijkstra's algorithm to find a path.

    if (character.has_movement_path()) {
        return;
    }

    Buffer<Vec2<u8>, 16> exclude_slots;

    // We may want to keep track of how many of the player's characters have
    // boarded our island. We might not want to transport to the player's island
    // if many of the player's characters have boarded.
    int player_characters_local = 0;
    int player_characters_remote = 0; // number of player characters on the
                                      // player's island.

    // The number of ai characters that have already boarded the player's
    // island.
    int ai_characters_remote = 0;
    int ai_characters_local = 0;

    // Should we board the player's castle? Well, if we have no weapons
    // remaining, a boarding party is the only way that we can deal damage,
    // so...
    int cannon_count = 0;
    int missile_count = 0;

    int player_cannon_count = 0;
    int player_missile_count = 0;

    for (auto& room : app.opponent_island()->rooms()) {
        if (room->metaclass() == cannon_mt) {
            ++cannon_count;
        }
        if (room->metaclass() == missile_silo_mt) {
            ++missile_count;
        }
        for (auto& other : room->characters()) {
            if (other->owner() == this and other.get() not_eq &character) {
                if (auto dest = other->destination()) {
                    exclude_slots.push_back(*dest);
                } else {
                    exclude_slots.push_back(other->grid_position());
                }
                ++ai_characters_local;
            } else if (other->owner() not_eq this) {
                ++player_characters_local;
            } else if (other.get() == &character) {
                ++ai_characters_local;
            }
        }
    }

    for (auto& room : app.player_island().rooms()) {
        if (room->metaclass() == cannon_mt) {
            ++player_cannon_count;
        }
        if (room->metaclass() == missile_silo_mt) {
            ++player_missile_count;
        }
        for (auto& chr : room->characters()) {
            if (chr->owner() == this) {
                ++ai_characters_remote;
            } else {
                ++player_characters_remote;
            }
        }
    }

    DynamicMemory<bool[16][16]> matrix_ = allocate_dynamic<bool[16][16]>(pfrm);

    app.opponent_island()->plot_walkable_zones(*matrix_);

    u8 matrix[16][16];
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y]) {
                matrix[x][y] = 1;
            } else {
                matrix[x][y] = 0;
            }
        }
    }

    auto current_pos = character.grid_position();

    flood_fill(pfrm, matrix, 2, current_pos.x, current_pos.y);

    struct Destination {
        Vec2<u8> coord_;
        Float ai_weight_;
    };

    Buffer<Destination, 48> slots;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y] == 2) {
                slots.push_back({{x, y}, 0.f});
            }
        }
    }

    if (slots.empty()) {
        return; // hmm...
    }


    const auto infirmary_metac = load_metaclass("infirmary");


    for (auto& slot : slots) {
        if (auto room = app.opponent_island()->get_room(slot.coord_)) {

            const auto base_weight = (*room->metaclass())->ai_base_weight();

            // Increase room weight if damaged.
            slot.ai_weight_ =
                base_weight +
                (base_weight -
                 base_weight * (Float(room->health()) / room->max_health()));

            if (room->health() not_eq room->max_health()) {
                slot.ai_weight_ += 500.f;
            }

            slot.ai_weight_ -= 3 * manhattan_length(slot.coord_, current_pos);

            if (room->metaclass() == infirmary_metac) {
                // If our health is really low, we probably want to go to the
                // infirmary. If our health is just kinda low, we maybe want to
                // go to the infirmary.
                if (character.health() < 25) {
                    slot.ai_weight_ += 2000.f;
                } else if (character.health() < 200 and
                           not player_characters_local) {
                    slot.ai_weight_ += 2000.f;
                }
            } else if (auto transporter = dynamic_cast<Transporter*>(room)) {
                // Now, let's see. We want to raid the player's island if we
                // have more characters, but also...
                //
                // TODO: Increase the transporter weight a lot if we don't have
                // any remaining offensive capabilities. In that case, raids
                // would be our only offense.
                if (transporter->ready()) {

                    if (player_characters_remote >= ai_characters_remote and
                        ai_characters_remote + ai_characters_local >
                            player_characters_remote) {
                        slot.ai_weight_ += 400.f * ai_characters_local;
                    }
                    if (player_characters_local > ai_characters_local) {
                        slot.ai_weight_ -= 250.f * (player_characters_local -
                                                    ai_characters_local);
                    }
                    const auto weapon_count = cannon_count + missile_count;
                    if (weapon_count == 0) {
                        // If we don't have any remaining weapons, potentially
                        // board the player's castle, even if doing so would be
                        // a suicide mission.
                        slot.ai_weight_ += 1000.f;
                    } else if (missile_count and cannon_count == 0 and
                               player_cannon_count) {
                        slot.ai_weight_ += 400.f;
                    } else if (cannon_count and missile_count == 0 and
                               player_missile_count) {
                        slot.ai_weight_ += 400.f;
                    }
                } else {
                    slot.ai_weight_ -= 300;
                }
            }
        }
        for (auto& exc : exclude_slots) {
            if (slot.coord_ == exc) {
                // Don't move into a slot targeted by another one of our ai
                // characters.
                slot.ai_weight_ = -2000.f;
            }
        }
    }

    std::sort(slots.begin(),
              slots.end(),
              [&](const Destination& lhs, const Destination& rhs) {
                  return lhs.ai_weight_ < rhs.ai_weight_;
              });

    if (slots.back().ai_weight_ == 0.f) {
        // Again, perhaps this is overly defensive coding. But we should never
        // end up in a situation where the weights of the rooms are all
        // uninitialized...
        return;
    }

    auto target = slots.back();

    if (auto path = find_path(
            pfrm, &*app.opponent_island(), current_pos, target.coord_)) {
        if (not((*path)->size() == 1 and
                (**path)[0] == character.grid_position())) {
            // Don't waste a path buffer on an entity if the ideal path
            // represents a single node with the character's current position.
            character.set_movement_path(std::move(*path));
        }
    }
}



void EnemyAI::assign_boarded_character(Platform& pfrm,
                                       App& app,
                                       BasicCharacter& character)
{
    // Ok, so here, our ai character has boarded the player's island. We want to
    // find an ideal position to move the character to, with consideration to
    // the layout of the player's island. We can only move into a connected
    // component, and we want to attack a room with a high weight.

    // TODO: we may want to decrease the weight of a room if one of the player's
    // own characters stands between us and the target room. An unprotected room
    // is of course better than a guarded one.

    if (character.has_movement_path()) {
        // we've already assigned a movement path to the character. We may want
        // to recalculate the path at some point. But the computations below are
        // a bit expensive, so, instead, why don't we just defer the computation
        // until a room in the player's island is actually destroyed?
        return;
    }


    Buffer<Vec2<u8>, 16> exclude_slots; // Don't move into currently occupied
                                        // slots, or slots that will be
                                        // occupied.

    for (auto& room : app.player_island().rooms()) {
        for (auto& other : room->characters()) {
            if (other->owner() == this and other.get() not_eq &character) {
                if (auto dest = other->destination()) {
                    exclude_slots.push_back(*dest);
                } else {
                    exclude_slots.push_back(other->grid_position());
                }
            }
        }
    }


    DynamicMemory<bool[16][16]> matrix_ = allocate_dynamic<bool[16][16]>(pfrm);

    app.player_island().plot_walkable_zones(*matrix_);

    u8 matrix[16][16];
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if ((*matrix_)[x][y]) {
                matrix[x][y] = 1;
            } else {
                matrix[x][y] = 0;
            }
        }
    }

    auto current_pos = character.grid_position();

    flood_fill(pfrm, matrix, 2, current_pos.x, current_pos.y);

    struct Destination {
        Vec2<u8> coord_;
        Float ai_weight_;
    };

    Buffer<Destination, 48> slots;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y] == 2) {
                slots.push_back({{x, y}, 0.f});
            }
        }
    }

    if (slots.empty()) {
        // TODO: what to do in this case? How can we even end up in a situation
        // like this? If all of the player's rooms were destroyed, and one of
        // the player's rooms retain a unique pointer to the invading enemy
        // character, then it should be theoretically impossible for us to end
        // up in this state.
        return;
    }

    for (auto& slot : slots) {
        if (auto room = app.player_island().get_room(slot.coord_)) {
            slot.ai_weight_ = (*room->metaclass())->ai_base_weight();
            slot.ai_weight_ -= 3 * manhattan_length(slot.coord_, current_pos);

            Float player_chr_remove_weight = 0.f;
            for (auto& chr : room->characters()) {
                // If the player has a bunch of characters in the room, label it
                // as "toxic" and resist allocating entities to the room (unless
                // it's really valuable).
                if (chr->owner() not_eq this) {
                    player_chr_remove_weight += 100.f;

                    if (character.health() < 50) {
                        // More likely to flee the room if we're badly injured.
                        player_chr_remove_weight += 100.f;
                    }
                }
            }

            // NOTE: exclude_slots.size() + 1 gives us the total number of
            // boarded (invading) characters that the AI controls.  We want to
            // avoid entering a room if it is crammed with defending characters,
            // but also, we want to divide up the weight that we plan to
            // subtract off by the number of our own AI characters, so that we
            // will still potentially attack the room if we have enough
            // characters to (potentially) overwhelm the occupants.
            slot.ai_weight_ -=
                player_chr_remove_weight / (0.75f * (exclude_slots.size() + 1));
        }
        for (auto& exc : exclude_slots) {
            if (slot.coord_ == exc) {
                // Don't move into a slot targeted by another one of our ai
                // characters.
                slot.ai_weight_ = -2000.f;
            }
        }
    }

    std::sort(slots.begin(),
              slots.end(),
              [&](const Destination& lhs, const Destination& rhs) {
                  return lhs.ai_weight_ < rhs.ai_weight_;
              });

    if (slots.back().ai_weight_ == 0.f) {
        // Again, perhaps this is overly defensive coding. But we should never
        // end up in a situation where the weights of the rooms are all
        // uninitialized...
        return;
    }

    auto target = slots.back();

    if (auto path =
            find_path(pfrm, &app.player_island(), current_pos, target.coord_)) {
        if (not((*path)->size() == 1 and
                (**path)[0] == character.grid_position())) {
            // Don't waste a path buffer on an entity if the ideal path
            // represents a single node with the character's current position.
            character.set_movement_path(std::move(*path));
        }
    }
}



void EnemyAI::set_target(Platform& pfrm,
                         App& app,
                         const u8 matrix[16][16],
                         IonCannon& ion_cannon)
{
    Room* highest_weighted_room = nullptr;
    Float highest_weight = 3E-5;



    for (auto& room : app.player_island().rooms()) {
        auto meta_c = room->metaclass();

        auto w = (*meta_c)->ai_base_weight();

        if (meta_c == bulkhead_mt and app.player_island().is_boarded()) {
            if (auto bulkhead = dynamic_cast<Bulkhead*>(room.get())) {
                if (not bulkhead->is_open()) {
                    w = (*forcefield_mt)->ai_base_weight() +
                        2 * manhattan_length(room->origin(),
                                             ion_cannon.origin());
                }
            }
        } else if (meta_c not_eq forcefield_mt) {
            continue;
        } else {
            w += 3 * manhattan_length(room->origin(), ion_cannon.origin());
        }


        if (w > highest_weight) {
            highest_weighted_room = room.get();
            highest_weight = w;
        }
    }

    if (highest_weighted_room) {
        ion_cannon.set_target(highest_weighted_room->position());
    }
}



static void place_offensive_drone(Platform& pfrm,
                                  App& app,
                                  DroneBay& db,
                                  bool slot[16][16],
                                  const u8 player_rooms[16][16],
                                  bool restrict_columns[16],
                                  DroneMeta* metac,
                                  Island& player_island,
                                  Island& ai_island)
{
    Float left_column_weights[16];
    for (auto& val : left_column_weights) {
        val = 0.f;
    }

    for (auto& drone : player_island.drones()) {
        if (auto drone_sp = drone.promote()) {
            // Don't stack drones in the same column, or we could end up
            // shooting one of our other drones.
            if (drone_sp and (*drone_sp)->parent() == &ai_island) {
                restrict_columns[(*drone_sp)->position().x] = true;
            }
        }
    }

    Float top_row_weights[16];
    for (int i = 0; i < 16; ++i) {
        if (restrict_columns[i]) {
            top_row_weights[i] = -10000.f;
        } else {
            top_row_weights[i] = 0.f;
        }
    }

    // 9-14 range: check immediately rightwards.
    if (not restrict_columns[0]) {
        for (u8 y = 9; y < 14; ++y) {
            if (slot[0][y]) {
                for (u8 x = 1; x < 16; ++x) {
                    if (player_rooms[x][y]) {
                        if (auto room = player_island.get_room({x, y})) {
                            left_column_weights[y] =
                                (*room->metaclass())->ai_base_weight();
                            break;
                        }
                    }
                }
                if (left_column_weights[y] == 0.f) {
                    left_column_weights[y] = 0.5f;
                }
            }
        }
    }

    // range 6-10: check diagonally right/down
    if (not restrict_columns[0]) {
        for (u8 y = 7; y < 9; ++y) {
            if (slot[0][y]) {
                Vec2<u8> cursor{1, u8(y + 1)};
                while (cursor.x < 16 and cursor.y < 15) {
                    if (player_rooms[cursor.x][cursor.y]) {
                        if (auto room = player_island.get_room(cursor)) {
                            left_column_weights[y] =
                                (*room->metaclass())->ai_base_weight();
                            break;
                        }
                    }
                    ++cursor.x;
                    ++cursor.y;
                }
                if (left_column_weights[y] == 0.f) {
                    left_column_weights[y] = 0.5f;
                }
            }
        }
    }

    // top row: check vertically down
    // NOTE: += 2 because we don't want to place two drones directly adjacent,
    // as they might accidentally shoot eachother.
    for (u8 x = 0; x < 16; x += 2) {
        if (slot[x][6] and not restrict_columns[x]) {
            for (u8 y = 6; y < 15; ++y) {
                if (player_rooms[x][y]) {
                    if (auto room = player_island.get_room({x, y})) {
                        top_row_weights[x] =
                            (*room->metaclass())->ai_base_weight();
                    }
                    break;
                }
            }
            if (top_row_weights[x] == 0.f) {
                top_row_weights[x] = 0.5f;
            }
        }
    }

    std::optional<Vec2<u8>> ideal_coord;
    Float max_weight = 0.f;
    for (u8 y = 6; y < 15; ++y) {
        if (left_column_weights[y] > max_weight) {
            ideal_coord = {0, y};
            max_weight = left_column_weights[y];
        }
    }

    for (u8 x = 0; x < 16; ++x) {
        if (top_row_weights[x] > max_weight) {
            ideal_coord = {x, 6};
            max_weight = top_row_weights[x];
        }
    }

    auto create_pos = db.position();
    create_pos.y -= 1;

    if (ideal_coord) {
        auto drone = (*metac)->create(&ai_island, &player_island, create_pos);
        if (drone) {
            (*drone)->set_movement_target(*ideal_coord);
            db.attach_drone(pfrm, app, *drone);
            player_island.drones().push(*drone);
        }
    }
}



void get_drone_slots(bool slots[16][16], Island* dest_island, Island* parent);



void EnemyAI::update_room(Platform& pfrm,
                          App& app,
                          const u8 matrix[16][16],
                          DroneBay& db)
{
    if (db.reload_time_remaining() > 0) {
        return;
    }

    const auto combat_drone_index = DroneMeta::index("combat-drone");
    const auto cannon_drone_index = DroneMeta::index("cannon-drone");
    const auto flak_drone_index = DroneMeta::index("flak-drone");


    auto current_drone = db.drone();
    auto [dt, ds] = drone_metatable();

    static const int weight_array_size = 32;
    Float weights[weight_array_size];
    for (auto& w : weights) {
        w = 0.f;
    }

    if (weight_array_size < ds) {
        pfrm.fatal("drone weight array insufficiently sized");
    }

    // We want to ideally place flak-drones with a greater likelihood than
    // cannon drones, unless certain conditions arise (see below).
    weights[flak_drone_index] = 24.f;
    weights[cannon_drone_index] = 16.f;

    bool player_missile_silos[16];
    bool opponent_missile_silos[16];
    for (int i = 0; i < 16; ++i) {
        player_missile_silos[i] = 0;
        opponent_missile_silos[16] = 0;
    }

    (void)player_missile_silos;

    auto ai_controlled = [&](Drone& d) {
        return d.parent() == &*app.opponent_island();
    };

    for (auto& room : app.opponent_island()->rooms()) {
        if (room->metaclass() == missile_silo_mt) {
            opponent_missile_silos[room->position().x] = true;
        }
    }

    for (auto& room : app.player_island().rooms()) {
        // NOTE: this loop assumes that the initial weight for combat drones is
        // zeroe'd.
        if (room->metaclass() == drone_bay_mt) {
            // If the player has drone bays, he/she may deploy drones, in which
            // case, we might want to think about proactively deploying a combat
            // drone of our own.
            if (weights[combat_drone_index] == 0.f) {
                weights[combat_drone_index] = 14.f;
            } else {
                // For each DroneBay controlled by the player, it becomes
                // increasingly likely that the player will be deploying drones,
                // in which case, we'll want to be able to defend ourself.
                weights[combat_drone_index] *= 2.f;
            }
        } else if (room->metaclass() == missile_silo_mt) {
            // DroneBay rooms quite are vulnerable to missiles, so we want to
            // preferentially deploy cannon drones rather than flak drones if
            // the player has a lot of missile silos.
            weights[cannon_drone_index] += 8.f;

            // We want to keep track of which of the player's grid coordinates
            // contains a missile silo, as we wouldn't want to place a drone
            // directly in the path of a weapon.
            player_missile_silos[room->position().x] = true;
        }
    }

    auto update_weights = [&](Drone& d, bool local) {
        const auto metac = d.metaclass_index();
        if (ai_controlled(d)) {
            if (metac == combat_drone_index) {
                // If we have a combat drone already, we have less reason to
                // place another one. In fact, if the player has no combat
                // drones, we should think about potentially dropping our combat
                // drone.
                weights[combat_drone_index] -= 48.f;
            }
        } else {
            if (metac == combat_drone_index) {
                // If the player has a combat drone, any drone that we place
                // would be particularly vulnerable, so we need to take out the
                // player's drone first.
                weights[combat_drone_index] += 32.f;
            } else if (metac == cannon_drone_index or
                       metac == flak_drone_index) {
                // If the player has a couple of offensive drones, we may want
                // to react defensively.
                weights[combat_drone_index] += 8.f;
                if (local) {
                    weights[combat_drone_index] += 16.f;
                }
            }
        }
    };

    for (auto& drone_wp : app.player_island().drones()) {
        if (auto drone_sp = drone_wp.promote()) {
            update_weights(**drone_sp, false);
        }
    }

    for (auto& drone_wp : app.opponent_island()->drones()) {
        if (auto drone_sp = drone_wp.promote()) {
            update_weights(**drone_sp, true);
        }
    }

    int highest_weight_index = 0;
    for (int i = 0; i < weight_array_size; ++i) {
        if (weights[i] > weights[highest_weight_index]) {
            highest_weight_index = i;
        }
    }

    // Ok, at this point, we've determined which type of drone that we want to
    // deploy. But now, which grid location to place the drone into...


    if (current_drone) {
        if ((*current_drone)->metaclass_index() == highest_weight_index) {
            // The currently-deployed drone is the same as the highest-weighted
            // one, so no reason to make any changes.
            return;
        }
    }

    bool slots[16][16];
    for (int y = 0; y < 16; ++y) {
        for (int x = 0; x < 16; ++x) {
            slots[x][y] = y > 5;
        }
    }


    db.start_reload();


    auto create_pos = db.position();
    create_pos.y -= 1;

    // auto place_offensive_drone = [&](DroneMeta* metac) {
    //     get_drone_slots(slots, &app.player_island(), &*app.opponent_island());
    // };


    if (highest_weight_index == cannon_drone_index) {

        get_drone_slots(slots, &app.player_island(), &*app.opponent_island());

        place_offensive_drone(pfrm,
                              app,
                              db,
                              slots,
                              matrix,
                              player_missile_silos,
                              &dt[cannon_drone_index],
                              app.player_island(),
                              *app.opponent_island());

    } else if (highest_weight_index == combat_drone_index) {

        get_drone_slots(
            slots, &*app.opponent_island(), &*app.opponent_island());

        for (u8 y = 0; y < 16; y += 2) {
            for (u8 x = 0; x < 16; x += 2) {
                if (slots[x][y] and not opponent_missile_silos[x]) {
                    auto drone =
                        dt[combat_drone_index]->create(&*app.opponent_island(),
                                                       &*app.opponent_island(),
                                                       create_pos);
                    if (drone) {
                        (*drone)->set_movement_target({x, y});
                        db.attach_drone(pfrm, app, *drone);
                        app.opponent_island()->drones().push(*drone);
                        return;
                    }
                }
            }
        }

    } else if (highest_weight_index == flak_drone_index) {

        get_drone_slots(slots, &app.player_island(), &*app.opponent_island());

        place_offensive_drone(pfrm,
                              app,
                              db,
                              slots,
                              matrix,
                              player_missile_silos,
                              &dt[flak_drone_index],
                              app.player_island(),
                              *app.opponent_island());
    }
}



void EnemyAI::combat_drone_set_target(Platform& pfrm,
                                      App& app,
                                      u8 matrix[16][16],
                                      Drone& drone)
{
    for (auto& drone_wp : app.player_island().drones()) {
        if (auto drone_sp = drone_wp.promote()) {
            if ((*drone_sp)->parent() == &app.player_island()) {
                drone.set_target((*drone_sp)->position(), true);
            }
        }
    }

    for (auto& drone_wp : app.opponent_island()->drones()) {
        if (auto drone_sp = drone_wp.promote()) {
            if ((*drone_sp)->parent() == &app.player_island()) {
                drone.set_target((*drone_sp)->position(), false);
            }
        }
    }
}



void EnemyAI::offensive_drone_set_target(Platform& pfrm,
                                         App& app,
                                         const u8 matrix[16][16],
                                         Drone& drone)
{
    // Calculate a few radial line-of-sight paths based on the drone's position,
    // and select a target based on what we believe to be visible.

    // We cannot do very intensive raycasting to determine an ideal target
    // without potentially affecting performance on the gameboy, if the enemy
    // has deployed a lot of drones.

    std::optional<Vec2<u8>> ideal_pos;
    Float highest_weight = 0.f;

    const auto drone_pos = drone.position();

    Vec2<u8> cursor = drone_pos;

    const auto width = app.player_island().terrain().size();

    auto enqueue = [&] {
        if (auto room = app.player_island().get_room(cursor)) {
            auto weight = (*room->metaclass())->ai_base_weight();
            if (weight > highest_weight) {
                ideal_pos = cursor;
            }
        }
    };

    // seek left
    while (cursor.x > 0) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        --cursor.x;
    }

    cursor = drone_pos;

    // seek right
    while (cursor.x < width) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        ++cursor.x;
    }

    cursor = drone_pos;

    // seek down
    while (cursor.y < 15) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        ++cursor.y;
    }

    cursor = drone_pos;

    // seek diagonally, with slope -1
    while (cursor.x < width and cursor.y < 15) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        ++cursor.x;
        ++cursor.y;
    }

    cursor = drone_pos;

    // seek diagonally, with slope = 1
    while (cursor.x > 0 and cursor.y < 15) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        --cursor.x;
        ++cursor.y;
    }

    cursor = drone_pos;

    // seek diagonally, with slope = -2
    while (cursor.x < width and cursor.y < 15) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        ++cursor.x;
        cursor.y += 2;
    }

    cursor = drone_pos;

    // seek diagonally, with slope = 2
    while (cursor.x > 0 and cursor.y < 15) {
        if (matrix[cursor.x][cursor.y]) {
            enqueue();
            break;
        }
        --cursor.x;
        cursor.y += 2;
    }

    // Ok, we might want to think about casting a few trajectories diagonally
    // upwards, but for now, let's stick with what we've done so far.

    if (ideal_pos) {
        drone.set_target(*ideal_pos);
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

        for (auto& chr : room->characters()) {
            const bool ai_owns_character = chr->owner() == this;
            if (ai_owns_character) {
                // Sometimes we want to blow up a room that contains some of our
                // own invading characters, but often, we don't :)
                //
                // We can make this calculation a bit more complex,
                // though. E.G. if we're attacking a power core, and we don't
                // realistically have enough weapons to destroy the player's
                // offensive capabilities, then the only path to victory may be
                // through destroying rooms containing our own characters.
                w -= 200.f;
            }
        }

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
        if (not app.tutorial_mode() and visible_rooms.size() > 1 and
            rng::choice<4>(rng::utility_state) == 0) {
            silo.set_target(visible_rooms[rng::choice(visible_rooms.size(),
                                                      rng::utility_state)]
                                ->position());
        } else {
            silo.set_target(highest_weighted_room->position());
        }
    }
}



void EnemyAI::set_target(Platform& pfrm,
                         App& app,
                         const u8 matrix[16][16],
                         FlakGun& flak_gun)
{
    struct RoomInfo {
        Room* room_;
        int x_;
        int y_;
    };
    Buffer<RoomInfo, 32> visible_rooms;

    for (u8 y = 0; y < 16; ++y) {
        for (int x = 15; x > -1; --x) {
            if (matrix[x][y]) {
                if (auto room = app.player_island().get_room({u8(x), y})) {
                    visible_rooms.push_back({room, x, y});
                }
                break;
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    Float highest_weight = 3E-5;

    for (auto room_info : visible_rooms) {
        auto meta_c = room_info.room_->metaclass();
        auto w = (*meta_c)->ai_base_weight();

        auto neighbor_weight = [&](int x_offset, int y_offset) {
            const auto x = room_info.x_ + x_offset;
            const auto y = room_info.y_ + y_offset;

            if (x >= 0 and x < 15 and y >= 0 and y < 15) {
                if (auto room = app.player_island().get_room({u8(x), u8(y)})) {
                    return (*room->metaclass())->ai_base_weight();
                }
            }

            return 0.f;
        };

        // Yeah, this is pretty bad. Project due in a few hours, though!
        w += 0.5f * neighbor_weight(1, 0);
        w += 0.5f * neighbor_weight(-1, 0);
        w += 0.5f * neighbor_weight(0, 1);
        w += 0.5f * neighbor_weight(0, -1);

        w += 0.3f * neighbor_weight(2, 0);
        w += 0.3f * neighbor_weight(1, -1);
        w += 0.3f * neighbor_weight(0, -2);
        w += 0.3f * neighbor_weight(-1, -1);
        w += 0.3f * neighbor_weight(-2, 0);
        w += 0.3f * neighbor_weight(-1, 1);
        w += 0.3f * neighbor_weight(0, 2);
        w += 0.3f * neighbor_weight(1, 1);


        if (w > highest_weight) {
            highest_weighted_room = room_info.room_;
            highest_weight = w;
        }
    }

    if (not app.tutorial_mode() and visible_rooms.size() > 1 and
        rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1].room_;
    }

    if (highest_weighted_room) {
        flak_gun.set_target(highest_weighted_room->position());
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
        for (int x = 15; x > -1; --x) {
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
    if (not app.tutorial_mode() and visible_rooms.size() > 1 and
        rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1];
    }

    if (highest_weighted_room) {
        cannon.set_target(highest_weighted_room->position());
    }
}



void EnemyAI::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    if (app.opponent_island()) {
    }
}



} // namespace skyland
