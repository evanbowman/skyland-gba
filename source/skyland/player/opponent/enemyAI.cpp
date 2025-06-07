////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "enemyAI.hpp"
#include "number/random.hpp"
#include "skyland/entity/drones/droneMeta.hpp"
#include "skyland/entity/projectile/missile.hpp"
#include "skyland/latency.hpp"
#include "skyland/network.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/arcGun.hpp"
#include "skyland/rooms/ballista.hpp"
#include "skyland/rooms/beamGun.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/clumpBomb.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/fireCharge.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/ionCannon.hpp"
#include "skyland/rooms/masonry.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/rocketSilo.hpp"
#include "skyland/rooms/sparkCannon.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/rooms/warhead.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"



// I wrote this AI code frantically for a game jam. I know it's kind of a mess.
// P.S.:
// A few months after the game jam: realistically, I'm not going to rewrite this
// code, because it works just fine and I have a deadline. But really, some of
// the worst code I've ever written.
//
// Later on: I'm adding some comments about how the AI works: The Engine assigns
// an ATP (Attack Target Priority) value to each structure. The AI generally
// uses a simple greedy algorithm to attack the rooms within range with the
// highest ATP values. I thought about making the AI a bit smarter, but people
// have said that the game is already too hard, so no need to write a smarter AI
// I guess.


namespace skyland
{



void EnemyAI::update(Time delta)
{
    if (APP.player_island().is_destroyed()) {
        return;
    }

    if (not APP.opponent_island()) {
        return;
    }
    if (ai_island_ == nullptr or target_island_ == nullptr) {
        ai_island_ = opponent_island();
        target_island_ = &player_island();
    }

    if (ai_island_->power_supply() < ai_island_->power_drain()) {

        // The AI will destroy the least important power-consuming rooms
        // until power balance allows it to attack again. If we destroy the
        // rooms all at once, looks kinda bad, so stagger them a bit.

        insufficent_power_resolve_timer_ += delta;
        if (insufficent_power_resolve_timer_ >
            insufficent_power_resolve_timeout) {

            insufficent_power_resolve_timer_ = 0;
            resolve_insufficient_power();
        }
    }

    next_action_timer_ -= delta;
    character_reassign_timer_ -= delta;
    local_character_reassign_timer_ -= delta;

    drone_update_timer_ -= delta;

    if (drone_update_timer_ <= 0) {
        drone_update_timer_ = drone_update_timeout_;

        for (auto& drone_sp : (*target_island_).drones()) {
            if (drone_sp->parent() == ai_island_) {
                auto& matrix = (*target_island_).rooms_plot();
                drone_set_target(matrix, *drone_sp, ai_island_, target_island_);
            }
        }

        for (auto& drone_sp : ai_island_->drones()) {
            if (drone_sp->parent() == ai_island_) {
                auto& matrix = (*target_island_).rooms_plot();
                drone_set_target(matrix, *drone_sp, ai_island_, target_island_);
            }
        }
    }

    if (next_action_timer_ <= 0) {
        next_action_timer_ = next_action_timeout;

        if (ai_island_) {

            // For a sufficiently small island, we can update all rooms
            // infrequently in a single pass. For large islands, doing this can
            // cause lag, so we instead update the target of one room per
            // iteration of the loop.

            if (ai_island_->rooms().size() > 20 or
                APP.game_mode() == App::GameMode::co_op) {
                if (room_update_index_ >= ai_island_->rooms().size()) {
                    room_update_index_ = 0;
                    next_action_timer_ = milliseconds(500);
                } else {
                    update_room(*ai_island_->rooms()[room_update_index_++],
                                (*target_island_).rooms_plot(),
                                this,
                                ai_island_,
                                target_island_);
                    next_action_timer_ = milliseconds(32);
                }

            } else {
                for (auto& room : ai_island_->rooms()) {
                    update_room(*room,
                                (*target_island_).rooms_plot(),
                                this,
                                ai_island_,
                                target_island_);
                }
            }
        }
    }

    if (character_reassign_timer_ <= 0) {
        character_reassign_timer_ = character_reassign_timeout;

        for (auto& room : (*target_island_).rooms()) {
            for (auto& character : room->characters()) {
                if (character->owner() == this) {
                    assign_boarded_character(
                        *character, this, ai_island_, target_island_);
                }
            }
        }
    }

    if (local_character_reassign_timer_ <= 0) {

        if (ai_island_) {
            if (ai_island_->character_count() > 2) {
                bool reassigned = false;
                for (auto& room : ai_island_->rooms()) {
                    for (auto& character : room->characters()) {
                        if (character->owner() == this) {
                            if (not character->ai_marked()) {
                                assign_local_character(
                                    *character, this, ai_island_);
                                character->ai_mark();
                                reassigned = true;
                                goto DONE;
                            }
                        }
                    }
                }
            DONE:
                if (reassigned) {
                    local_character_reassign_timer_ = milliseconds(200);
                } else {
                    for (auto& room : ai_island_->rooms()) {
                        for (auto& character : room->characters()) {
                            if (character->owner() == this) {
                                if (character->ai_marked()) {
                                    character->ai_unmark();
                                }
                            }
                        }
                    }
                    local_character_reassign_timer_ =
                        character_reassign_timeout - seconds(3);
                }
            } else {
                local_character_reassign_timer_ = character_reassign_timeout;
                for (auto& room : ai_island_->rooms()) {
                    for (auto& character : room->characters()) {
                        if (character->owner() == this) {
                            assign_local_character(
                                *character, this, ai_island_);
                        }
                    }
                }
            }
        }
    }
}



void EnemyAI::drone_set_target(const Bitmatrix<16, 16>& matrix,
                               Drone& drone,
                               Island* ai_island,
                               Island* target_island)
{
    const auto combat_drone_index = DroneMeta::index("combat-drone");
    const auto cannon_drone_index = DroneMeta::index("cannon-drone");
    const auto flak_drone_index = DroneMeta::index("flak-drone");

    if (drone.metaclass_index() == cannon_drone_index or
        drone.metaclass_index() == flak_drone_index) {

        offensive_drone_set_target(matrix, drone, ai_island, target_island);

    } else if (drone.metaclass_index() == combat_drone_index) {
        combat_drone_set_target(matrix, drone, ai_island, target_island);
    }
}



void EnemyAI::assign_weapon_target(Room& weapon,
                                   const RoomCoord& target,
                                   Island* ai_island)
{
    weapon.set_target(target, false);

    // NOTE: in co-op mode, one console controls the AI, and needs to update the
    // other console withe the AI's decisions. We _could_ have both consoles run
    // a copy of the AI, but it's way simpler to have a single instance of the
    // AI.
    network::packet::WeaponSetTarget packet;
    packet.weapon_x_ = weapon.position().x;
    packet.weapon_y_ = weapon.position().y;
    packet.target_x_ = target.x;
    packet.target_y_ = target.y;
    packet.weapon_near_ = ai_island not_eq opponent_island();
    network::transmit(packet);
}



void EnemyAI::update_room(Room& room,
                          const Bitmatrix<16, 16>& matrix,
                          Player* owner,
                          Island* ai_island,
                          Island* target_island)
{
    const auto category = (*room.metaclass())->category();

    if (category == Room::Category::wall) {
        // The AI shouldn't need to care about giving directions to rooms
        // categorized as walls, because the don't really do anything special.
        return;
    }

    // NOTE: there should never be as many as 8 boarded ai characters at
    // once! I doubt that the game can realistically run AI code and
    // pathfinding for all of those entities. Of course, the game _can_
    // handle that many entities, but doing so would result in periodic long
    // pauses.
    //
    // -> P.S. This comment is not accurate at all, but still, there are no
    // rooms that have the capacity to hold 8 characters. The game used to run
    // all character movement logic at once, and it did periodically pause, but
    // the crew movement is more spaced out now.
    Buffer<std::pair<Character*, Room*>, 8> boarded_ai_characters;
    for (auto& room : (*target_island).rooms()) {
        for (auto& character : room->characters()) {
            if (character->owner() == owner) {
                boarded_ai_characters.push_back({character.get(), room.get()});
            }
        }
    }

    if (room.cast<Decimator>()) {
        // Do nothing.
    } else if (auto silo = room.cast<ClumpBomb>()) {
        set_target(matrix, *silo, owner, ai_island, target_island);
    } else if (room.cast<RocketSilo>() or room.cast<Warhead>()) {
        set_target_rocketsilo(matrix, room, owner, ai_island, target_island);
    } else if (auto silo = room.cast<MissileSilo>()) {
        set_target(matrix, *silo, owner, ai_island, target_island);
    } else if (auto beam = room.cast<BeamGun>()) {
        set_target(matrix, *beam, owner, ai_island, target_island);
    } else if (auto flak_gun = room.cast<FlakGun>()) {
        set_target(matrix, *flak_gun, owner, ai_island, target_island);
    } else if (auto ballista = room.cast<Ballista>()) {
        set_target(matrix, *ballista, owner, ai_island, target_island);
    } else if (auto ion_cannon = room.cast<IonCannon>()) {
        set_target(matrix, *ion_cannon, owner, ai_island, target_island);
    } else if (auto arc_gun = room.cast<ArcGun>()) {
        set_target(matrix, *arc_gun, owner, ai_island, target_island);
    } else if (auto spark_cannon = room.cast<SparkCannon>()) {
        if (spark_cannon->level() == 2) {
            spark_cannon->select({});
        }
    } else if (auto fire_charge = room.cast<FireCharge>()) {
        set_target(matrix, *fire_charge, owner, ai_island, target_island);
    } else if (category == Room::Category::weapon or
               (*room.metaclass())->properties() & RoomProperties::plugin) {
        // NOTE: if we haven't hit any of the cases above, assume that the
        // weapon is a generic cannon-type weapon.
        set_target(matrix, room, owner, ai_island, target_island);
    } else if (auto db = room.cast<DroneBay>()) {
        // Don't spawn drones until the level's been running for a
        // bit.
        if (ai_island->get_drift() == 0.0_fixed) {
            if (APP.game_speed() not_eq GameSpeed::stopped) {
                update_drone_bay(matrix, *db, ai_island, target_island);
            }
        }
    } else if (auto transporter = room.cast<Transporter>()) {
        if (length(transporter->characters()) and transporter->ready()) {
            auto transport_chr = transporter->characters().begin();
            if ((*transport_chr)->state() not_eq
                    Character::State::repair_room and
                (*transport_chr)->owner() == owner) {


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
                        not(*transport_chr)->get_movement_path()->empty() and
                        (not((*(*transport_chr)->get_movement_path())[0] ==
                             (*transport_chr)->grid_position())))) {

                    transporter->transport_occupant();
                }
            }
        } else if (transporter->ready()) {
            // If we have an infirmary, potentially transport some
            // of our injured AIs back to heal.

            bool found_infirmary = false;

            auto metac = load_metaclass("infirmary");
            for (auto& room : ai_island->rooms()) {
                if (room->metaclass() == metac) {
                    found_infirmary = true;
                }
            }

            if (found_infirmary) {
                auto recover_pos = [&]() -> Optional<RoomCoord> {
                    for (auto it = boarded_ai_characters.begin();
                         it not_eq boarded_ai_characters.end();) {
                        if ((*it).first->health() < 25 and
                            not(*it).first->is_replicant()) {
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
                        if (str_cmp((*(*it).second->metaclass())->name(),
                                    "plundered-room") == 0 and
                            (*it).first->idle_count() > 600) {
                            // If we've plundered a room, and we've
                            // been waiting a while, i.e. we have
                            // nowhere to go, then we should recover
                            // the character.
                            it = boarded_ai_characters.erase(it);
                            recover_pos = (*it).first->grid_position();
                        } else {
                            ++it;
                        }
                    }
                }

                if (recover_pos) {
                    transporter->recover_character(*recover_pos);
                }
            }
        }
    }
}



void EnemyAI::resolve_insufficient_power()
{
    // When we have more power drain than power supply, we need to salvage a
    // bunch of structures to rebalance our power usage.

    // TODO: Make the AI less stupid.

    Room* lowest_weighted_room = nullptr;
    ATP lowest_weight = 2000000.0_atp;

    for (auto& room : ai_island_->rooms()) {
        auto name = (*room->metaclass())->name();
        if (str_eq(name, "reactor") or str_eq(name, "power-core")) {
            // We certainly won't restore power by scrapping our remaining power
            // supply.
            continue;
        }
        if ((*room->metaclass())->properties() &
            RoomProperties::salvage_disallowed) {
            continue;
        }
        auto pwr = room->power_usage();
        if (pwr > 0) {
            auto w = room->get_atp();
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
        lowest_weighted_room->apply_damage(Room::health_upper_limit());
    }
}



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y);



void flood_fill_through_portals(Island& isle,
                                u8 matrix[16][16],
                                u8 replace,
                                u8 x,
                                u8 y);



void EnemyAI::assign_local_character(Character& character,
                                     Player* owner,
                                     Island* ai_island_,
                                     bool repair_priority)
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

    Buffer<RoomCoord, 32> exclude_slots;

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
    int weapon_count = 0;
    int cannon_count = 0;
    int missile_count = 0;
    u8 decimator_count = 0;
    u8 player_decimator_count = 0;

    int player_weapon_count = 0;
    int player_cannon_count = 0;
    int player_missile_count = 0;

    bool damaged_habitable_rooms = false;

    Buffer<Room*, 10> radiators;

    for (auto& room : ai_island_->rooms()) {
        auto mt = room->metaclass();

        if (room->health() not_eq room->max_health() and
            (*mt)->properties() & RoomProperties::habitable) {
            damaged_habitable_rooms = true;
        }
        if (mt == cannon_mt) {
            ++weapon_count;
            ++cannon_count;
        } else if (mt == missile_silo_mt) {
            ++weapon_count;
            ++missile_count;
        } else if (mt == decimator_mt) {
            ++weapon_count;
            ++decimator_count;
        } else if (mt == flak_gun_mt) {
            ++weapon_count;
        } else if (mt == radiator_mt) {
            radiators.push_back(room.get());
        }
        for (auto& other : room->characters()) {
            if (other->owner() == owner and other.get() not_eq &character) {
                if (auto dest = other->destination()) {
                    exclude_slots.push_back(*dest);
                } else {
                    exclude_slots.push_back(other->grid_position());
                }
                ++ai_characters_local;
            } else if (other->owner() not_eq owner) {
                ++player_characters_local;
            } else if (other.get() == &character) {
                ++ai_characters_local;
            }
        }
    }

    for (auto& room : APP.player_island().rooms()) {
        auto mt = room->metaclass();
        if (mt == cannon_mt) {
            ++player_weapon_count;
            ++player_cannon_count;
        } else if (mt == missile_silo_mt) {
            ++player_weapon_count;
            ++player_missile_count;
        } else if (mt == decimator_mt) {
            ++player_weapon_count;
            ++player_decimator_count;
        } else if (mt == flak_gun_mt) {
            ++player_weapon_count;
        }
        for (auto& chr : room->characters()) {
            if (chr->owner() == owner) {
                ++ai_characters_remote;
            } else {
                ++player_characters_remote;
            }
        }
    }


    DynamicMemory<bool[16][16]> matrix_ =
        allocate_dynamic<bool[16][16]>("ai-rooms-plot");

    ai_island_->plot_walkable_zones(*matrix_, &character);



    u8 matrix[16][16];
    for (u32 x = 0; x < ai_island_->terrain().size(); ++x) {
        for (int y = construction_zone_min_y; y < 15; ++y) {
            if ((*matrix_)[x][y]) {
                matrix[x][y] = 1;
            } else {
                matrix[x][y] = 0;
            }
        }
    }

    auto current_pos = character.grid_position();

    flood_fill_through_portals(
        *ai_island_, matrix, 2, current_pos.x, current_pos.y);

    struct Destination
    {
        RoomCoord coord_;
        ATP ai_weight_;
    };

    Buffer<Destination, 48> slots;

    for (u8 x = 0; x < ai_island_->terrain().size(); ++x) {
        for (u8 y = construction_zone_min_y; y < 15; ++y) {
            if (matrix[x][y] == 2) {
                slots.push_back({{x, y}, 0.0_atp});
            }
        }
    }

    if (slots.empty()) {
        return; // hmm...
    }


    for (auto& slot : slots) {

        bool excluded = false;
        for (auto& exc : exclude_slots) {
            if (slot.coord_ == exc) {
                // Don't move into a slot targeted by another one of our ai
                // characters.
                slot.ai_weight_ = ATP::from_integer(-2000);
                excluded = true;
            }
        }

        if (excluded) {
            continue;
        }

        if (auto room = ai_island_->get_room(slot.coord_)) {

            auto mt = room->metaclass();

            const auto base_weight = room->get_atp();

            slot.ai_weight_ = base_weight;

            for (auto& r : radiators) {
                if (abs(r->position().x - slot.coord_.x) < 3 and
                    abs(r->position().y - slot.coord_.y) < 3) {
                    slot.ai_weight_ -= 600.0_atp;
                }
            }

            if (room->health() not_eq room->max_health()) {
                // Increase room weight if damaged.
                slot.ai_weight_ +=
                    (base_weight - base_weight * ATP(float(room->health()) /
                                                     (room->max_health())));
            } else {
                // FIXME: this causes crewmembers to cycle back and forth
                // between rooms.

                // if (length(room->characters()) > 3) {
                //     slot.ai_weight_ -= 100.0_atp;
                // }
            }

            if (room->is_powered_down()) {
                slot.ai_weight_ /= 2.0_atp;
            }

            if (room->owner() not_eq ai_island_) {
                slot.ai_weight_ += 150.0_atp;
            }

            if (ai_island_->fire_present(slot.coord_) and
                // NOTE: if room health less than eight, don't move into the
                // slot that's on fire, because we may not be able to put out
                // the fire before it destroys the room. The character may be
                // able to repair the room somewhat from another adjacent slot
                // first, and then put out the fire.
                room->health() > 8) {
                // Yeah, really important to put out any fires!
                slot.ai_weight_ += 1500.0_atp;
            }

            if (room->health() not_eq room->max_health()) {
                slot.ai_weight_ += 500.0_atp;
            }

            slot.ai_weight_ -= ATP::from_integer(
                3 * manhattan_length(slot.coord_, current_pos));

            if (not room->is_powered_down() and mt == infirmary_mt) {

                auto chr_room = ai_island_->get_room(character.grid_position());

                // If our health is really low, we probably want to go to the
                // infirmary. If our health is just kinda low, we maybe want to
                // go to the infirmary.
                if (character.is_replicant()) {
                    // Replicants cannot heal, so don't bother.
                } else {
                    const auto chr_max_health = character.get_max_health();
                    if (not damaged_habitable_rooms and
                        character.health() < chr_max_health) {
                        // Character has no rooms to repair and character is
                        // injured.
                        slot.ai_weight_ += 5000.0_atp;
                    } else if (character.health() < 25) {
                        slot.ai_weight_ += 2000.0_atp;
                    } else if (character.health() < 200 and
                               character.health() < chr_max_health and
                               not player_characters_local) {
                        slot.ai_weight_ += 2000.0_atp;
                    } else {
                        if (chr_room and chr_room == room and
                            room->health() == room->max_health()) {
                            // The crewmember is in the infirmary, and the
                            // crewmember is fully healed, and the infirmary is
                            // fully healed, then vacate the infirmary to allow
                            // someone else to heal.
                            slot.ai_weight_ -= 400.0_atp;
                        }
                    }
                }
            } else if (not room->is_powered_down() and mt == transporter_mt) {

                auto transporter = room->cast<Transporter>();

                // Now, let's see. We want to raid the player's island if we
                // have more characters, but also...
                //
                // TODO: Increase the transporter weight a lot if we don't have
                // any remaining offensive capabilities. In that case, raids
                // would be our only offense.
                if (damaged_habitable_rooms and repair_priority) {
                    // ...
                } else if (transporter->ready()) {

                    const bool co_op_mode =
                        APP.game_mode() == App::GameMode::co_op;

                    if (player_characters_remote >= ai_characters_remote and
                        ai_characters_remote + ai_characters_local >
                            player_characters_remote) {

                        if (co_op_mode) {
                            // Make boarding less likely in co_op mode, as
                            // character combat is sort of difficult without
                            // being able to pause.
                            slot.ai_weight_ +=
                                ATP::from_integer(100 * ai_characters_local);
                        } else {
                            slot.ai_weight_ +=
                                ATP::from_integer(400 * ai_characters_local);
                        }
                    }
                    if (player_characters_local > ai_characters_local) {
                        slot.ai_weight_ -=
                            ATP::from_integer(250 * (player_characters_local -
                                                     ai_characters_local));
                    }
                    if (weapon_count == 0) {
                        // If we don't have any remaining weapons, potentially
                        // board the player's castle, even if doing so would be
                        // a suicide mission.
                        slot.ai_weight_ += 1000.0_atp;
                    } else if (missile_count and cannon_count == 0 and
                               player_cannon_count) {
                        slot.ai_weight_ += 400.0_atp;
                    } else if (cannon_count and missile_count == 0 and
                               player_missile_count) {
                        slot.ai_weight_ += 400.0_atp;
                    }
                    if (decimator_count == 0 and player_decimator_count) {
                        slot.ai_weight_ += 600.0_atp;
                    }
                } else {
                    slot.ai_weight_ -= 300.0_atp;
                }
            }

            if (weapon_count < player_weapon_count) {
                if (mt == decimator_mt) {
                    slot.ai_weight_ += 500.0_atp;
                }
            }
        }
    }

    if (character.wants_to_chat()) {
        for (auto& slot : slots) {
            if (auto chr = ai_island_->character_at_location(slot.coord_)) {
                if (chr not_eq &character) {
                    if (chr->wants_to_chat()) {
                        for (auto& sl : slots) {
                            auto d = manhattan_length(slot.coord_, sl.coord_);
                            auto is_adjacent = d < 2;
                            if (sl.coord_ not_eq slot.coord_ and is_adjacent) {
                                if (sl.ai_weight_ > 0.0_atp) {
                                    sl.ai_weight_ += 2000.0_atp;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    std::sort(slots.begin(),
              slots.end(),
              [&](const Destination& lhs, const Destination& rhs) {
                  return lhs.ai_weight_ < rhs.ai_weight_;
              });

    if (slots.back().ai_weight_ == 0.0_atp) {
        // Again, perhaps this is overly defensive coding. But we should never
        // end up in a situation where the weights of the rooms are all
        // uninitialized...
        return;
    }


    auto target = slots.back();

    if (auto path =
            find_path(ai_island_, &character, current_pos, target.coord_)) {
        if (not((*path)->size() == 1 and
                (**path)[0] == character.grid_position())) {
            // Don't waste a path buffer on an entity if the ideal path
            // represents a single node with the character's current position.
            character.set_movement_path(std::move(*path));

            network::packet::ChrSetTargetV2 packet;
            packet.target_x_ = target.coord_.x;
            packet.target_y_ = target.coord_.y;
            packet.chr_id_.set(character.id());

            if (APP.game_mode() == App::GameMode::multiplayer) {
                packet.near_island_ = false;
            } else {
                packet.near_island_ = true;
            }

            network::transmit(packet);
        }
    }
}



void EnemyAI::assign_boarded_character(Character& character,
                                       Player* owner,
                                       Island* ai_island_,
                                       Island* target_island_)
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


    Buffer<RoomCoord, 32> exclude_slots; // Don't move into currently occupied
                                         // slots, or slots that will be
                                         // occupied.

    for (auto& room : (*target_island_).rooms()) {
        for (auto& other : room->characters()) {
            if (other->owner() == owner and other.get() not_eq &character) {
                if (auto dest = other->destination()) {
                    exclude_slots.push_back(*dest);
                } else {
                    exclude_slots.push_back(other->grid_position());
                }
            }
        }
    }


    DynamicMemory<bool[16][16]> matrix_ =
        allocate_dynamic<bool[16][16]>("ai-chr-slots");

    (*target_island_).plot_walkable_zones(*matrix_, &character);

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

    flood_fill_through_portals(
        *target_island_, matrix, 2, current_pos.x, current_pos.y);

    struct Destination
    {
        RoomCoord coord_;
        ATP ai_weight_;
    };

    Buffer<Destination, 48> slots;

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            if (matrix[x][y] == 2) {
                slots.push_back({{x, y}, 0.0_atp});
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
        if (auto room = (*target_island_).get_room(slot.coord_)) {
            slot.ai_weight_ = room->get_atp();
            slot.ai_weight_ -= ATP::from_integer(
                3 * manhattan_length(slot.coord_, current_pos));

            if (room->is_powered_down()) {
                slot.ai_weight_ /= 2.0_atp;
            }

            if ((*target_island_).fire_present(slot.coord_)) {
                // The slot is already on fire! Maybe we can do more damage
                // elsewhere...
                slot.ai_weight_ -= 800.0_atp;
            }

            ATP player_chr_remove_weight = 0.0_atp;
            for (auto& chr : room->characters()) {
                // If the player has a bunch of characters in the room, label it
                // as "toxic" and resist allocating entities to the room (unless
                // it's really valuable).
                if (chr->owner() not_eq owner) {
                    player_chr_remove_weight += 100.0_atp;

                    if (character.health() < 50) {
                        // More likely to flee the room if we're badly injured.
                        player_chr_remove_weight += 100.0_atp;
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
                player_chr_remove_weight /
                (0.75_atp * ATP::from_integer(exclude_slots.size() + 1));
        }
        for (auto& exc : exclude_slots) {
            if (slot.coord_ == exc) {
                // Don't move into a slot targeted by another one of our ai
                // characters.
                slot.ai_weight_ = ATP::from_integer(-2000);
            }
        }
    }

    std::sort(slots.begin(),
              slots.end(),
              [&](const Destination& lhs, const Destination& rhs) {
                  return lhs.ai_weight_ < rhs.ai_weight_;
              });

    if (slots.back().ai_weight_ == 0.0_atp) {
        // Again, perhaps this is overly defensive coding. But we should never
        // end up in a situation where the weights of the rooms are all
        // uninitialized...
        return;
    }

    auto target = slots.back();

    if (auto path = find_path(
            &(*target_island_), &character, current_pos, target.coord_)) {
        if (not((*path)->size() == 1 and
                (**path)[0] == character.grid_position())) {
            // Don't waste a path buffer on an entity if the ideal path
            // represents a single node with the character's current position.
            character.set_movement_path(std::move(*path));

            network::packet::ChrSetTargetV2 packet;
            packet.target_x_ = target.coord_.x;
            packet.target_y_ = target.coord_.y;
            packet.chr_id_.set(character.id());
            if (APP.game_mode() == App::GameMode::multiplayer) {
                // NOTE: because in multiplayer, both players are on the same
                // side of the screen, so we need to invert some fields so that
                // things are correct from the receiver's perspective.
                packet.near_island_ = true;
            } else {
                packet.near_island_ = false;
            }
            network::transmit(packet);
        }
    }
}



template <typename T> T abs_fp(const T& val)
{
    return (val > 0.0_fixed) ? val : val * Fixnum::from_integer(-1);
}



template <typename T> T manhattan_length_fp(const Vec2<T>& a, const Vec2<T>& b)
{
    return abs_fp(a.x - b.x) + abs_fp(a.y - b.y);
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         IonCannon& ion_cannon,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.00003_atp;



    for (auto& room : (*target_island).rooms()) {
        auto meta_c = room->metaclass();

        auto w = room->get_atp();

        if (not room->ai_aware()) {
            // ...
        } else if ((ai_island->has_radar() or (*target_island).is_boarded()) and
                   str_cmp((*meta_c)->name(), "reactor") == 0) {
            w += 3.0_atp *
                 manhattan_length_fp(room->origin(), ion_cannon.origin());
        } else if (not is_forcefield(meta_c) and
                   str_cmp((*meta_c)->name(), "energized-hull") not_eq 0) {
            continue;
        } else {
            w += 3.0_atp *
                 manhattan_length_fp(room->origin(), ion_cannon.origin());
        }


        if (w > highest_weight) {
            highest_weighted_room = room.get();
            highest_weight = w;
        }
    }

    if (highest_weighted_room) {
        assign_weapon_target(
            ion_cannon, highest_weighted_room->position(), ai_island);
    }
}



static void place_offensive_drone(DroneBay& db,
                                  bool slot[16][16],
                                  const Bitmatrix<16, 16>& player_rooms,
                                  bool left_anchor,
                                  bool restrict_columns[16],
                                  const DroneMeta* metac,
                                  Island& player_island,
                                  Island& ai_island)
{
    ATP left_column_weights[16];
    for (auto& val : left_column_weights) {
        val = 0.0_atp;
    }

    const int right_column = player_island.terrain().size() - 1;

    for (auto& drone_sp : player_island.drones()) {
        // Don't stack drones in the same column, or we could end up
        // shooting one of our other drones.
        if (drone_sp->parent() == &ai_island) {
            restrict_columns[drone_sp->position().x] = true;
        }
    }

    ATP top_row_weights[16];
    for (int i = 0; i < 16; ++i) {
        if (restrict_columns[i]) {
            top_row_weights[i] = ATP::from_integer(-10000);
        } else {
            top_row_weights[i] = 0.0_atp;
        }
    }

    // Find lowest available slot y-value, into which we may place a drone.
    u8 min_y = 15;
    for (int y = 15; y > 0; --y) {
        if (slot[1][y]) {
            min_y = y;
            break;
        }
    }

    if (left_anchor) {
        // 9-14 range: check immediately rightwards.
        if (not restrict_columns[0]) {
            for (u8 y = 9; y < 14; ++y) {
                if (slot[0][y]) {
                    for (u8 x = 1; x < 16; ++x) {
                        if (player_rooms.get(x, y)) {
                            if (auto room = player_island.get_room({x, y})) {
                                left_column_weights[y] = room->get_atp();
                                break;
                            }
                        }
                    }
                    if (left_column_weights[y] == 0.0_atp) {
                        left_column_weights[y] = 0.5_atp;
                    }
                }
            }
        }
    } else {
        // 9-14 range: check immediately rightwards.
        if (not restrict_columns[right_column]) {
            for (u8 y = 9; y < 14; ++y) {
                if (slot[0][y]) {
                    for (u8 x = right_column; x > 0; --x) {
                        if (player_rooms.get(x, y)) {
                            if (auto room = player_island.get_room({x, y})) {
                                left_column_weights[y] = room->get_atp();
                                break;
                            }
                        }
                    }
                    if (left_column_weights[y] == 0.0_atp) {
                        left_column_weights[y] = 0.5_atp;
                    }
                }
            }
        }
    }

    if (left_anchor) {
        // range 6-10: check diagonally right/down
        if (not restrict_columns[0]) {
            for (u8 y = 7; y < 9; ++y) {
                if (slot[0][y]) {
                    RoomCoord cursor{1, u8(y + 1)};
                    while (cursor.x < 16 and cursor.y < 15) {
                        if (player_rooms.get(cursor.x, cursor.y)) {
                            if (auto room = player_island.get_room(cursor)) {
                                left_column_weights[y] = room->get_atp();
                                break;
                            }
                        }
                        ++cursor.x;
                        ++cursor.y;
                    }
                    if (left_column_weights[y] == 0.0_atp) {
                        left_column_weights[y] = 0.5_atp;
                    }
                }
            }
        }
    } else {
        // range 6-10: check diagonally right/down
        if (not restrict_columns[right_column]) {
            for (u8 y = 7; y < 9; ++y) {
                if (slot[right_column][y]) {
                    RoomCoord cursor{u8(right_column - 1), u8(y + 1)};
                    while (cursor.x > 0 and cursor.y < 15) {
                        if (player_rooms.get(cursor.x, cursor.y)) {
                            if (auto room = player_island.get_room(cursor)) {
                                left_column_weights[y] = room->get_atp();
                                break;
                            }
                        }
                        --cursor.x;
                        ++cursor.y;
                    }
                    if (left_column_weights[y] == 0.0_atp) {
                        left_column_weights[y] = 0.5_atp;
                    }
                }
            }
        }
    }

    // top row: check vertically down
    // NOTE: += 2 because we don't want to place two drones directly adjacent,
    // as they might accidentally shoot eachother.
    for (u8 x = 0; x < 16; x += 2) {
        if (not restrict_columns[x]) {
            for (int yy = 14; yy > 0; --yy) {
                if (slot[x][yy]) {
                    for (u8 y = yy; y < 15; ++y) {
                        if (player_rooms.get(x, y)) {
                            if (auto room = player_island.get_room({x, y})) {
                                top_row_weights[x] = room->get_atp();
                            }
                            break;
                        }
                    }
                    if (top_row_weights[x] == 0.0_atp) {
                        top_row_weights[x] = 0.5_atp;
                    }
                }
            }
        }
    }

    Optional<RoomCoord> ideal_coord;
    ATP max_weight = 0.0_atp;
    for (u8 y = construction_zone_min_y; y < 15; ++y) {
        if (left_column_weights[y] > max_weight) {
            if (left_anchor) {
                ideal_coord = {0, y};
            } else {
                ideal_coord = {(u8)right_column, y};
            }

            max_weight = left_column_weights[y];
        }
    }

    for (u8 x = 0; x < 16; ++x) {
        if (top_row_weights[x] > max_weight) {
            ideal_coord = {x, min_y};
            max_weight = top_row_weights[x];
        }
    }

    auto create_pos = db.position();
    create_pos.y -= 1;

    if (ideal_coord) {
        auto drone = (*metac)->create(&ai_island, &player_island, create_pos);
        if (drone) {
            (*drone)->set_movement_target(*ideal_coord);
            db.attach_drone(*drone);
            player_island.drones().push(*drone);
        }
    } else {
        // ...
    }
}



void get_drone_slots(bool slots[16][16], Island* dest_island, Island* parent);



void EnemyAI::update_drone_bay(const Bitmatrix<16, 16>& matrix,
                               DroneBay& db,
                               Island* ai_island,
                               Island* target_island)
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
    ATP weights[weight_array_size];
    for (auto& w : weights) {
        w = 0.0_atp;
    }

    if (weight_array_size < ds) {
        PLATFORM.fatal("drone weight array insufficiently sized");
    }

    // We want to ideally place flak-drones with a greater likelihood than
    // cannon drones, unless certain conditions arise (see below).
    weights[flak_drone_index] = 24.0_atp;
    weights[cannon_drone_index] = 16.0_atp;

    bool player_missile_silos[16];
    bool opponent_missile_silos[16];
    for (int i = 0; i < 16; ++i) {
        player_missile_silos[i] = 0;
        opponent_missile_silos[i] = 0;
    }


    auto ai_controlled = [&](Drone& d) { return d.parent() == ai_island; };

    for (auto& room : ai_island->rooms()) {
        if (room->metaclass() == missile_silo_mt) {
            opponent_missile_silos[room->position().x] = true;
        }
    }

    for (auto& room : (*target_island).rooms()) {
        // NOTE: this loop assumes that the initial weight for combat drones is
        // zeroe'd.
        if (room->metaclass() == drone_bay_mt) {
            bool is_recons_drone = false;
            if (auto opt_drone = room->drone()) {
                if (str_eq((*opt_drone)->name(), "reconstruction-drone")) {
                    is_recons_drone = true;
                }
            }
            if (is_recons_drone) {
                // Hmm.. how do you do negative literals?
                // Anyway, reconstruction_drones are immune to damage, so don't
                // deploy combat drones.
                weights[combat_drone_index] = 0.0_atp - 90.0_atp;
            } else if (weights[combat_drone_index] == 0.0_atp) {
                // If the player has drone bays, he/she may deploy drones, in
                // which case, we might want to think about proactively
                // deploying a combat drone of our own.
                weights[combat_drone_index] = 14.0_atp;
            } else {
                // For each DroneBay controlled by the player, it becomes
                // increasingly likely that the player will be deploying drones,
                // in which case, we'll want to be able to defend ourself.
                weights[combat_drone_index] *= 2.0_atp;
            }
        } else if (room->metaclass() == missile_silo_mt) {
            // DroneBay rooms quite are vulnerable to missiles, so we want to
            // preferentially deploy cannon drones rather than flak drones if
            // the player has a lot of missile silos.
            weights[cannon_drone_index] += 8.0_atp;

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
                weights[combat_drone_index] -= 48.0_atp;
            } else if (metac == cannon_drone_index) {
                weights[cannon_drone_index] -= 8.0_atp;
            }
        } else {
            if (metac == combat_drone_index) {
                // If the player has a combat drone, any drone that we place
                // would be particularly vulnerable, so we need to take out the
                // player's drone first.
                weights[combat_drone_index] += 32.0_atp;
            } else if (metac == cannon_drone_index or
                       metac == flak_drone_index) {
                // If the player has a couple of offensive drones, we may want
                // to react defensively.
                weights[combat_drone_index] += 8.0_atp;
                if (local) {
                    weights[combat_drone_index] += 16.0_atp;
                }
            }
        }
    };

    for (auto& drone_sp : (*target_island).drones()) {
        update_weights(*drone_sp, false);
    }

    for (auto& drone_sp : ai_island->drones()) {
        update_weights(*drone_sp, true);
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
            slots[x][y] = y >= construction_zone_min_y;
        }
    }


    db.start_reload();


    auto create_pos = db.position();
    create_pos.y -= 1;

    // auto place_offensive_drone = [&](DroneMeta* metac) {
    //     get_drone_slots(slots, &(*target_island_), &*ai_island_);
    // };


    if (highest_weight_index == cannon_drone_index) {

        get_drone_slots(slots, &(*target_island), ai_island);

        place_offensive_drone(db,
                              slots,
                              matrix,
                              ai_island == APP.opponent_island(),
                              player_missile_silos,
                              &dt[cannon_drone_index],
                              (*target_island),
                              *ai_island);

    } else if (highest_weight_index == combat_drone_index) {

        get_drone_slots(slots, ai_island, &*ai_island);

        for (u8 y = 0; y < 16; y += 2) {
            for (u8 x = 0; x < 16; x += 2) {
                if (slots[x][y] and not opponent_missile_silos[x]) {
                    auto drone = dt[combat_drone_index]->create(
                        ai_island, ai_island, create_pos);
                    if (drone) {
                        (*drone)->set_movement_target({x, y});
                        db.attach_drone(*drone);
                        ai_island->drones().push(*drone);
                        return;
                    }
                }
            }
        }

    } else if (highest_weight_index == flak_drone_index) {

        get_drone_slots(slots, &(*target_island), ai_island);

        place_offensive_drone(db,
                              slots,
                              matrix,
                              ai_island == APP.opponent_island(),
                              player_missile_silos,
                              &dt[flak_drone_index],
                              (*target_island),
                              *ai_island);
    }
}



void EnemyAI::combat_drone_set_target(const Bitmatrix<16, 16>& matrix,
                                      Drone& drone,
                                      Island* ai_island,
                                      Island* target_island)
{
    for (auto& drone_sp : (*target_island).drones()) {
        if (drone_sp->parent() == target_island) {
            drone.set_target(drone_sp->position(), false, true);
        }
    }

    for (auto& drone_sp : ai_island->drones()) {
        if (drone_sp->parent() == target_island) {
            drone.set_target(drone_sp->position(), false, false);
        }
    }
}



static bool test_local_reachability(const Bitmatrix<16, 16>& matrix,
                                    int x0,
                                    int y0,
                                    int x1,
                                    int y1)
{
    int dx = abs(x1 - x0);
    int sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0);
    int sy = y0 < y1 ? 1 : -1;
    int error = dx + dy;

    while (true) {
        if (x0 == x1 && y0 == y1) {
            return true;
        }
        if (matrix.get(x0, y0)) {
            return false;
        }
        int e2 = 2 * error;
        if (e2 >= dy) {
            if (x0 == x1) {
                break;
            }
            error = error + dy;
            x0 = x0 + sx;
        }
        if (e2 <= dx) {
            if (y0 == y1) {
                break;
            }
            error = error + dx;
            y0 = y0 + sy;
        }
    }

    return true;
}



void EnemyAI::offensive_drone_set_target(const Bitmatrix<16, 16>& matrix,
                                         Drone& drone,
                                         Island* ai_island,
                                         Island* target_island)
{
    if (drone.target_checksum() == target_island->checksum()) {
        return; // No reason to recalculate...
    }

    Optional<RoomCoord> ideal_pos;
    ATP highest_weight = 0.0_atp;

    const auto drone_pos = drone.position();

    auto enqueue = [&](auto& room) {
        auto weight = room->get_atp();
        if (weight > highest_weight) {
            highest_weight = weight;
            ideal_pos = room->position();
        }
    };

    Vector<Room*> outer;
    collect_outer_rooms(*target_island, outer);
    // FIXME: what about sorting the rooms by weight? Then we can break out of
    // the below loop early... but iterating isn't that expensive, whereas
    // sorting could be costly... of course, I could and should just measure it
    // both ways... which is why I haven't made the proposed changes yet...

    for (auto& room : outer) {
        for (int x = 0; x < room->size().x; ++x) {
            for (int y = 0; y < room->size().y; ++y) {
                int rx = room->position().x + x;
                int ry = room->position().y + y;
                if (room->get_atp() > highest_weight and
                    test_local_reachability(target_island->rooms_plot(),
                                            drone_pos.x,
                                            drone_pos.y,
                                            rx,
                                            ry)) {
                    // PLATFORM.set_tile(target_island->layer(),
                    //                   rx,
                    //                   ry, StaticTile::path_marker);
                    enqueue(room);
                }
            }
        }
    }

    if (ideal_pos) {
        // PLATFORM.set_tile(target_island->layer(),
        //                   ideal_pos->x,
        //                   ideal_pos->y, StaticTile::airborne_selection);
        drone.set_target(*ideal_pos, false, false);

        drone.target_checksum() = target_island->checksum();
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         ClumpBomb& silo,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    if (silo.parent()->get_drift() not_eq 0.0_fixed) {
        // Wait until we've stopped moving
        return;
    }

    Buffer<std::pair<Room*, ATP>, 64> visible_rooms;


    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            if (matrix.get(x, y)) {
                if (auto room = (*target_island).get_room({u8(x), u8(y)})) {
                    visible_rooms.push_back({room, room->get_atp()});
                    if (room->ai_aware() and
                        room->category() not_eq Room::Category::decoration) {
                        break;
                    }
                }
            }
        }
    }

    for (auto& info : visible_rooms) {
        auto pos = info.first->position();

        if (auto room = (*target_island).get_room({pos.x, u8(pos.y - 1)})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }

        if (auto room = (*target_island).get_room({pos.x, u8(pos.y + 1)})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }

        if (auto room = (*target_island).get_room({u8(pos.x + 1), pos.y})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }

        if (auto room = (*target_island).get_room({u8(pos.x - 1), pos.y})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }
    }

    if (visible_rooms.empty()) {
        return;
    }

    ATP highest_weight = ATP::from_integer(-1);
    Room* best_room = nullptr;

    for (auto& info : visible_rooms) {
        if (info.second > highest_weight) {
            highest_weight = info.second;
            best_room = info.first;
        }
    }

    if (not best_room) {
        // Should never happen.
        return;
    }

    assign_weapon_target(silo, best_room->position(), ai_island);
}



void EnemyAI::set_target_rocketsilo(const Bitmatrix<16, 16>& matrix,
                                    Room& silo,
                                    Player* owner,
                                    Island* ai_island,
                                    Island* target_island)
{
    if (silo.parent()->get_drift() not_eq 0.0_fixed) {
        // Wait until we've stopped moving
        return;
    }

    Buffer<std::pair<Room*, ATP>, 64> visible_rooms;


    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            if (matrix.get(x, y)) {
                if (auto room = (*target_island).get_room({u8(x), u8(y)})) {
                    visible_rooms.push_back({room, room->get_atp()});
                    if (room->ai_aware() and
                        room->category() not_eq Room::Category::decoration) {
                        break;
                    }
                }
            }
        }
    }

    for (auto& info : visible_rooms) {
        auto pos = info.first->position();

        if (auto room = (*target_island).get_room({pos.x, u8(pos.y - 1)})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }

        if (auto room = (*target_island).get_room({pos.x, u8(pos.y + 1)})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }

        if (auto room = (*target_island).get_room({u8(pos.x + 1), pos.y})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }

        if (auto room = (*target_island).get_room({u8(pos.x - 1), pos.y})) {
            ATP mult = 0.5_atp;
            if (not(*target_island).fire_present(room->position())) {
                mult = 0.8_atp;
            }
            info.second += mult * room->get_atp();
        }
    }

    if (visible_rooms.empty()) {
        return;
    }

    ATP highest_weight = ATP::from_integer(-1);
    Room* best_room = nullptr;

    for (auto& info : visible_rooms) {
        if (info.second > highest_weight) {
            highest_weight = info.second;
            best_room = info.first;
        }
    }

    if (not best_room) {
        // Should never happen.
        return;
    }

    assign_weapon_target(silo, best_room->position(), ai_island);
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         MissileSilo& silo,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    if (silo.parent()->get_drift() not_eq 0.0_fixed) {
        // Wait until we've stopped moving
        return;
    }

    Buffer<Room*, 32> visible_rooms;

    // In many cases, the player will have covered the entire surface of his/her
    // castle with hull blocks. Peek into the y + 1 tile, in case we get to a
    // point where we only see hull blocks.
    Buffer<Room*, 32> second_tier;

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            if (matrix.get(x, y)) {
                if (auto room = (*target_island).get_room({u8(x), u8(y)})) {
                    visible_rooms.push_back(room);
                    if (matrix.get(x, y + 1)) {
                        if (auto st_room =
                                (*target_island).get_room({u8(x), u8(y + 1)})) {
                            second_tier.push_back(st_room);
                        }
                    }
                }
                break;
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.00003_atp;

    auto cannon_metac = load_metaclass("cannon");

    bool cannons_remaining = false;

    for (auto& room : silo.parent()->rooms()) {
        if (room->metaclass() == cannon_metac) {
            cannons_remaining = true;
            break;
        }
    }

    Room* highest_weighted_second_tier = nullptr;
    ATP highest_second_tier_weight = 0.00003_atp;
    for (auto& room : second_tier) {
        auto meta_c = room->metaclass();
        auto w = room->get_atp();

        // We don't have any cannons left, but the other player does. Try to
        // take out some of those cannons with missiles.
        if (room->ai_aware() and meta_c == cannon_metac and
            not cannons_remaining) {
            w += 200.0_atp;
        }

        if (w > highest_second_tier_weight) {
            highest_second_tier_weight = w;
            highest_weighted_second_tier = room;
        }
    }

    for (auto room : visible_rooms) {
        auto meta_c = room->metaclass();
        auto w = room->get_atp();

        for (auto& chr : room->characters()) {
            const bool ai_owns_character = chr->owner() == owner;
            if (ai_owns_character) {
                // Sometimes we want to blow up a room that contains some of our
                // own invading characters, but often, we don't :)
                //
                // We can make this calculation a bit more complex,
                // though. E.G. if we're attacking a power core, and we don't
                // realistically have enough weapons to destroy the player's
                // offensive capabilities, then the only path to victory may be
                // through destroying rooms containing our own characters.
                w -= 200.0_atp;
            }
        }

        // Give the room some extra weight, if firing a missile into it would be
        // really destructive.
        if (w > 400.0_atp and room->health() <= missile_damage) {
            w += 300.0_atp;
        }

        // We don't have any cannons left, but the other player does. Try to
        // take out some of those cannons with missiles.
        if (meta_c == cannon_metac and not cannons_remaining) {
            w += 200.0_atp;
        }

        if (w > highest_weight) {
            highest_weighted_room = room;
            highest_weight = w;
        }
    }

    if (highest_weighted_room) {

        Room* target = highest_weighted_room;

        if (highest_weighted_second_tier) {
            if (highest_second_tier_weight > highest_weight and
                highest_weight < 9.0_atp) {
                // If a second-tier room has a weight significantly higher than
                // the upper-layer's visible room, use the second tier room
                // instead. Relevant in cases where the player has covered the
                // top layer of his castle with hull blocks.
                target = highest_weighted_second_tier;
            }
        }

        if (APP.game_mode() not_eq App::GameMode::tutorial and
            visible_rooms.size() > 1 and
            rng::choice<4>(rng::utility_state) == 0) {
            assign_weapon_target(silo,
                                 visible_rooms[rng::choice(visible_rooms.size(),
                                                           rng::utility_state)]
                                     ->position(),
                                 ai_island);
        } else {
            assign_weapon_target(silo, target->position(), ai_island);
        }
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         BeamGun& beam_gun,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    struct Target
    {
        RoomCoord position_;
        ATP weight_;
    };

    using TargetBuffer = Buffer<Target, Island::Rooms::Rooms::capacity()>;

    auto buffer = allocate_dynamic<TargetBuffer>("beam-targets");

    for (auto& room : target_island->rooms()) {
        auto pos = room->position();
        auto weight = room->get_atp();
        if ((*room->metaclass())->properties() & RoomProperties::habitable) {
            for (auto& chr : room->characters()) {
                (void)chr;
                weight -= 250.0_atp;
            }
        }
        buffer->push_back(Target{pos, weight});
    }

    std::sort(buffer->begin(), buffer->end(), [](auto& lhs, auto& rhs) {
        return lhs.weight_ > rhs.weight_;
    });

    if (not buffer->empty()) {
        assign_weapon_target(beam_gun, (*buffer)[0].position_, ai_island);
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         Ballista& ballista,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    struct RoomInfo
    {
        Room* room_;
        int x_;
        int y_;
    };
    Buffer<RoomInfo, 32> visible_rooms;

    if (ai_island == APP.opponent_island()) {
        for (u8 x = (u8)target_island->terrain().size() - 1; x > 0; --x) {
            u8 y =
                std::max(target_island->min_y() - 1, construction_zone_min_y);
            u8 xx = x;
            while (y < 14 and xx > 0) {
                if (auto r = target_island->get_room({xx, y})) {
                    visible_rooms.push_back({r, xx, y});
                    break;
                }
                --xx;
                ++y;
            }
        }
    } else {
        for (u8 x = 0; x < (u8)target_island->terrain().size(); ++x) {
            u8 y =
                std::max(target_island->min_y() - 1, construction_zone_min_y);
            u8 xx = x;
            while (y < 14 and xx < (u8)target_island->terrain().size()) {
                if (auto r = target_island->get_room({xx, y})) {
                    visible_rooms.push_back({r, xx, y});
                    break;
                }
                ++xx;
                ++y;
            }
        }
    }

    for (u8 y = 0; y < 16; ++y) {
        if (ai_island == APP.opponent_island()) {
            for (int x = target_island->terrain().size(); x > -1; --x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back({room, (u8)x, y});
                    }
                    break;
                }
            }
        } else {
            for (int x = 0; x < (int)target_island->terrain().size(); ++x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back({room, (u8)x, y});
                    }
                    break;
                }
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.0_atp;

    for (auto room_info : visible_rooms) {
        auto w = room_info.room_->get_atp();

        if (w > highest_weight) {
            highest_weighted_room = room_info.room_;
            highest_weight = w;
        }
    }

    if (highest_weighted_room) {
        assign_weapon_target(
            ballista, highest_weighted_room->position(), ai_island);
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         FlakGun& flak_gun,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    struct RoomInfo
    {
        Room* room_;
        int x_;
        int y_;
    };
    Buffer<RoomInfo, 32> visible_rooms;

    for (u8 y = 0; y < 16; ++y) {
        if (ai_island == APP.opponent_island()) {
            for (int x = target_island->terrain().size(); x > -1; --x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back({room, x, y});
                    }
                    break;
                }
            }
        } else {
            for (u32 x = 0; x < target_island->terrain().size(); ++x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back({room, (int)x, y});
                    }
                    break;
                }
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.00003_atp;

    for (auto room_info : visible_rooms) {
        auto w = room_info.room_->get_atp();

        auto neighbor_weight = [&](int x_offset, int y_offset) {
            const auto x = room_info.x_ + x_offset;
            const auto y = room_info.y_ + y_offset;

            if (x >= 0 and x < 15 and y >= 0 and y < 15) {
                if (auto room = (*target_island).get_room({u8(x), u8(y)})) {
                    return room->get_atp();
                }
            }

            return 0.0_atp;
        };

        // Yeah, this is pretty bad. Project due in a few hours, though!
        w += 0.5_atp * neighbor_weight(1, 0);
        w += 0.5_atp * neighbor_weight(-1, 0);
        w += 0.5_atp * neighbor_weight(0, 1);
        w += 0.5_atp * neighbor_weight(0, -1);

        w += 0.3_atp * neighbor_weight(2, 0);
        w += 0.3_atp * neighbor_weight(1, -1);
        w += 0.3_atp * neighbor_weight(0, -2);
        w += 0.3_atp * neighbor_weight(-1, -1);
        w += 0.3_atp * neighbor_weight(-2, 0);
        w += 0.3_atp * neighbor_weight(-1, 1);
        w += 0.3_atp * neighbor_weight(0, 2);
        w += 0.3_atp * neighbor_weight(1, 1);


        if (w > highest_weight) {
            highest_weighted_room = room_info.room_;
            highest_weight = w;
        }
    }

    if (APP.game_mode() not_eq App::GameMode::tutorial and
        visible_rooms.size() > 1 and rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1].room_;
    }

    if (highest_weighted_room) {
        assign_weapon_target(
            flak_gun, highest_weighted_room->position(), ai_island);
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         FireCharge& fire_charge,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    Buffer<Room*, 32> visible_rooms;

    for (u8 y = 0; y < 16; ++y) {
        if (ai_island == APP.opponent_island()) {
            for (int x = 15; x > -1; --x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back(room);
                    }
                    break;
                }
            }
        } else {
            for (u32 x = 0; x < target_island->terrain().size(); ++x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back(room);
                    }
                    break;
                }
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.00003_atp;

    for (auto room : visible_rooms) {
        auto w = room->get_atp();

        u8 x = room->position().x;
        u8 y = room->position().y;

        auto check_neighbor = [&](u8 x, u8 y) {
            if ((*target_island).fire_present({x, y})) {
                // This flammable room shouldn't add weight, it's already on
                // fire at this slot! Just let it burn.
                return;
            }
            if (auto room = (*target_island).get_room({x, y})) {
                auto props = (*room->metaclass())->properties();
                if (props & RoomProperties::habitable and
                    room->position().y <= y) {
                    w += 200.0_atp;
                }
                if (props & RoomProperties::highly_flammable) {
                    w += 300.0_atp;
                }
            }
        };

        check_neighbor(x + 1, y);
        check_neighbor(x + 1, y - 1);
        check_neighbor(x + 1, y + 1);

        check_neighbor(x - 1, y);
        check_neighbor(x - 1, y - 1);
        check_neighbor(x - 1, y + 1);
        check_neighbor(x, y - 1);
        check_neighbor(x, y + 1);

        if ((*target_island).fire_present({x, y})) {
            // The room's already on fire and has flammable neighbors that
            // aren't on fire. If we destroy the room before the fire can
            // spread, well then that's no good!
            w -= 700.0_atp;
        }

        if (w > highest_weight) {
            highest_weighted_room = room;
            highest_weight = w;
        }
    }

    if (highest_weighted_room) {
        auto target = highest_weighted_room;
        assign_weapon_target(fire_charge, target->position(), ai_island);
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         ArcGun& arc_gun,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    Buffer<Room*, 32> visible_rooms;
    Buffer<Room*, 32> second_tier;

    for (u8 y = 0; y < 16; ++y) {
        if (ai_island == APP.opponent_island()) {
            for (int x = target_island->terrain().size(); x > -1; --x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back(room);

                        if (x > 0 and matrix.get(x - 1, y)) {
                            if (auto st_room =
                                    (*target_island).get_room({u8(x - 1), y})) {
                                second_tier.push_back(st_room);
                            }
                        }
                    }
                    break;
                }
            }
        } else {
            for (int x = 0; x < (int)target_island->terrain().size(); ++x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back(room);

                        if (x < 15 and matrix.get(x + 1, y)) {
                            if (auto st_room =
                                    (*target_island).get_room({u8(x + 1), y})) {
                                second_tier.push_back(st_room);
                            }
                        }
                    }
                    break;
                }
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.00003_atp;

    for (auto room : visible_rooms) {
        auto w = room->get_atp();

        auto mti = room->metaclass_index();
        auto p = room->position();

        if (p.x > 0) {
            if (auto left = target_island->get_room({(u8)(p.x - 1), p.y})) {
                // NOTE: check left not_eq room because many blocks span
                // multiple coordinates.
                if (left not_eq room and left->metaclass_index() == mti) {
                    w += 200.0_atp;
                }
            }
        }

        if (p.x < 15) {
            if (auto right = target_island->get_room({(u8)(p.x + 1), p.y})) {
                if (right not_eq room and right->metaclass_index() == mti) {
                    w += 200.0_atp;
                }
            }
        }

        if (p.y > 0) {
            if (auto up = target_island->get_room({p.x, (u8)(p.y - 1)})) {
                if (up not_eq room and up->metaclass_index() == mti) {
                    w += 200.0_atp;
                }
            }
        }

        if (p.y < 15) {
            if (auto down = target_island->get_room({p.x, (u8)(p.y + 1)})) {
                if (down not_eq room and down->metaclass_index() == mti) {
                    w += 200.0_atp;
                }
            }
        }

        if (w > highest_weight) {
            highest_weighted_room = room;
            highest_weight = w;
        }
    }

    // Potentially attack the second highest weighted visible room, just to keep
    // things interesting.
    if (APP.game_mode() not_eq App::GameMode::tutorial and
        visible_rooms.size() > 1 and rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1];
    }

    if (highest_weighted_room) {
        auto target = highest_weighted_room;

        assign_weapon_target(arc_gun, target->position(), ai_island);
    }
}



void EnemyAI::set_target(const Bitmatrix<16, 16>& matrix,
                         Room& generic_gun,
                         Player* owner,
                         Island* ai_island,
                         Island* target_island)
{
    Buffer<Room*, 32> visible_rooms;
    Buffer<Room*, 32> second_tier;

    for (u8 y = 0; y < 16; ++y) {
        if (ai_island == APP.opponent_island()) {
            for (int x = target_island->terrain().size(); x > -1; --x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back(room);

                        if (x > 0 and matrix.get(x - 1, y)) {
                            if (auto st_room =
                                    (*target_island).get_room({u8(x - 1), y})) {
                                second_tier.push_back(st_room);
                            }
                        }
                    }
                    break;
                }
            }
        } else {
            for (int x = 0; x < (int)target_island->terrain().size(); ++x) {
                if (matrix.get(x, y)) {
                    if (auto room = (*target_island).get_room({u8(x), y})) {
                        if (room->is_decoration() and
                            not room->cast<Masonry>()) {
                            // Ignore decoration blocks
                            continue;
                        }
                        visible_rooms.push_back(room);

                        if (x < 15 and matrix.get(x + 1, y)) {
                            if (auto st_room =
                                    (*target_island).get_room({u8(x + 1), y})) {
                                second_tier.push_back(st_room);
                            }
                        }
                    }
                    break;
                }
            }
        }
    }

    Room* highest_weighted_room = nullptr;
    ATP highest_weight = 0.00003_atp;

    Room* highest_weighted_second_tier_room = nullptr;
    ATP highest_second_tier_weight = 0.00003_atp;

    for (auto& room : second_tier) {
        auto w = room->get_atp();

        if (w > highest_second_tier_weight) {
            highest_weighted_second_tier_room = room;
            highest_second_tier_weight = w;
        }
    }

    for (auto room : visible_rooms) {
        auto w = room->get_atp();

        if (w > highest_weight) {
            highest_weighted_room = room;
            highest_weight = w;
        }
    }

    // Potentially attack the second highest weighted visible room, just to keep
    // things interesting.
    if (APP.game_mode() not_eq App::GameMode::tutorial and
        visible_rooms.size() > 1 and rng::choice<3>(rng::utility_state) == 0) {
        highest_weighted_room = visible_rooms[1];
    }

    if (highest_weighted_room) {
        auto target = highest_weighted_room;
        if (highest_weighted_second_tier_room and highest_weight < 9.0_atp and
            highest_second_tier_weight > highest_weight) {
            target = highest_weighted_second_tier_room;
        }

        assign_weapon_target(generic_gun, target->position(), ai_island);
    }
}



void EnemyAI::on_room_damaged(Room& room)
{
    Opponent::on_room_damaged(room);
}



} // namespace skyland
