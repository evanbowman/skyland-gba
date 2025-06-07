////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "procgenEnemyAI.hpp"
#include "allocator.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/network.hpp"
#include "skyland/roomTable.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/chaosCore.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/masonry.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static SHARED_VARIABLE(sf_p1_coin_yield);
static SHARED_VARIABLE(sf_p2_coin_yield);
static SHARED_VARIABLE(sf_p3_coin_yield);
static SHARED_VARIABLE(sf_p4_coin_yield);
static SHARED_VARIABLE(chaos_core_placement_chance);



void prep_level();



ProcgenEnemyAI::ProcgenEnemyAI(rng::LinearGenerator seed, u8 difficulty)
    : rng_source_(seed), difficulty_(difficulty)
{
}



void ProcgenEnemyAI::update(Time delta)
{
    if (not APP.opponent_island()) {
        generate_level();

        if (APP.game_mode() == App::GameMode::skyland_forever) {
            for (auto it = APP.birds().begin(); it not_eq APP.birds().end();) {
                if ((*it)->island() == opponent_island()) {
                    it = APP.birds().erase(it);
                } else {
                    ++it;
                }
            }
            GenericBird::spawn(*APP.opponent_island(),
                               rng::choice<4>(rng::utility_state));
        }

        if (APP.game_mode() == App::GameMode::co_op) {

            const int prep_seconds = [&] {
                if (levelgen_enemy_count_ < 2) {
                    return 30;
                } else if (levelgen_enemy_count_ < 3) {
                    return 20;
                } else {
                    return 3;
                }
            }();


            auto& g = globals();

            g.multiplayer_prep_seconds_ = prep_seconds;
            g.multiplayer_pauses_remaining_ = 3;
            g.multiplayer_pause_owner_ = false;

            if (state_bit_load(StateBit::multiboot)) {
                // The multiboot peer doesn't run the same game logic, it's just
                // an empty shell that recieves all updates from the server host
                // (this code). We need to tell the multiboot instance what the
                // opponent's island looks like.

                int counter = 0;
                for (auto& room : APP.opponent_island()->rooms()) {
                    if (++counter == 16) {
                        counter = 0;
                        // Wait for some queued transmits to finish
                        PLATFORM.sleep(4);
                    }

                    network::packet::OpponentRoomCreated p;
                    p.x_ = room->position().x;
                    p.y_ = room->position().y;
                    p.metaclass_index_.set(room->metaclass_index());
                    network::transmit(p);
                }
            }
        }
    } else {
        auto mt_prep_seconds = globals().multiplayer_prep_seconds_;

        if (APP.game_mode() == App::GameMode::co_op) {

            if (mt_prep_seconds == 0 and PLATFORM.network_peer().is_host()) {
                EnemyAI::update(delta);
            }
        } else {
            EnemyAI::update(delta);
        }
    }

    if (not APP.opponent_island() or APP.player_island().is_destroyed()) {
        level_text_.reset();
    } else if (not level_text_) {
        level_text_.emplace(
            OverlayCoord{u8(calc_screen_tiles().x -
                            integer_text_length(levelgen_enemy_count_)),
                         0});
    } else {
        level_text_->assign(levelgen_enemy_count_);
    }
}



void ProcgenEnemyAI::generate_level()
{
    for (auto& room : APP.player_island().rooms()) {
        // TODO: gather information about the layout of the player's castle.
        (void)room;
    }

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    PLATFORM.set_scroll(Layer::map_1_ext, -250, -374);
    PLATFORM.screen().clear();

    auto area = [&] {
        if (levelgen_enemy_count_ < 3) {
            return 20;
        } else if (levelgen_enemy_count_ < 5) {
            return 30;
        } else if (levelgen_enemy_count_ < 8) {
            return 40;
        } else if (levelgen_enemy_count_ < 12) {
            return 50;
        } else {
            return 70;
        }
    }();

    levelgen_size_.y = 4 + rng::choice<5>(rng_source_);

    if (levelgen_enemy_count_ < 4) {
        levelgen_size_.y = std::min(u8(5), levelgen_size_.y);
    }

    levelgen_size_.x = std::min(13, std::max(3, area / levelgen_size_.y));


    APP.create_opponent_island(levelgen_size_.x);
    APP.opponent_island()->show_flag(true);

    generate_power_sources();

    if (levelgen_enemy_count_ > 2 or rng::choice<2>(rng_source_)) {
        generate_stairwells();
    }

    generate_secondary_rooms();
    generate_characters();
    generate_radiators();

    generate_hull();

    int weapon_limit = 0;
    if (levelgen_enemy_count_ < 1) {
        weapon_limit = 1;
    } else if (levelgen_enemy_count_ < 2) {
        weapon_limit = 2;
    } else if (core_count_ < 2) {
        weapon_limit = 3;
    } else if (core_count_ < 3) {
        weapon_limit = 5;
    } else if (core_count_ < 4) {
        weapon_limit = 7;
    } else {
        weapon_limit = 11;
    }
    generate_weapons(weapon_limit);

    if (core_count_ >= 2) {
        generate_forcefields();
        generate_missile_defenses();
    }

    generate_foundation();

    cleanup_unused_terrain();

    generate_walls_behind_weapons();

    generate_decorations();

    generate_foundation();


    prep_level();
    show_island_exterior(APP.opponent_island());

    // The level generation may have taken a long time. Shouldn't have any
    // bearing on gameplay.
    PLATFORM.delta_clock().reset();

    ++levelgen_enemy_count_;

    if (APP.game_mode() == App::GameMode::skyland_forever or
        APP.game_mode() == App::GameMode::co_op) {
        for (auto& room : APP.opponent_island()->rooms()) {

            auto frac = 0.4f;

            if (levelgen_enemy_count_ > 42) {
                frac = 0.01f;
            }
            if (levelgen_enemy_count_ > 36) {
                frac = (sf_p4_coin_yield * 0.01) / 8;
            } else if (levelgen_enemy_count_ > 28) {
                frac = (sf_p4_coin_yield * 0.01) / 2;
            } else if (levelgen_enemy_count_ > 20) {
                frac = sf_p4_coin_yield * 0.01;
            } else if (levelgen_enemy_count_ > 16) {
                frac = sf_p3_coin_yield * 0.01;
            } else if (levelgen_enemy_count_ > 12) {
                frac = sf_p2_coin_yield * 0.01;
            } else if (levelgen_enemy_count_ > 7) {
                frac = sf_p1_coin_yield * 0.01;
            }

            // NOTE: we award coins based on opponent island value. But we
            // generate mirror-hull to discourage certain playing styles, so do
            // not award extra value to players when generating mirror-hull.
            Coins cost = 0;
            if (str_eq(room->name(), "mirror-hull")) {
                cost = 200;
            } else {
                cost = (*room->metaclass())->cost();
            }

            if (APP.game_mode() == App::GameMode::co_op) {
                // For co-op, our score calculation differs slightly. Give each
                // player half of the resulting coins.
                APP.victory_coins() += (frac * cost * 0.5f);
            } else {
                APP.victory_coins() += frac * cost;
            }
        }
    }

    if (APP.game_mode() == App::GameMode::co_op) {
        // Give the player a just a bit more coins in co-op mode, as the coin
        // count is shared with the other player.
        APP.victory_coins() += 500;
    }

    if (APP.game_mode() not_eq App::GameMode::co_op) {
        APP.time_stream().enable_pushes(true);
        APP.time_stream().clear();
    }

    level_text_.emplace(OverlayCoord{
        u8(calc_screen_tiles().x - integer_text_length(levelgen_enemy_count_)),
        0});
}



void ProcgenEnemyAI::set_levelgen_count(int count)
{
    levelgen_enemy_count_ = count;
}



static const int level_threshold_two_powercores = 4;



void ProcgenEnemyAI::generate_power_sources()
{
    core_count_ = 0;
    int reactor_count = 0;

    if (levelgen_enemy_count_ < level_threshold_two_powercores) {
        core_count_ = 1;
    } else if (levelgen_enemy_count_ < 10) {
        core_count_ = 2;
    } else if (levelgen_enemy_count_ < 14) {
        core_count_ = 3;
    } else {
        core_count_ = 3;
        reactor_count = 1;
    }

    for (int i = 0; i < core_count_; ++i) {
        place_room_random_loc(
            levelgen_size_.x < 4 ? 1 : levelgen_size_.x / 2 - 1, "power-core");
    }

    for (int i = 0; i < reactor_count; ++i) {
        const char* core = "reactor";
        if (rng::chance(chaos_core_placement_chance, rng_source_)) {
            bool player_has_chaos_core = false;
            for (auto& room : APP.player_island().rooms()) {
                if (room->cast<ChaosCore>()) {
                    player_has_chaos_core = true;
                }
            }
            if (not player_has_chaos_core) {
                core = "chaos-core";
            }
        }
        place_room_random_loc(
            levelgen_size_.x < 4 ? 1 : levelgen_size_.x / 2 - 1, core);
    }

    core_count_ += reactor_count;
}



Power ProcgenEnemyAI::power_remaining() const
{
    return APP.opponent_island()->power_supply() -
           APP.opponent_island()->power_drain();
}



void ProcgenEnemyAI::generate_weapons(int max)
{
    const bool co_op = APP.game_mode() == App::GameMode::co_op;

    int missile_count = 0;
    int drone_count = 0;
    int cannon_count = 0;
    int flak_count = 0;
    int nemesis_count = 0;
    int arc_count = 0;
    int forcefield_count = 0;
    int misc_cannon_count = 0;
    [[maybe_unused]] int ion_cannon_count = 0;
    int mycelium_count = 0;

    for (auto& room : APP.player_island().rooms()) {
        const auto category = (*room->metaclass())->category();

        if (str_eq(room->name(), "splitter")) {
            missile_count += 2;
        } else if (str_eq(room->name(), "missile-silo") or
                   str_eq(room->name(), "rocket-bomb")) {
            ++missile_count;
        } else if (str_eq(room->name(), "drone-bay")) {
            ++drone_count;
        } else if (str_eq(room->name(), "cannon")) {
            ++cannon_count;
        } else if (str_eq(room->name(), "forcefield") or
                   str_eq(room->name(), "forcefield*")) {
            ++forcefield_count;
        } else if (str_eq(room->name(), "flak-gun")) {
            ++flak_count;
        } else if (str_eq(room->name(), "nemesis")) {
            ++nemesis_count;
        } else if (str_eq(room->name(), "arc-gun")) {
            ++arc_count;
        } else if (str_eq(room->name(), "spark-cannon")) {
            ++arc_count;
        } else if (str_eq(room->name(), "ion-cannon")) {
            ++ion_cannon_count;
        } else if (str_eq(room->name(), "mycelium")) {
            ++mycelium_count;
        } else if (category == Room::Category::weapon) {
            ++misc_cannon_count;
        }
    }


    int generic_cannon_count =
        cannon_count + flak_count + arc_count + misc_cannon_count;

    [[maybe_unused]] int player_avg_roof_hull_thickness = 0;

    for (u8 x = 0; x < (int)APP.player_island().terrain().size(); ++x) {
        for (u8 y = construction_zone_min_y; y < 15; ++y) {
            if (auto room = APP.player_island().get_room({x, y})) {
                if ((*room->metaclass())->category() == Room::Category::wall) {
                    ++player_avg_roof_hull_thickness;
                } else {
                    break;
                }
            }
        }
    }

    if (APP.player_island().terrain().size() not_eq 0) {
        player_avg_roof_hull_thickness /= APP.player_island().terrain().size();
    }

    int player_avg_forward_hull_thickness = 0;
    int min_present_y = 15;

    for (u8 y = construction_zone_min_y; y < 15; ++y) {
        for (u8 x = (int)APP.player_island().terrain().size() - 1; x > 1; --x) {
            if (auto room = APP.player_island().get_room({x, y})) {
                if ((*room->metaclass())->category() == Room::Category::wall) {
                    min_present_y = std::min(min_present_y, (int)y);
                    ++player_avg_forward_hull_thickness;
                } else {
                    break;
                }
            }
        }
    }

    const int divisor = 15 - min_present_y;
    if (divisor) {
        player_avg_forward_hull_thickness /= divisor;
    }

    struct Context
    {
        Buffer<RoomMeta*, 200> distribution_;

        struct Pair
        {
            const char* weapon_name_;
            Float probability_;
        };

        bool invalidated_missile_cells_[16][16];
        bool invalidated_cannon_cells_[16][16];

        Buffer<Pair, 30> pairs_;
    };

    auto c = allocate_dynamic<Context>("procgen-buffer");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            c->invalidated_cannon_cells_[x][y] = false;
            c->invalidated_missile_cells_[x][y] = false;
        }
    }

    auto enq_prob = [&](const char* mt_name, Float prob) {
        c->pairs_.push_back({mt_name, prob});
    };

    if (levelgen_enemy_count_ > 4) {
        Float flak_prob = 100.f;
        flak_prob += (player_avg_forward_hull_thickness < 2 ? 25.f : -25.f);
        if (nemesis_count) {
            flak_prob += 100.f;
        }
        enq_prob("flak-gun", flak_prob);
    }

    if (forcefield_count) {
        enq_prob("ion-cannon", 40.f + forcefield_count * 20.f);
    }

    if (levelgen_enemy_count_ > 9 and difficulty_ > 0 and
        // TODO: send network events in EnemyAI for creating/destroying
        // drones. Until then, we need to disable drone-bays in co-op mode.
        APP.game_mode() not_eq App::GameMode::co_op) {

        if (missile_count < 5) {
            enq_prob("drone-bay",
                     (50.f - missile_count * 10.f) + drone_count * 20.f);
        }
    }

    // Fire spread is really super difficult to synchronize over a low bandwidth
    // GBA link cable. Disabled until I can get it to work reliably.
    const char* fire_charge_substitution =
        PLATFORM.network_peer().is_connected() ? "nemesis" : "fire-charge";

    if (levelgen_enemy_count_ > 8) {
        if (missile_count > 2) {
            enq_prob("cannon", 50.f);
            enq_prob("arc-gun", 220.f);
            enq_prob("nemesis", 30.f);
            enq_prob(fire_charge_substitution, 30.f);
        } else {
            enq_prob("cannon", 100.f);
            if (mycelium_count) {
                enq_prob("arc-gun", 200.f);
                enq_prob(fire_charge_substitution, 90.f);
                enq_prob("nemesis", 30.f);
            } else {
                enq_prob("arc-gun", 120.f);
                enq_prob(fire_charge_substitution, 40.f);
                enq_prob("nemesis", 80.f);
            }
        }

        enq_prob("missile-silo",
                 240.f + 10.f * missile_count + 10 * generic_cannon_count +
                     50.f * drone_count);
        enq_prob(co_op ? "missile-silo" : "rocket-bomb", 60.f);
    } else {
        enq_prob("cannon", 100.f);

        enq_prob("missile-silo",
                 90.f + 10.f * missile_count + 10 * generic_cannon_count +
                     50.f * drone_count);
    }

    Float total_prob = 0.f;
    for (auto& p : c->pairs_) {
        total_prob += p.probability_;
    }

    if (total_prob == 0.f) {
        Platform::fatal("weapon gen failed!");
    }

    for (auto& p : c->pairs_) {
        p.probability_ /= total_prob;
        auto& mt = require_metaclass(p.weapon_name_);
        for (int i = 0; i < p.probability_ * c->distribution_.capacity(); ++i) {
            c->distribution_.push_back(&mt);
        }
    }

    auto place_missile_silo = [&](RoomMeta* mt) {
        Buffer<RoomCoord, 16> slots;
        const u8 ht = (*mt)->size().y;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = construction_zone_min_y; y < 14; ++y) {
                auto room = APP.opponent_island()->get_room({x, u8(y + ht)});
                if ((y == 13 or room) and has_space({x, y}, {1, ht}) and
                    not c->invalidated_missile_cells_[x][y] and
                    not c->invalidated_missile_cells_[x][y + 1]) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng_source_);

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(APP.opponent_island(), {s.x, s.y});

            for (int yy = 0; yy < s.y; ++yy) {
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_cannon_cells_[s.x][yy] = true;
            }
        }
    };

    auto place_cannon = [&](RoomMeta* mt) {
        Buffer<RoomCoord, 16> slots;


        for (u8 y = construction_zone_min_y; y < 14; ++y) {
            for (u8 x = 0; x < 15; ++x) {

                auto room = APP.opponent_island()->get_room(
                    {u8(x + (*mt)->size().x), y});

                bool invalid = false;
                for (int xx = x; xx < x + (*mt)->size().x; ++xx) {
                    if (c->invalidated_cannon_cells_[xx][y]) {
                        invalid = true;
                    }
                }

                if ((room or
                     APP.opponent_island()->get_room({x, u8(y + 1)})) and
                    has_space({x, y}, (*mt)->size()) and not invalid) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng_source_);

        // Placing weapons nearer to the leftmost position reduces the chances
        // of placing cannons in the path of other cannons.
        std::sort(slots.begin(), slots.end(), [](auto& lhs, auto& rhs) {
            return lhs.x < rhs.x;
        });

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(APP.opponent_island(), {s.x, s.y});

            for (int xx = 0; xx < 15; ++xx) {
                c->invalidated_cannon_cells_[xx][s.y] = true;
            }

            // Only invalidate the slots in front of the cannon, i.e. it's fine
            // to place missile-silos behind a cannon-type weapon.
            for (int xx = 0; xx < s.x; ++xx) {
                c->invalidated_missile_cells_[xx][s.y] = true;
            }
        }
    };

    auto place_drone_bay = [&](RoomMeta* mt) {
        // We mostly treat a drone bay as if it were simply a differently-shaped
        // missile-silo.

        Buffer<RoomCoord, 16> slots;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = construction_zone_min_y; y < 14; ++y) {
                auto room = APP.opponent_island()->get_room({x, u8(y + 1)});
                if ((y == 13 or room) and has_space({x, y}, {2, 1}) and
                    not c->invalidated_missile_cells_[x][y] and
                    not c->invalidated_missile_cells_[x + 1][y]) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng_source_);

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(APP.opponent_island(), {s.x, s.y});

            c->invalidated_cannon_cells_[s.x][s.y - 1] = true;
            c->invalidated_cannon_cells_[s.x + 1][s.y - 1] = true;

            for (int yy = 0; yy < s.y; ++yy) {
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_missile_cells_[s.x + 1][yy] = true;
            }

            for (int yy = s.y; yy < 15; ++yy) {
                // Invalidate missile-silos beneath the drone bay
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_missile_cells_[s.x + 1][yy] = true;
            }
        }
    };

    [[maybe_unused]] int place_missile_count = 0;
    int placed_ion_cannons = 0;
    int fails = 0;

    Buffer<u16, 20> missile_silo_mts;

    for (int i = 0; i < max; ++i) {
    RETRY:
        auto ind = rng::choice(c->distribution_.size(), rng_source_);
        auto sel = c->distribution_[ind];

        if (fails > 40) {
            sel = load_metaclass("cannon");
            fails = 0;
        }

        if (power_remaining() > (*sel)->consumes_power()) {

            if (str_eq((*sel)->name(), "ion-cannon") and
                placed_ion_cannons > 1) {
                ++fails;
                goto RETRY;
            }

            if (str_eq((*sel)->name(), "ion-cannon")) {
                ++placed_ion_cannons;
            }

            if (str_eq((*sel)->name(), "missile-silo") or
                str_eq((*sel)->name(), "rocket-bomb")) {
                ++place_missile_count;
                missile_silo_mts.push_back(ind);
            } else if (str_eq((*sel)->name(), "drone-bay")) {
                place_drone_bay(sel);
            } else {
                place_cannon(sel);
            }

        } else {
            break;
        }
    }

    for (auto ind : missile_silo_mts) {
        auto sel = c->distribution_[ind];
        place_missile_silo(sel);
    }
}



void ProcgenEnemyAI::generate_missile_defenses()
{
    if (rng::choice<4>(rng_source_) == 0) {
        // Skip it sometimes.
        return;
    }

    int missile_count = 0;
    int lateral_count = 0;
    int clump_count = 0;
    int ballista_count = 0;

    auto& shull = require_metaclass("stacked-hull");

    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "splitter")) {
            clump_count += 1;
            missile_count += 3;
        } else if (str_eq(room->name(), "missile-silo") or
                   str_eq(room->name(), "rocket-bomb")) {
            ++missile_count;
        } else if ((*room->metaclass())->category() == Room::Category::weapon) {
            ++lateral_count;
        } else if (str_eq(room->name(), "ballista")) {
            ++ballista_count;
        }
    }

    // If the player has a missile heavy build, create extra defenses.
    if ((ballista_count > 1 and rng::choice<3>(rng_source_)) or
        (clump_count > 0 and rng::choice<2>(rng_source_)) or
        (lateral_count == 0 and missile_count > 0 and
         rng::choice<3>(rng_source_) == 0) or
        (missile_count > lateral_count and missile_count - lateral_count > 3)) {

        bool invalid_rows[16];
        memset(invalid_rows, false, sizeof invalid_rows);
        for (int y = 0; y < construction_zone_min_y; ++y) {
            invalid_rows[y] = true;
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                invalid_rows[room->position().y] = true;
            }
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon and
                not str_eq(room->name(), "missile-silo") and
                not str_eq(room->name(), "rocket-bomb")) {
                auto p = room->position();
                p.y--;
                if (invalid_rows[p.y]) {
                    continue;
                }
                for (int x = 0; x < room->size().x; ++x) {
                    if (not APP.opponent_island()->get_room(p)) {
                        shull->create(APP.opponent_island(), p);
                    }
                    ++p.x;
                }
            }
        }
    }
}



void shift_rooms_right(Island& island);



void ProcgenEnemyAI::generate_forcefields()
{
    struct Context
    {
        struct Slot
        {
            RoomCoord coord_;
            Float weight_;
        };

        Buffer<Slot, 60> slots_;
    };

    auto c = allocate_dynamic<Context>("procgen-buffer");

    int free_count = 15 - construction_zone_min_y;
    for (int i = 0; i < 15; ++i) {
        if (APP.opponent_island()->get_room({0, (u8)i})) {
            --free_count;
        }
    }
    auto t_size = APP.opponent_island()->terrain().size();
    if (free_count < 5 and t_size < 13) {
        APP.opponent_island()->init_terrain(++t_size, false);
        shift_rooms_right(*APP.opponent_island());
        APP.opponent_island()->repaint();
    }


    auto find_ideal_forcefield_locs = [&] {
        c->slots_.clear();

        int player_missile_count = 0;
        int player_cannon_count = 0;
        [[maybe_unused]] int player_ion_cannon_count = 0;
        int player_annihilator_count = 0;
        for (auto& room : APP.player_island().rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                if (str_eq(room->name(), "annihilator")) {
                    ++player_annihilator_count;
                } else if (str_eq(room->name(), "missile-silo") or
                           str_eq(room->name(), "rocket-bomb")) {
                    ++player_missile_count;
                } else if (str_eq(room->name(), "ion-cannon")) {
                    ++player_ion_cannon_count;
                } else {
                    ++player_cannon_count;
                }
            }
        }

        int opponent_missile_count = 0;
        int opponent_cannon_count = 0;
        for (auto& room : APP.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                if (str_eq(room->name(), "missile-silo") or
                    str_eq(room->name(), "rocket-bomb")) {
                    ++opponent_missile_count;
                } else {
                    ++opponent_cannon_count;
                }
            }
        }

        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = construction_zone_min_y; y < 15; ++y) {
                Float weight = 0.f;

                auto get_room = [&](u8 x, u8 y) {
                    return APP.opponent_island()->get_room({x, y});
                };

                if (x < 15) {
                    if (APP.opponent_island()->rooms_plot().get(x + 1, y)) {
                        if (auto room = get_room(x + 1, y)) {
                            if ((not str_eq(room->name(), "missile-silo") and
                                 not str_eq(room->name(), "rocket-bomb") and
                                 not str_eq(room->name(), "ion-cannon")) and
                                (*room->metaclass())->category() ==
                                    Room::Category::weapon) {

                                if (opponent_cannon_count >
                                    player_cannon_count) {
                                    weight += 150.f;
                                } else {
                                    weight += 100.f;
                                }
                            }
                        }
                    }
                }

                if (y < 15) {
                    if (APP.opponent_island()->rooms_plot().get(x, y + 1)) {
                        if (auto room = get_room(x, y + 1)) {
                            if (str_eq(room->name(), "missile-silo") or
                                str_eq(room->name(), "rocket-bomb") or
                                str_eq(room->name(), "drone-bay")) {
                                if (opponent_missile_count >
                                        player_missile_count or
                                    player_annihilator_count) {
                                    weight += 150.f;
                                } else {
                                    weight += 100.f;
                                }
                            } else if (str_eq(room->name(), "drone-bay")) {
                                if (player_missile_count or
                                    player_annihilator_count) {
                                    weight += 110.f;
                                }
                            }
                        }
                    }
                }

                if (weight not_eq 0.f) {
                    c->slots_.push_back({{x, y}, weight});
                }
            }
        }

        rng::shuffle(c->slots_, rng_source_);

        std::sort(c->slots_.begin(), c->slots_.end(), [](auto& lhs, auto& rhs) {
            return lhs.weight_ > rhs.weight_;
        });
    };


    auto& mt = require_metaclass("forcefield");
    auto& mt2 = require_metaclass("forcefield*");

    while (true) {
        const auto power = power_remaining();

        RoomMeta* sel = &mt;

        if (core_count_ > 2 and rng::choice<2>(rng_source_) and
            power > mt2->consumes_power()) {
            sel = &mt2;
        }

        if (power < (*sel)->consumes_power()) {
            return;
        }

        find_ideal_forcefield_locs();

        if (not c->slots_.empty()) {
            bool placed = false;
            for (auto& slot : c->slots_) {
                if (has_space(slot.coord_, {1, 1})) {
                    placed = true;
                    (*sel)->create(APP.opponent_island(), slot.coord_);
                    break;
                }
            }
            if (not placed) {
                return;
            }
        } else {
            return;
        }
    }
}



void ProcgenEnemyAI::generate_hull()
{
    auto& hull = require_metaclass("hull");
    auto& mhull = require_metaclass("mirror-hull");
    auto& shull = require_metaclass("stacked-hull");
    auto& ehull = require_metaclass("energized-hull");
    auto& bhull = require_metaclass("bronze-hull");

    int missile_count = 0;
    int bomb_count = 0;

    int lateral_count = 0;
    int cannon_count = 0;
    int flak_count = 0;
    int arc_count = 0;
    int fire_count = 0;

    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "missile-silo") or
            str_eq(room->name(), "rocket-bomb")) {
            ++missile_count;
        } else if (str_eq(room->name(), "cannon")) {
            ++cannon_count;
            ++lateral_count;
        } else if (str_eq(room->name(), "flak-gun")) {
            ++flak_count;
            ++lateral_count;
        } else if (str_eq(room->name(), "arc-gun") or
                   str_eq(room->name(), "spark-cannon")) {
            ++arc_count;
            ++lateral_count;

            if (str_eq(room->name(), "spark-cannon")) {
                ++arc_count;
            }
        } else if (str_eq(room->name(), "fire-charge")) {
            ++fire_count;
            ++lateral_count;
        }
        if (str_eq(room->name(), "rocket-bomb")) {
            ++bomb_count;
        }
    }

    const int max_identical =
        std::max({cannon_count, flak_count, arc_count, fire_count});

    bool missile_defense = false;

    auto generate_roof = [&](bool conservative, int ehull_count) {
        for (u8 x = 0; x < 15; ++x) {

            for (u8 y = construction_zone_min_y; y < 14; ++y) {

                if (APP.opponent_island()->rooms_plot().get(x, y + 1)) {
                    auto below =
                        APP.opponent_island()->get_room({x, u8(y + 1)});

                    if (not conservative or
                        (below and (*below->metaclass())->category() not_eq
                                       Room::Category::wall)) {
                        if (not APP.opponent_island()->rooms_plot().get(x, y)) {
                            if (ehull_count > 0 and
                                rng::choice<3>(rng_source_) == 0) {
                                --ehull_count;
                                ehull->create(APP.opponent_island(), {x, y});
                            } else if (missile_defense) {
                                shull->create(APP.opponent_island(), {x, y});
                            } else {
                                hull->create(APP.opponent_island(), {x, y});
                            }
                        }
                    }
                }
            }
        }
    };

    generate_roof(true, 0);

    if (missile_count > 2) {
        int ehull_count = 0;
        switch (core_count_) {
        case 1:
            ehull_count = rng::choice<3>(rng_source_);
            break;

        case 2:
            ehull_count = rng::choice<5>(rng_source_);
            break;

        default:
            ehull_count = rng::choice<6>(rng_source_);
            break;
        }

        if (missile_count > 4 or bomb_count > 2) {
            missile_defense = true;
        } else {
            missile_defense = rng::choice<2>(rng_source_);
        }

        if (not missile_defense and missile_count > 0 and lateral_count == 0) {
            if (levelgen_enemy_count_ > 5) {
                missile_defense = rng::choice<2>(rng_source_);
            }
        }
        generate_roof(false, ehull_count);
    }

    auto make_lateral_hull = [&](bool ignore_if_wall) {
        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = 0; y < 15; ++y) {

                if (APP.opponent_island()->rooms_plot().get(x + 1, y)) {
                    auto right =
                        APP.opponent_island()->get_room({u8(x + 1), y});
                    if (not right) {
                        continue;
                    }
                    if ((*right->metaclass())->category() ==
                            Room::Category::weapon and
                        not str_eq(right->name(), "missile-silo") and
                        not str_eq(right->name(), "rocket-bomb") and
                        not str_eq(right->name(), "ion-cannon")) {
                        continue;
                    }

                    bool place_bhull = false;
                    bool place_mhull = false;
                    if (levelgen_enemy_count_ > 8) {
                        place_mhull = rng::choice<8>(rng_source_) == 0;
                    }

                    if (max_identical > 6 and max_identical == arc_count) {
                        // NOTE: use completely different logic if the player
                        // built lots of arc-guns, as mirror-hull would be weak
                        // to arc-chaining. 1/2 of rooms generated as mirror
                        // hull would be technically the most frustrating thing
                        // for a strategy that heavily favors arc-guns.
                        // But... rather than relying on randomness, let's use
                        // coordinate even/odd parity! That way, every other
                        // hull will be mirrored.
                        place_mhull = x % 2 not_eq y % 2;
                    } else {
                        // If the player just tried to build tons of cannons,
                        // generate some mirror-hull to encourage the player to
                        // attack in a different way.
                        if (max_identical >= 6 and not place_mhull) {
                            place_mhull = true;
                        }
                        if (max_identical >= 6 and not place_mhull) {
                            place_mhull = rng::choice<2>(rng_source_) == 0;
                        } else if (max_identical >= 5 and not place_mhull) {
                            place_mhull = rng::choice<2>(rng_source_) == 0;
                        } else if (max_identical > 3 and not place_mhull) {
                            place_mhull = rng::choice<5>(rng_source_) == 0;
                        }
                    }

                    if (not place_mhull and arc_count > 4) {
                        place_bhull = x % 2 not_eq y % 2;
                    }

                    auto cat = (*right->metaclass())->category();

                    if ((not ignore_if_wall or
                         (ignore_if_wall and
                          cat not_eq Room::Category::wall)) and
                        not APP.opponent_island()->rooms_plot().get(x, y)) {

                        if (place_mhull) {
                            mhull->create(APP.opponent_island(), {x, y});
                        } else if (place_bhull) {
                            bhull->create(APP.opponent_island(), {x, y});
                        } else {
                            hull->create(APP.opponent_island(), {x, y});
                        }
                    }
                }
            }
        }
    };

    make_lateral_hull(true);

    if ((flak_count >= 4 or lateral_count > 5)) {
        // Generate an extra later of forward hull if the player has a lot of
        // cannon-type weapons.

        u32 unused_columns = 0;
        for (unused_columns = 0;
             unused_columns < APP.opponent_island()->terrain().size();
             ++unused_columns) {

            u8 x = unused_columns;

            for (u8 y = 0; y < 15; ++y) {
                if (APP.opponent_island()->get_room({x, y})) {
                    goto DONE;
                }
            }
        }
    DONE:
        // But, only generate an extra layer of hull if there's significant
        // space in front of the generated layout.
        if (unused_columns > 0) {
            make_lateral_hull(false);
        }
    }
}



void ProcgenEnemyAI::generate_walls_behind_weapons()
{
    auto& hull = require_metaclass("masonry");

    for (auto& room : APP.opponent_island()->rooms()) {
        if ((*room->metaclass())->category() == Room::Category::weapon and
            not str_eq(room->name(), "missile-silo") and
            not str_eq(room->name(), "rocket-bomb")) {

            auto behind = room->position();
            behind.x += (*room->metaclass())->size().x;

            if (not APP.opponent_island()->get_room(behind)) {
                bool skip = false;
                for (u8 y = behind.y; y < 15; ++y) {
                    if (auto room =
                            APP.opponent_island()->get_room({behind.x, y})) {
                        if (str_eq(room->name(), "missile-silo") or
                            str_eq(room->name(), "rocket-bomb")) {
                            // Don't generate a hull over top of a missile
                            // silo. The enemy will just shoot itself with
                            // missiles.
                            skip = true;
                        }
                    }
                }

                if (not skip) {
                    hull->create(APP.opponent_island(), behind);
                }
            }
        }
    }
}



void ProcgenEnemyAI::generate_stairwells()
{
    // Generate stairwells

    if (levelgen_size_.x <= 3) {
        return;
    }

    struct StairwellPos
    {
        RoomCoord coord_;
        u8 val_;
    };

    struct Context
    {
        u8 matrix[16][16];
        bool walkable_zones[16][16];
        Buffer<StairwellPos, 30> slots;
    };

    auto c = allocate_dynamic<Context>("procgen-buffer");


    auto mt = load_metaclass("stairwell");

    auto find_ideal_stairwell_slots = [&] {
        c->slots.clear();
        APP.opponent_island()->plot_walkable_zones(c->walkable_zones, nullptr);

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                c->matrix[x][y] = 0;
            }
        }

        for (int x = 1; x < (int)APP.opponent_island()->terrain().size(); ++x) {
            for (int y = 7; y < 15; ++y) {
                if (APP.opponent_island()->rooms_plot().get(x, y)) {
                    c->matrix[x][y] = 0;
                } else {
                    // For a column the size of a stairwell room: how many
                    // walkable tiles would it connect if we placed it starting
                    // at (x,y)?
                    for (int yy = y; yy < std::min(y + 4, 15); ++yy) {
                        if (APP.opponent_island()->rooms_plot().get(x, yy)) {
                            continue;
                        }
                        if (x > 0) {
                            if (c->walkable_zones[x - 1][yy]) {
                                ++c->matrix[x][y];
                            }
                        }
                        if (x < 15) {
                            if (c->walkable_zones[x + 1][yy]) {
                                ++c->matrix[x][y];
                            }
                        }
                        if (yy > 0) {
                            if (c->walkable_zones[x][yy - 1]) {
                                ++c->matrix[x][y];
                            }
                        }
                        if (yy < 15) {
                            if (c->walkable_zones[x][yy + 1]) {
                                ++c->matrix[x][y];
                            }
                        }
                    }
                }
            }
        }
        for (u8 x = 1; x < (int)APP.opponent_island()->terrain().size(); ++x) {
            for (u8 y = 7; y < 15; ++y) {
                if (c->matrix[x][y]) {
                    c->slots.push_back({{x, y}, c->matrix[x][y]});
                }
            }
        }
        std::sort(c->slots.begin(), c->slots.end(), [](auto& lhs, auto& rhs) {
            return lhs.val_ > rhs.val_;
        });
    };

    find_ideal_stairwell_slots();

    int count = 1;
    if (APP.opponent_island()->rooms().size() >= 4) {
        count = 2;
    }

    int tries = 0;
    while (count and tries < 255) {
        for (auto& slot : c->slots) {
            if (count == 0) {
                break;
            }
            if (has_space(slot.coord_, (*mt)->size())) {
                (*mt)->create(APP.opponent_island(), slot.coord_);
                --count;
                tries = 0;
                break;
            }
        }
        find_ideal_stairwell_slots();
        ++tries;
    }
}



void ProcgenEnemyAI::generate_radiators()
{
    int player_character_count = 0;
    int player_transporter_count = 0;
    int player_replicator_count = 0;
    int character_count = 0;

    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "replicator")) {
            ++player_replicator_count;
        } else if (str_eq(room->name(), "transporter")) {
            ++player_transporter_count;
        }
        player_character_count += length(room->characters());
    }

    for (auto& room : APP.opponent_island()->rooms()) {
        character_count += length(room->characters());
    }

    if ((player_replicator_count and player_transporter_count > 2) or
        (player_transporter_count > 2 and
         player_character_count > character_count + 3)) {

        // If the player has a replicator and a transporter, or the player has a
        // transporter and more characters than we do, then we may be dealing
        // with a player who's offensively focused on boarding parties. Generate
        // some radiator blocks to make things more difficult.

        const int count = rng::choice<4>(rng_source_);
        for (int i = 0; i < count; ++i) {
            place_room_adjacent("radiator");
        }
        place_room_adjacent("infirmary");
    }
}



void ProcgenEnemyAI::generate_characters()
{
    int transporter_count = 0;
    for (auto& room : APP.opponent_island()->rooms()) {
        if (str_eq(room->name(), "transporter")) {
            ++transporter_count;
        }
    }

    const int chr_count =
        1 + rng::choice(core_count_, rng_source_) +
        (transporter_count > 0 ? rng::choice(transporter_count, rng_source_)
                               : 0);

    struct Context
    {
        struct Slot
        {
            RoomCoord coord_;
            ATP weight_;
        };

        Buffer<Slot, 30> slots_;

        bool matrix_[16][16];
    };

    auto c = allocate_dynamic<Context>("procgen-buffer");

    for (int i = 0; i < chr_count; ++i) {
        APP.opponent_island()->plot_walkable_zones(c->matrix_, nullptr);

        c->slots_.clear();

        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = 0; y < 15; ++y) {
                if (c->matrix_[x][y]) {
                    if (auto room = APP.opponent_island()->get_room({x, y})) {
                        c->slots_.push_back(
                            {{x, y}, (*room->metaclass())->atp_value()});
                    }
                }
            }
        }

        rng::shuffle(c->slots_, rng_source_);

        std::sort(c->slots_.begin(), c->slots_.end(), [](auto& lhs, auto& rhs) {
            return lhs.weight_ > rhs.weight_;
        });

        if (not c->slots_.empty()) {
            for (auto& slot : c->slots_) {
                if (APP.opponent_island()->character_at_location(slot.coord_)) {
                    continue;
                }
                auto e = alloc_entity<Character>(
                    APP.opponent_island(), &APP.opponent(), slot.coord_, false);
                APP.opponent_island()->add_character(std::move(e));

                break;
            }
        }
    }
}



void ProcgenEnemyAI::generate_decorations()
{
    if (APP.environment().is_cold()) {
        // Don't generate decorative plants and stuff when the weather is snowy.
        return;
    }

    auto& shrubbery_mt = require_metaclass("shrubbery");

    for (u8 x = 0; x < (int)APP.opponent_island()->terrain().size(); ++x) {
        bool empty_column = true;
        u8 y;
        for (y = construction_zone_min_y; y < 14; ++y) {
            if (APP.opponent_island()->rooms_plot().get(x, y)) {
                if (auto room = APP.opponent_island()->get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                            Room::Category::decoration or
                        str_eq(room->name(), "drone-bay")) {
                        empty_column = false;
                        y -= 1;
                        break;
                    }
                    y -= 1;
                    break;
                }
            }
        }


        if (empty_column and y > construction_zone_min_y) {
            switch (rng::choice<5>(rng_source_)) {
            case 0:
            case 1:
            case 2:
                break;

            case 3:
            case 4:
                shrubbery_mt->create(APP.opponent_island(), {x, y});
                break;
            }
        }
    }
}



void ProcgenEnemyAI::place_room_adjacent(const char* room_name)
{
    struct Slot
    {
        RoomCoord coord_;
        u8 val_;
    };

    struct Context
    {
        u8 matrix[16][16];
        bool walkable_zones[16][16];
        Buffer<Slot, 50> slots;
    };

    auto c = allocate_dynamic<Context>("procgen-buffer");

    auto find_connected_slots = [&](int room_height) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                c->matrix[x][y] = 0;
            }
        }

        c->slots.clear();
        APP.opponent_island()->plot_walkable_zones(c->walkable_zones, nullptr);

        for (int x = 0; x < (int)APP.opponent_island()->terrain().size(); ++x) {
            for (int y = 7; y < 15; ++y) {
                if (APP.opponent_island()->rooms_plot().get(x, y)) {
                    c->matrix[x][y] = 0;
                } else {
                    // For a column the size of a stairwell room: how many
                    // walkable tiles would it connect if we placed it starting
                    // at (x,y)?

                    const int yy = y + (room_height - 1);

                    if (x > 0) {
                        if (c->walkable_zones[x - 1][yy]) {
                            ++c->matrix[x][y];
                        }
                    }
                    if (x < 15) {
                        if (c->walkable_zones[x + 1][yy]) {
                            ++c->matrix[x][y];
                        }
                    }
                    if (yy > 0) {
                        if (c->walkable_zones[x][yy - 1]) {
                            ++c->matrix[x][y];
                        }
                    }
                    if (yy < 15) {
                        if (c->walkable_zones[x][yy + 1]) {
                            ++c->matrix[x][y];
                        }
                    }
                }
            }
        }

        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (c->matrix[x][y]) {
                    c->slots.push_back({{x, y}, 0});
                }
            }
        }

        rng::shuffle(c->slots, rng_source_);
    };

    auto try_place_room = [&](RoomMeta& m) {
        int tries = 0;
        while (tries < 255) {
            for (auto& slot : c->slots) {
                if (slot.coord_.x == 0) {
                    continue;
                }
                if (has_space(slot.coord_, m->size())) {
                    m->create(APP.opponent_island(), slot.coord_);
                    return;
                }
            }
            ++tries;
        }
    };

    auto& mt = require_metaclass(room_name);

    find_connected_slots(mt->size().y);
    try_place_room(mt);
}



void ProcgenEnemyAI::generate_secondary_rooms()
{
    if (levelgen_enemy_count_ > 6 and rng::choice<2>(rng_source_)) {

        place_room_adjacent("infirmary");
    }

    int player_char_count = 0;
    int player_transporter_count = 0;
    for (auto& room : APP.player_island().rooms()) {
        player_char_count += length(room->characters());
        if (str_eq(room->name(), "transporter")) {
            ++player_transporter_count;
        }
    }

    if (player_char_count > 3 or player_transporter_count > 2) {
        place_room_adjacent("infirmary");
    }

    if (levelgen_enemy_count_ > 6 and rng::choice<2>(rng_source_)) {

        int count = rng::choice<3>(rng_source_);
        if (core_count_ == 4) {
            count++;
        }

        for (int i = 0; i < count; ++i) {
            place_room_adjacent("transporter");
        }
    }
}



void ProcgenEnemyAI::generate_foundation()
{
    // Generate filler structures to ensure that no rooms are floating.

    auto& mt = require_metaclass("masonry");
    auto& dynamite = require_metaclass("dynamite");
    auto& dynamite_ii = require_metaclass("dynamite-ii");

    for (u8 x = 0; x < (int)APP.opponent_island()->terrain().size(); ++x) {
        for (u8 y = 0; y < 15; ++y) {
            if (APP.opponent_island()->rooms_plot().get(x, y)) {
                if (auto room = APP.opponent_island()->get_room({x, y})) {
                    if (is_forcefield(room->metaclass())) {
                        // Don't put foundation blocks beneath a forcefield
                        continue;
                    }
                }
                for (u8 yy = y; yy < 15; ++yy) {
                    if (not APP.opponent_island()->rooms_plot().get(x, yy)) {
                        if (auto room = APP.opponent_island()->get_room(
                                {u8(x + 1), yy})) {
                            if ((*room->metaclass())->category() ==
                                    Room::Category::weapon and
                                not str_eq(room->name(), "missile-silo") and
                                not str_eq(room->name(), "rocket-bomb")) {
                                // Don't put a foundation block to the left of a
                                // cannon-type weapon.
                                continue;
                            }
                        }
                        if (auto room = APP.opponent_island()->get_room(
                                {x, u8(yy + 1)})) {
                            if (str_eq(room->name(), "drone-bay")) {
                                continue;
                            }
                        }
                        if (rng::choice<18>(rng_source_) == 0) {
                            dynamite->create(APP.opponent_island(), {x, yy});
                        } else if (rng::choice<40>(rng_source_) == 0) {
                            dynamite_ii->create(APP.opponent_island(), {x, yy});
                        } else {
                            mt->create(APP.opponent_island(), {x, yy});
                            if (auto r =
                                    APP.opponent_island()->get_room({x, yy})) {
                                if (auto m = r->cast<Masonry>()) {
                                    if (rng::choice<4>(rng_source_) == 0) {
                                        m->set_gfx(2);
                                    } else if (rng::choice<11>(rng_source_) ==
                                               0) {
                                        m->set_gfx(3);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}



void ProcgenEnemyAI::cleanup_unused_terrain()
{
    while (not APP.opponent_island()->terrain().empty()) {
        const u8 x_end = APP.opponent_island()->terrain().size() - 1;

        for (u8 y = 0; y < 16; ++y) {
            if (APP.opponent_island()->get_room({x_end, y})) {
                // Column contains a room, i.e. not unused.
                return;
            }
        }

        // Empty column, reduce terrain size by 1.
        APP.opponent_island()->init_terrain(x_end);
    }
}



void ProcgenEnemyAI::place_room_random_loc(int x_start, const char* room_name)
{
    auto mt = load_metaclass(room_name);

    if (not mt) {
        Platform::fatal(format("procgen ai: % missing!", room_name).c_str());
    }

    auto make_room = [&](const RoomCoord& position) {
        (*mt)->create(APP.opponent_island(), position);
    };

    int tries = 0;
    while (tries < 255) {
        u8 x = x_start + rng::choice((levelgen_size_.x - x_start), rng_source_);
        u8 y = 14 - rng::choice(levelgen_size_.y, rng_source_);

        if (has_space({x, y}, (*mt)->size())) {
            make_room({x, y});
            return;
        }

        ++tries;
    }
}



bool ProcgenEnemyAI::has_space(const RoomCoord& loc, const RoomCoord& sz)
{
    for (u8 x = 0; x < sz.x; ++x) {
        for (u8 y = 0; y < sz.y; ++y) {
            if (loc.y + y >= 15 or loc.x + x >= levelgen_size_.x) {
                return false;
            }
            if (APP.opponent_island()->rooms_plot().get(loc.x + x, loc.y + y)) {
                return false;
            }
        }
    }

    return true;
}



} // namespace skyland
