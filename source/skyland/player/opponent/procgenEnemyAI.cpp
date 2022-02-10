#include "procgenEnemyAI.hpp"
#include "bulkAllocator.hpp"
#include "skyland/roomTable.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



static SHARED_VARIABLE(sf_p1_coin_yield);
static SHARED_VARIABLE(sf_p2_coin_yield);
static SHARED_VARIABLE(sf_p3_coin_yield);
static SHARED_VARIABLE(sf_p4_coin_yield);



void prep_level(Platform& pfrm, App& app);



ProcgenEnemyAI::ProcgenEnemyAI(u8 difficulty) :
    difficulty_(difficulty)
{
}



void ProcgenEnemyAI::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.opponent_island()) {
        generate_level(pfrm, app);
    } else {
        EnemyAI::update(pfrm, app, delta);
    }

    if (not app.opponent_island() or app.player_island().is_destroyed()) {
        level_text_.reset();
    } else if (not level_text_) {
        level_text_.emplace(
            pfrm,
            OverlayCoord{u8(30 - integer_text_length(levelgen_enemy_count_)),
                         0});
    } else {
        level_text_->assign(levelgen_enemy_count_);
    }
}



void ProcgenEnemyAI::generate_level(Platform& pfrm, App& app)
{
    app.victory_coins();

    for (auto& room : app.player_island().rooms()) {
        // TODO: gather information about the layout of the player's castle.
        (void)room;
    }

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    pfrm.set_scroll(Layer::map_1_ext, -250, -374);
    pfrm.screen().clear();

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

    levelgen_size_.y = 4 + rng::choice<5>(rng::critical_state);

    if (levelgen_enemy_count_ < 4) {
        levelgen_size_.y = std::min(u8(5), levelgen_size_.y);
    }

    levelgen_size_.x = std::min(13, std::max(3, area / levelgen_size_.y));


    app.create_opponent_island(pfrm, levelgen_size_.x);
    app.opponent_island()->show_flag(true);

    generate_power_sources(pfrm, app);

    if (levelgen_enemy_count_ > 2 or rng::choice<2>(rng::critical_state)) {
        generate_stairwells(pfrm, app);
    }

    generate_secondary_rooms(pfrm, app);

    generate_hull(pfrm, app);

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
    generate_weapons(pfrm, app, weapon_limit);
    generate_forcefields(pfrm, app);

    generate_foundation(pfrm, app);

    generate_characters(pfrm, app);
    generate_decorations(pfrm, app);


    prep_level(pfrm, app);
    show_island_exterior(pfrm, app, app.opponent_island());

    // The level generation may have taken a long time. Shouldn't have any
    // bearing on gameplay.
    pfrm.delta_clock().reset();

    ++levelgen_enemy_count_;

    for (auto& room : app.opponent_island()->rooms()) {

        auto frac = 0.4f;

        if (levelgen_enemy_count_ > 20) {
            frac = sf_p4_coin_yield * 0.01;
        } else if (levelgen_enemy_count_ > 16) {
            frac = sf_p3_coin_yield * 0.01;
        } else if (levelgen_enemy_count_ > 12) {
            frac = sf_p2_coin_yield * 0.01;
        } else if (levelgen_enemy_count_ > 7) {
            frac = sf_p1_coin_yield * 0.01;
        }
        app.victory_coins() += frac * (*room->metaclass())->cost();
    }

    app.time_stream().enable_pushes(true);
    app.time_stream().clear();

    level_text_.emplace(pfrm,
            OverlayCoord{u8(30 - integer_text_length(levelgen_enemy_count_)),
                         0});
}



static const int level_threshold_two_powercores = 4;



void ProcgenEnemyAI::generate_power_sources(Platform& pfrm, App& app)
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
        place_room_random_loc(pfrm,
                              app,
                              levelgen_size_.x < 4 ? 1
                                                   : levelgen_size_.x / 2 - 1,
                              "power-core");
    }

    for (int i = 0; i < reactor_count; ++i) {
        place_room_random_loc(pfrm,
                              app,
                              levelgen_size_.x < 4 ? 1
                                                   : levelgen_size_.x / 2 - 1,
                              "reactor");
    }

    core_count_ += reactor_count;
}



Power ProcgenEnemyAI::power_remaining(App& app) const
{
    return app.opponent_island()->power_supply() -
           app.opponent_island()->power_drain();
}



void ProcgenEnemyAI::generate_weapons(Platform& pfrm, App& app, int max)
{
    int missile_count = 0;
    int drone_count = 0;
    int cannon_count = 0;
    int flak_count = 0;
    int nemesis_count = 0;
    int arc_count = 0;
    int forcefield_count = 0;
    int misc_cannon_count = 0;
    int ion_cannon_count = 0;

    for (auto& room : app.player_island().rooms()) {
        const auto category = (*room->metaclass())->category();

        if (str_eq(room->name(), "missile-silo")) {
            ++missile_count;
        } else if (str_eq(room->name(), "drone-bay")) {
            ++drone_count;
        } else if (str_eq(room->name(), "cannon")) {
            ++cannon_count;
        } else if (str_eq(room->name(), "forcefield")) {
            ++forcefield_count;
        } else if (str_eq(room->name(), "flak-gun")) {
            ++flak_count;
        } else if (str_eq(room->name(), "nemesis")) {
            ++nemesis_count;
        } else if (str_eq(room->name(), "arc-gun")) {
            ++arc_count;
        } else if (str_eq(room->name(), "ion-cannon")) {
            ++ion_cannon_count;
        } else if (category == Room::Category::weapon) {
            ++misc_cannon_count;
        }
    }


    int generic_cannon_count =
        cannon_count + flak_count + arc_count + misc_cannon_count;

    int player_avg_roof_hull_thickness = 0;

    for (u8 x = 0; x < (int)app.player_island().terrain().size(); ++x) {
        for (u8 y = 6; y < 15; ++y) {
            if (auto room = app.player_island().get_room({x, y})) {
                if ((*room->metaclass())->category() == Room::Category::wall) {
                    ++player_avg_roof_hull_thickness;
                } else {
                    break;
                }
            }
        }
    }

    if (app.player_island().terrain().size() not_eq 0) {
        player_avg_roof_hull_thickness /= app.player_island().terrain().size();
    }

    int player_avg_forward_hull_thickness = 0;
    int min_present_y = 15;

    for (u8 y = 6; y < 15; ++y) {
        for (u8 x = (int)app.player_island().terrain().size() - 1; x > 1; --x) {
            if (auto room = app.player_island().get_room({x, y})) {
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

    struct Context {
        Buffer<RoomMeta*, 200> distribution_;

        struct Pair {
            const char* weapon_name_;
            Float probability_;
        };

        bool invalidated_missile_cells_[16][16];
        bool invalidated_cannon_cells_[16][16];

        Buffer<Pair, 30> pairs_;
    };

    auto c = allocate_dynamic<Context>(pfrm);

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            c->invalidated_cannon_cells_[x][y] = false;
            c->invalidated_missile_cells_[x][y] = false;
        }
    }

    auto enq_prob = [&](const char* mt_name, Float prob) {
        c->pairs_.push_back({mt_name, prob});
    };

    enq_prob("cannon", 100.f);

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

    if (levelgen_enemy_count_ > 9 and difficulty_ > 0) {
        if (missile_count < 5) {
            enq_prob("drone-bay",
                     (50.f - missile_count * 10.f) +
                     drone_count * 20.f);
        }
    }

    if (levelgen_enemy_count_ > 8) {
        enq_prob("arc-gun", 120.f);
        enq_prob("nemesis", 120.f);

        enq_prob("missile-silo",
                 300.f + 10.f * missile_count + 10 * generic_cannon_count +
                     50.f * drone_count);
    } else {
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
        Buffer<Vec2<u8>, 16> slots;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 6; y < 14; ++y) {
                auto room = app.opponent_island()->get_room({x, u8(y + 2)});
                if ((y == 13 or room) and has_space(app, {x, y}, {1, 2}) and
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

        rng::shuffle(slots, rng::critical_state);

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(pfrm, app, app.opponent_island(), {s.x, s.y});

            for (int yy = 0; yy < s.y; ++yy) {
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_cannon_cells_[s.x][yy] = true;
            }
        }
    };

    auto place_cannon = [&](RoomMeta* mt) {
        Buffer<Vec2<u8>, 16> slots;


        for (u8 y = 6; y < 14; ++y) {
            for (u8 x = 0; x < 15; ++x) {

                auto room = app.opponent_island()->get_room(
                    {u8(x + (*mt)->size().x), y});

                bool invalid = false;
                for (int xx = x; xx < x + (*mt)->size().x; ++xx) {
                    if (c->invalidated_cannon_cells_[xx][y]) {
                        invalid = true;
                    }
                }

                if ((room or
                     app.opponent_island()->get_room({x, u8(y + 1)})) and
                    has_space(app, {x, y}, (*mt)->size()) and not invalid) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng::critical_state);

        // Placing weapons nearer to the leftmost position reduces the chances
        // of placing cannons in the path of other cannons.
        std::sort(slots.begin(), slots.end(), [](auto& lhs, auto& rhs) {
            return lhs.x < rhs.x;
        });

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(pfrm, app, app.opponent_island(), {s.x, s.y});

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

        Buffer<Vec2<u8>, 16> slots;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 6; y < 14; ++y) {
                auto room = app.opponent_island()->get_room({x, u8(y + 1)});
                if ((y == 13 or room) and has_space(app, {x, y}, {2, 1}) and
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

        rng::shuffle(slots, rng::critical_state);

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(pfrm, app, app.opponent_island(), {s.x, s.y});

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

    int place_missile_count = 0;

    for (int i = 0; i < max; ++i) {
        auto sel = c->distribution_[rng::choice(c->distribution_.size(),
                                                rng::critical_state)];

        if (power_remaining(app) > (*sel)->consumes_power()) {

            if (str_eq((*sel)->name(), "missile-silo")) {
                ++place_missile_count;
            } else if (str_eq((*sel)->name(), "drone-bay")) {
                place_drone_bay(sel);
            } else {
                place_cannon(sel);
            }

        } else {
            break;
        }
    }

    for (int i = 0; i < place_missile_count; ++i) {
        place_missile_silo(&require_metaclass("missile-silo"));
    }
}



void ProcgenEnemyAI::generate_forcefields(Platform& pfrm, App& app)
{
    struct Context {
        struct Slot {
            Vec2<u8> coord_;
            Float weight_;
        };

        Buffer<Slot, 60> slots_;
    };

    auto c = allocate_dynamic<Context>(pfrm);


    auto find_ideal_forcefield_locs = [&] {
        c->slots_.clear();

        int player_missile_count = 0;
        int player_cannon_count = 0;
        for (auto& room : app.player_island().rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                if (str_eq(room->name(), "missile-silo")) {
                    ++player_missile_count;
                } else {
                    ++player_cannon_count;
                }
            }
        }

        int opponent_missile_count = 0;
        int opponent_cannon_count = 0;
        for (auto& room : app.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                if (str_eq(room->name(), "missile-silo")) {
                    ++opponent_missile_count;
                } else {
                    ++opponent_cannon_count;
                }
            }
        }

        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = 6; y < 15; ++y) {
                Float weight = 0.f;

                auto get_room = [&](u8 x, u8 y) {
                    return app.opponent_island()->get_room({x, y});
                };

                if (x < 15) {
                    if (app.opponent_island()->rooms_plot().get(x + 1, y)) {
                        if (auto room = get_room(x + 1, y)) {
                            if ((not str_eq(room->name(), "missile-silo")) and
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
                    if (app.opponent_island()->rooms_plot().get(x, y + 1)) {
                        if (auto room = get_room(x, y + 1)) {
                            if (str_eq(room->name(), "missile-silo") or
                                str_eq(room->name(), "drone-bay")) {
                                if (opponent_missile_count >
                                    player_missile_count) {
                                    weight += 150.f;
                                } else {
                                    weight += 100.f;
                                }
                            } else if (str_eq(room->name(), "drone-bay")) {
                                if (player_missile_count) {
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

        rng::shuffle(c->slots_, rng::critical_state);

        std::sort(c->slots_.begin(), c->slots_.end(), [](auto& lhs, auto& rhs) {
            return lhs.weight_ > rhs.weight_;
        });
    };


    auto& mt = require_metaclass("forcefield");

    while (true) {
        const auto power = power_remaining(app);
        if (power < mt->consumes_power()) {
            return;
        }

        find_ideal_forcefield_locs();

        if (not c->slots_.empty()) {
            bool placed = false;
            for (auto& slot : c->slots_) {
                if (has_space(app, slot.coord_, {1, 1})) {
                    placed = true;
                    mt->create(pfrm, app, app.opponent_island(), slot.coord_);
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



void ProcgenEnemyAI::generate_hull(Platform& pfrm, App& app)
{
    auto& hull = require_metaclass("hull");

    for (u8 x = 0; x < 15; ++x) {

        for (u8 y = 0; y < 14; ++y) {

            if (app.opponent_island()->rooms_plot().get(x, y + 1)) {
                auto below = app.opponent_island()->get_room({x, u8(y + 1)});

                if (below and (*below->metaclass())->category() not_eq
                                  Room::Category::wall) {
                    if (not app.opponent_island()->rooms_plot().get(x, y)) {
                        hull->create(pfrm, app, app.opponent_island(), {x, y});
                    }
                }
            }
        }
    }

    for (u8 x = 0; x < 15; ++x) {
        for (u8 y = 0; y < 15; ++y) {

            if (app.opponent_island()->rooms_plot().get(x + 1, y)) {
                auto right = app.opponent_island()->get_room({u8(x + 1), y});
                if (not right) {
                    continue;
                }
                if ((*right->metaclass())->category() ==
                        Room::Category::weapon and
                    not str_eq(right->name(), "missile-silo")) {
                    continue;
                }

                if ((*right->metaclass())->category() not_eq
                        Room::Category::wall and
                    not app.opponent_island()->rooms_plot().get(x, y)) {
                    hull->create(pfrm, app, app.opponent_island(), {x, y});
                }
            }
        }
    }
}



void ProcgenEnemyAI::generate_stairwells(Platform& pfrm, App& app)
{
    // Generate stairwells

    if (levelgen_size_.x <= 3) {
        return;
    }

    struct StairwellPos {
        Vec2<u8> coord_;
        u8 val_;
    };

    struct Context {
        u8 matrix[16][16];
        bool walkable_zones[16][16];
        Buffer<StairwellPos, 30> slots;
    };

    auto c = allocate_dynamic<Context>(pfrm);


    auto mt = load_metaclass("stairwell");

    auto find_ideal_stairwell_slots = [&] {
        c->slots.clear();
        app.opponent_island()->plot_walkable_zones(app, c->walkable_zones);

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                c->matrix[x][y] = 0;
            }
        }

        for (int x = 1; x < (int)app.opponent_island()->terrain().size(); ++x) {
            for (int y = 7; y < 15; ++y) {
                if (app.opponent_island()->rooms_plot().get(x, y)) {
                    c->matrix[x][y] = 0;
                } else {
                    // For a column the size of a stairwell room: how many
                    // walkable tiles would it connect if we placed it starting
                    // at (x,y)?
                    for (int yy = y; yy < std::min(y + 4, 15); ++yy) {
                        if (app.opponent_island()->rooms_plot().get(x, yy)) {
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
        for (u8 x = 1; x < (int)app.opponent_island()->terrain().size(); ++x) {
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
    if (app.opponent_island()->rooms().size() >= 4) {
        count = 2;
    }

    int tries = 0;
    while (count and tries < 255) {
        for (auto& slot : c->slots) {
            if (count == 0) {
                break;
            }
            if (has_space(app, slot.coord_, (*mt)->size())) {
                (*mt)->create(pfrm, app, app.opponent_island(), slot.coord_);
                --count;
                tries = 0;
                break;
            }
        }
        find_ideal_stairwell_slots();
        ++tries;
    }
}



void ProcgenEnemyAI::generate_characters(Platform& pfrm, App& app)
{
    int transporter_count = 0;
    for (auto& room : app.opponent_island()->rooms()) {
        if (str_eq(room->name(), "transporter")) {
            ++transporter_count;
        }
    }

    const int chr_count =
        core_count_ + rng::choice(transporter_count, rng::critical_state);

    struct Context {
        struct Slot {
            Vec2<u8> coord_;
            Float weight_;
        };

        Buffer<Slot, 30> slots_;

        bool matrix_[16][16];
    };

    auto c = allocate_dynamic<Context>(pfrm);

    for (int i = 0; i < chr_count; ++i) {
        app.opponent_island()->plot_walkable_zones(app, c->matrix_);

        c->slots_.clear();

        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = 0; y < 15; ++y) {
                if (c->matrix_[x][y]) {
                    if (auto room = app.opponent_island()->get_room({x, y})) {
                        c->slots_.push_back(
                            {{x, y}, (*room->metaclass())->ai_base_weight()});
                    }
                }
            }
        }

        rng::shuffle(c->slots_, rng::critical_state);

        std::sort(c->slots_.begin(), c->slots_.end(), [](auto& lhs, auto& rhs) {
            return lhs.weight_ > rhs.weight_;
        });

        if (not c->slots_.empty()) {
            for (auto& slot : c->slots_) {
                if (app.opponent_island()->character_at_location(slot.coord_)) {
                    continue;
                }
                app.opponent_island()->add_character(
                    alloc_entity<BasicCharacter>(app.opponent_island(),
                                                 &app.opponent(),
                                                 slot.coord_,
                                                 false));
                break;
            }
        }
    }
}



void ProcgenEnemyAI::generate_decorations(Platform& pfrm, App& app)
{
    auto& shrubbery_mt = require_metaclass("shrubbery");

    for (u8 x = 0; x < (int)app.opponent_island()->terrain().size(); ++x) {
        bool empty_column = true;
        u8 y;
        for (y = 6; y < 14; ++y) {
            if (app.opponent_island()->rooms_plot().get(x, y)) {
                if (auto room = app.opponent_island()->get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                        Room::Category::decoration) {
                        empty_column = false;
                        y -= 1;
                        break;
                    }
                    y -= 1;
                    break;
                }
            }
        }


        if (empty_column and y > 6) {
            switch (rng::choice<5>(rng::critical_state)) {
            case 0:
            case 1:
            case 2:
                break;

            case 3:
            case 4:
                shrubbery_mt->create(pfrm, app, app.opponent_island(), {x, y});
                break;
            }
        }
    }
}



void ProcgenEnemyAI::generate_secondary_rooms(Platform& pfrm, App& app)
{
    struct Slot {
        Vec2<u8> coord_;
        u8 val_;
    };

    struct Context {
        u8 matrix[16][16];
        bool walkable_zones[16][16];
        Buffer<Slot, 50> slots;
    };

    auto c = allocate_dynamic<Context>(pfrm);

    auto find_connected_slots = [&](int room_height) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                c->matrix[x][y] = 0;
            }
        }

        c->slots.clear();
        app.opponent_island()->plot_walkable_zones(app, c->walkable_zones);

        for (int x = 0; x < (int)app.opponent_island()->terrain().size(); ++x) {
            for (int y = 7; y < 15; ++y) {
                if (app.opponent_island()->rooms_plot().get(x, y)) {
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

        rng::shuffle(c->slots, rng::critical_state);
    };

    auto try_place_room = [&](RoomMeta& m) {
        int tries = 0;
        while (tries < 255) {
            for (auto& slot : c->slots) {
                if (slot.coord_.x == 0) {
                    continue;
                }
                if (has_space(app, slot.coord_, m->size())) {
                    m->create(pfrm, app, app.opponent_island(), slot.coord_);
                    return;
                }
            }
            ++tries;
        }
    };

    if (levelgen_enemy_count_ > 6 and rng::choice<2>(rng::critical_state)) {
        auto& mt = require_metaclass("infirmary");

        find_connected_slots(mt->size().y);

        try_place_room(mt);
    }

    if (levelgen_enemy_count_ > 6 and rng::choice<2>(rng::critical_state)) {

        auto& mt = require_metaclass("transporter");

        int count = rng::choice<3>(rng::critical_state);
        if (core_count_ == 4) {
            count++;
        }

        for (int i = 0; i < count; ++i) {
            find_connected_slots(mt->size().y);
            try_place_room(mt);
        }
    }
}



void ProcgenEnemyAI::generate_foundation(Platform& pfrm, App& app)
{
    // Generate filler structures to ensure that no rooms are floating.

    auto& mt = require_metaclass("masonry");
    auto& dynamite = require_metaclass("dynamite");
    auto& dynamite_ii = require_metaclass("dynamite-ii");

    for (u8 x = 0; x < (int)app.opponent_island()->terrain().size(); ++x) {
        for (u8 y = 0; y < 15; ++y) {
            if (app.opponent_island()->rooms_plot().get(x, y)) {
                if (auto room = app.opponent_island()->get_room({x, y})) {
                    if (str_eq(room->name(), "forcefield")) {
                        // Don't put foundation blocks beneath a forcefield
                        continue;
                    }
                }
                for (u8 yy = y; yy < 15; ++yy) {
                    if (not app.opponent_island()->rooms_plot().get(x, yy)) {
                        if (auto room = app.opponent_island()->get_room(
                                {u8(x + 1), yy})) {
                            if ((*room->metaclass())->category() ==
                                    Room::Category::weapon and
                                not str_eq(room->name(), "missile-silo")) {
                                // Don't put a foundation block to the left of a
                                // cannon-type weapon.
                                continue;
                            }
                        }
                        if (rng::choice<18>(rng::critical_state) == 0) {
                            dynamite->create(
                                pfrm, app, app.opponent_island(), {x, yy});
                        } else if (rng::choice<35>(rng::critical_state) == 0) {
                            dynamite_ii->create(
                                pfrm, app, app.opponent_island(), {x, yy});
                        } else {
                            mt->create(
                                pfrm, app, app.opponent_island(), {x, yy});
                        }
                    }
                }
            }
        }
    }
}



void ProcgenEnemyAI::place_room_random_loc(Platform& pfrm,
                                           App& app,
                                           int x_start,
                                           const char* room_name)
{
    auto mt = load_metaclass(room_name);

    if (not mt) {
        Platform::fatal(format("procgen ai: % missing!", room_name).c_str());
    }

    auto make_room = [&](const Vec2<u8>& position) {
        (*mt)->create(pfrm, app, app.opponent_island(), position);
    };

    int tries = 0;
    while (tries < 255) {
        u8 x = x_start +
               rng::choice((levelgen_size_.x - x_start), rng::critical_state);
        u8 y = 14 - rng::choice(levelgen_size_.y, rng::critical_state);

        if (has_space(app, {x, y}, (*mt)->size())) {
            make_room({x, y});
            return;
        }

        ++tries;
    }
}



bool ProcgenEnemyAI::has_space(App& app,
                               const Vec2<u8>& loc,
                               const Vec2<u8>& sz)
{
    for (u8 x = 0; x < sz.x; ++x) {
        for (u8 y = 0; y < sz.y; ++y) {
            if (loc.y + y >= 15 or loc.x + x >= levelgen_size_.x) {
                return false;
            }
            if (app.opponent_island()->rooms_plot().get(loc.x + x, loc.y + y)) {
                return false;
            }
        }
    }

    return true;
}



} // namespace skyland
