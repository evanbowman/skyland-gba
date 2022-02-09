#include "procgenEnemyAI.hpp"
#include "skyland/skyland.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/room_metatable.hpp"
#include "bulkAllocator.hpp"
#include "skyland/roomTable.hpp"



namespace skyland {



void prep_level(Platform& pfrm, App& app);



void ProcgenEnemyAI::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.opponent_island()) {
        generate_level(pfrm, app);
    } else {
        EnemyAI::update(pfrm, app, delta);
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
            return 60;
        }
    }();

    levelgen_size_.y = 4 + rng::choice<5>(rng::critical_state);

    if (levelgen_enemy_count_ < 4) {
        levelgen_size_.y = std::min(u8(5), levelgen_size_.y);
    }

    levelgen_size_.x = std::min(13, std::max(3, area / levelgen_size_.y));


    app.create_opponent_island(pfrm, levelgen_size_.x);

    generate_power_sources(pfrm, app);

    if (levelgen_enemy_count_ > 2 or rng::choice<2>(rng::critical_state)) {
        generate_stairwells(pfrm, app);
    }

    generate_secondary_rooms(pfrm, app);

    generate_hull(pfrm, app);

    // generate_weapons(pfrm, app, levelgen_enemy_count_ < 3 ? 2 : 1000);
    // if (levelgen_enemy_count_ > 4) {
    // generate_hull(pfrm, app);
    // }
    generate_foundation(pfrm, app);


    prep_level(pfrm, app);
    show_island_exterior(pfrm, app, app.opponent_island());

    // The level generation may have taken a long time. Shouldn't have any
    // bearing on gameplay.
    pfrm.delta_clock().reset();

    ++levelgen_enemy_count_;

    for (auto& room : app.opponent_island()->rooms()) {
        app.victory_coins() += 0.2f * (*room->metaclass())->cost();
    }
}



void ProcgenEnemyAI::generate_power_sources(Platform& pfrm, App& app)
{
    int core_count = 0;
    // int reactor_count = 0;

    if (levelgen_enemy_count_ < 4) {
        core_count = 1;
    } else if (levelgen_enemy_count_ < 10) {
        core_count = 2;
    } else {
        core_count = 3;
    }

    for (int i = 0; i < core_count; ++i) {
        place_room_random_loc(pfrm, app,
                              levelgen_size_.x < 4 ? 1 :
                              levelgen_size_.x / 2 - 1,
                              "power-core");
    }
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
        const auto category = room->category();

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

    int player_avg_roof_hull_thickness = 0;

    for (u8 x = 0; x < (int)app.player_island().terrain().size(); ++x) {
        for (u8 y = 6; y < 15; ++y) {
            if (auto room = app.player_island().get_room({x, y})) {
                if (room->category() == Room::Category::wall) {
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
                if (room->category() == Room::Category::wall) {
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

        Buffer<Pair, 30> pairs_;
    };

    auto c = allocate_dynamic<Context>(pfrm);

    auto enq_prob = [&](const char* mt_name, Float prob) {
        c->pairs_.push_back({mt_name, prob});
    };

    enq_prob("cannon", 100.f);

    enq_prob("missile-silo", 100.f);

    if (levelgen_enemy_count_ > 5) {
        Float flak_prob = 100.f;
        flak_prob += (player_avg_forward_hull_thickness < 2 ? 50.f : -50.f);
        if (nemesis_count) {
            flak_prob += 100.f;
        }
        enq_prob("flak-gun", flak_prob);
    }

    if (forcefield_count) {
        enq_prob("ion-cannon", 40.f + forcefield_count * 30.f);
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
            for (u8 y = 5; y < 14; ++y) {
                if (auto room = app.opponent_island()->get_room({x, u8(y + 2)})) {
                    if (not str_eq(room->name(), "missile-silo") and
                        has_space(app, {x, y}, {1, 2})) {
                        slots.push_back({x, y});
                    }
                }
            }
        }
        rng::shuffle(slots, rng::critical_state);
        if (not slots.empty()) {
            (*mt)->create(pfrm, app, app.opponent_island(), slots[0]);
        }
    };

    auto place_cannon = [&](RoomMeta* mt, u8 width, u8 height) {
        Buffer<Vec2<u8>, 16> slots;
        for (u8 y = 7; y < 15; ++y) {
            bool existing_weapon = false;
            bool seen_room = false;
            for (u8 x = 0; x < (int)app.opponent_island()->terrain().size(); ++x) {
                if (auto room = app.opponent_island()->get_room({x, y})) {
                    seen_room = true;
                    if (room->category() == Room::Category::weapon) {
                        existing_weapon = true;
                        break;
                    }
                }
            }
            if (existing_weapon) {
                break;
            }

            for (u8 x = 0; x < app.opponent_island()->terrain().size(); ++x) {
                if (seen_room and
                    has_space(app, {x, y}, {width, height})) {
                    slots.push_back({x, y});
                } else if (app.opponent_island()->get_room({x, y})) {
                    break;
                }
            }
        }
        rng::shuffle(slots, rng::critical_state);
        if (not slots.empty()) {
            (*mt)->create(pfrm, app, app.opponent_island(), slots[0]);
        }
    };

    while (max) {
        auto sel = c->distribution_[rng::choice(c->distribution_.size(),
                                                rng::critical_state)];

        if (power_remaining(app) >= (*sel)->consumes_power()) {
            --max;
            if (str_eq((*sel)->name(), "missile-silo")) {
                place_missile_silo(sel);
            } else {
                place_cannon(sel, (*sel)->size().x, (*sel)->size().y);
            }
        } else {
            break;
        }
    }
}



void ProcgenEnemyAI::generate_hull(Platform& pfrm, App& app)
{
    auto& hull = require_metaclass("hull");

    for (u8 x = 0; x < 15; ++x) {

        bool missile_in_column = false;
        for (u8 yy = 0; yy < 15; ++yy) {
            if (auto room = app.opponent_island()->get_room({x, yy})) {
                if (str_eq(room->name(), "missile-silo")) {
                    missile_in_column = true;
                    break;
                }
            }
        }

        if (missile_in_column) {
            continue;
        }

        for (u8 y = 0; y < 14; ++y) {

            if (app.opponent_island()->rooms_plot().get(x, y + 1)) {
                if (not app.opponent_island()->rooms_plot().get(x, y)) {
                    hull->create(pfrm, app, app.opponent_island(), {x, y});
                }
            }
        }
    }

    for (u8 x = 0; x < 15; ++x) {
        for (u8 y = 0; y < 15; ++y) {

            bool missile_in_column = false;
            for (u8 yy = 0; yy < 15; ++yy) {
                if (auto room = app.opponent_island()->get_room({x, yy})) {
                    if (str_eq(room->name(), "missile-silo")) {
                        missile_in_column = true;
                        break;
                    }
                }
            }

            if (missile_in_column) {
                continue;
            }

            if (app.opponent_island()->rooms_plot().get(x + 1, y)) {
                auto right = app.opponent_island()->get_room({u8(x + 1), y});
                if (right->category() == Room::Category::weapon and
                    not str_eq(right->name(), "missile-silo")) {
                    continue;
                }
                if (not app.opponent_island()->rooms_plot().get(x, y)) {
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

    if (levelgen_enemy_count_ > 6 and
        rng::choice<2>(rng::critical_state)) {
        auto& mt = require_metaclass("infirmary");

        find_connected_slots(mt->size().y);

        try_place_room(mt);
    }

    if (levelgen_enemy_count_ > 6 and
        rng::choice<2>(rng::critical_state)) {

        auto& mt = require_metaclass("transporter");

        for (int i = 0; i < rng::choice<3>(rng::critical_state); ++i) {
            find_connected_slots(mt->size().y);
            try_place_room(mt);
        }
    }

}



void ProcgenEnemyAI::generate_foundation(Platform& pfrm, App& app)
{
    // Generate filler structures to ensure that no rooms are floating.

    auto& mt = require_metaclass("masonry");
    auto& hull = require_metaclass("hull");
    auto& dynamite = require_metaclass("dynamite");
    auto& dynamite_ii = require_metaclass("dynamite-ii");

    for (u8 x = 0; x < (int)app.opponent_island()->terrain().size(); ++x) {
        for (u8 y = 0; y < 15; ++y) {
            if (app.opponent_island()->rooms_plot().get(x, y)) {
                for (u8 yy = y; yy < 15; ++yy) {
                    if (not app.opponent_island()->rooms_plot().get(x, yy)) {
                        if (rng::choice<20>(rng::critical_state) == 0) {
                            dynamite->create(pfrm, app, app.opponent_island(), {x, yy});
                        } else if (rng::choice<40>(rng::critical_state) == 0) {
                            dynamite_ii->create(pfrm, app, app.opponent_island(), {x, yy});
                        } else if (app.opponent_island()->rooms_plot().get(x, yy + 1)) {
                            hull->create(pfrm, app, app.opponent_island(), {x, yy});
                        } else {
                            mt->create(pfrm, app, app.opponent_island(), {x, yy});
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
        u8 x = x_start + rng::choice((levelgen_size_.x - x_start), rng::critical_state);
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
            if (app.opponent_island()->rooms_plot().get(loc.x + x,
                                                        loc.y + y)) {
                return false;
            }
        }
    }

    return true;
}



}
