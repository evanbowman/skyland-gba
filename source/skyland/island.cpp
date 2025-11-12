////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "island.hpp"
#include "alloc_entity.hpp"
#include "entity/explosion/explosion.hpp"
#include "entity/misc/smokePuff.hpp"
#include "entity/projectile/projectile.hpp"
#include "ext_workram_data.hpp"
#include "globals.hpp"
#include "latency.hpp"
#include "minimap.hpp"
#include "network.hpp"
#include "number/random.hpp"
#include "platform/flash_filesystem.hpp"
#include "roomPool.hpp"
#include "room_metatable.hpp"
#include "rooms/chaosCore.hpp"
#include "rooms/core.hpp"
#include "rooms/portal.hpp"
#include "rooms/weapon.hpp"
#include "script/lisp.hpp"
#include "skyland.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/ghost.hpp"
#include "skyland/rooms/canvas.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/speaker.hpp"
#include "skyland/rooms/synth.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "tile.hpp"



namespace skyland
{



void Island::init_terrain(int width, bool render)
{
    if (width < 0) {
        terrain_.clear();
        while (width < 0) {
            terrain_.push_back(0);
            ++width;
        }
        return;
    }

    if (width < 3) {
        return;
    }

    terrain_.clear();

    terrain_.push_back(13), --width;

    for (int i = 0; i < width - 1; ++i) {
        terrain_.push_back(12);
    }

    terrain_.push_back(14);

    if (render) {
        render_terrain();
    }
}


EXT_WORKRAM_DATA RoomMatrix player_room_matrix;
EXT_WORKRAM_DATA RoomMatrix opponent_room_matrix;


Island::Island(Layer layer, u8 width, Player& owner)
    : owner_(&owner), rooms_(owner_ == &APP.opponent() ? &opponent_room_matrix
                                                       : &player_room_matrix),
      layer_(layer), timer_(0), flag_anim_index_(Tile::flag_start),
      interior_visible_(false), show_flag_(false), dispatch_cancelled_(false),
      schedule_repaint_(false), schedule_repaint_partial_(false),
      has_radar_(false), is_boarded_(false), hidden_(false)
{
    init_terrain(width, false);
}



Island::Rooms& Island::rooms()
{
    return rooms_;
}


void Island::remove_character(const RoomCoord& location)
{
    if (auto room = get_room(location)) {
        for (auto it = room->edit_characters().begin();
             it not_eq room->edit_characters().end();) {
            if ((*it)->grid_position() == location) {
                room->edit_characters().erase(it);
                room->update_description();
                return;
            } else {
                ++it;
            }
        }
    }

    for (auto it = characters_.begin(); it not_eq characters_.end();) {
        if ((*it)->grid_position() == location) {
            characters_.erase(it);
            return;
        } else {
            ++it;
        }
    }
}


Character* Island::character_at_location(const RoomCoord& loc)
{
    if (auto room = get_room(loc)) {
        for (auto& chr : room->characters()) {
            if (chr->grid_position() == loc) {
                return chr.get();
            }
        }
    }
    for (auto& chr : characters_) {
        if (chr->grid_position() == loc) {
            return chr.get();
        }
    }

    return nullptr;
}



std::pair<Character*, Room*> Island::find_character_by_id(CharacterId id)
{
    for (auto& room : rooms_) {
        for (auto& character : room->characters()) {
            if (character->id() == id) {
                return {character.get(), room.get()};
            }
        }
    }
    return {nullptr, nullptr};
}



void Island::show_powerdown_opts(bool show)
{
    show_powerdown_opts_ = show;
    schedule_repaint();
}



int Island::smoke_sprite()
{
    if (dark_smoke_) {
        return 61;
    } else {
        return 27;
    }
}



static auto fire_alloc_texture(Island& island)
{
    Optional<Platform::DynamicTexturePtr> result;

    // Check to see if the other island already has a texture allocated
    // for the fire effect. If so, share the texture.
    if (is_player_island(&island) and APP.opponent_island()) {
        result = APP.opponent_island()->fire_texture();
    } else if (&island == APP.opponent_island()) {
        result = APP.player_island().fire_texture();
    }

    if (not result) {
        result = PLATFORM.make_dynamic_texture();
    }

    if (result) {
        (*result)->remap(154);
    }

    return result;
}



static const auto fire_spread_time = seconds(9);



void Island::FireState::rewind(Island& island, Time delta)
{
    if (spread_timer_ > 0) {
        spread_timer_ -= delta;
    } else {
        spread_timer_ += fire_spread_time - delta;
    }

    if (damage_timer_ > 0) {
        damage_timer_ -= delta;
    } else {

        bool present = false;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (island.fire_present({x, y})) {
                    present = true;
                }
            }
        }

        if (present and not texture_) {

            texture_ = fire_alloc_texture(island);

        } else if (not present) {
            texture_.reset();
        }

        damage_timer_ += seconds(2) - delta;
    }

    anim_timer_ -= delta;
    if (anim_timer_ < 0) {
        anim_timer_ += milliseconds(100);

        --anim_index_;
        if (anim_index_ == -1) {
            anim_index_ = 5;
        }

        if (texture_ and
            // We only want to maintain a single reference to the fire texture,
            // to save vram. Both islands share a texture for the fire effect,
            // and only one island updates the tile glyph.
            (texture_->strong_count() == 1 or is_player_island(&island))) {
            (*texture_)->remap(154 + anim_index_);
        }
    }
}



void Island::rewind(Time delta)
{
    timer_ -= delta;

    if (show_flag_ and flag_pos_) {
        flag_anim_timer_ -= delta;
        if (flag_anim_timer_ < 0) {
            flag_anim_timer_ = milliseconds(80);
            auto current = flag_anim_index_;
            if (current > Tile::flag_start) {
                --flag_anim_index_;
            } else {
                flag_anim_index_ = Tile::flag_end;
            }
        }
    }

    if (chimney_loc_) {
        chimney_spawn_timer_ -= delta;

        if (chimney_spawn_timer_ < 0) {
            chimney_spawn_timer_ = milliseconds(600);

            auto o = origin();

            o.x += Fixnum::from_integer(chimney_loc_->x * 16 + 8);
            o.y += Fixnum::from_integer(chimney_loc_->y * 16 - 4);

            if (drift_ == 0.0_fixed) {
                // FIXME: faking the rewound smoke effects doesn't work if the
                // island is moving. So just don't spawn them. We'll need to
                // think of another way.
                if (auto e = APP.alloc_entity<SmokePuff>(o, smoke_sprite())) {
                    e->jump_to_end();
                    APP.effects().push(std::move(e));
                }
            }
        }
    }

    auto& projectiles = this->projectiles();
    for (auto it = projectiles.begin(); it not_eq projectiles.end();) {
        if ((*it)->health() == 0) {
            it = projectiles.erase(it);
        } else {
            (*it)->rewind(delta);
            ++it;
        }
    }

    bulk_timer_.rewind(delta);

    for (auto& room : rooms_) {
        room->rewind(delta);
        for (auto& chr : room->characters()) {
            chr->rewind(delta);
        }
    }

    if (drift_ not_eq 0.0_fixed) {
        position_.x -= drift_ * Fixnum::from_integer(delta);
    }


    for (auto it = drones_.begin(); it not_eq drones_.end();) {
        if (not(*it)->alive()) {
            it = drones_.erase(it);
        } else {
            (*it)->rewind(delta);
            ++it;
        }
    }


    if (schedule_repaint_) {
        schedule_repaint_ = false;
        schedule_repaint_partial_ = false;
        repaint();
    } else if (schedule_repaint_partial_) {
        schedule_repaint_partial_ = false;
        repaint_partial();
    }


    s8 ambient_offset = 4 * float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
                        std::numeric_limits<s16>::max();

    if (mountain_terrain_) {
        ambient_offset = 0;
    }

    ambient_movement_ = ambient_offset;


    PLATFORM.set_scroll(layer(),
                        -get_position().x.as_integer(),
                        -get_position().y.as_integer() - ambient_offset);

    fire_.rewind(*this, delta);
}



void Island::unamplify_blocks()
{
    for (auto& r : rooms()) {
        r->amplify(false);
    }
}



void Island::check_destroyed()
{
    destroyed_ = false;

    if (owner_ == &APP.opponent()) {
        if (APP.opponent().is_friendly()) {
            // Just as a failsafe! You cannot destroy friendly
            // islands. Impossible. They won't even blow up if you found some
            // hacky way to do it.
            return;
        }
    }

    bool has_core = false;
    for (auto& room : rooms_) {
        if ((*room->metaclass())->category() == Room::Category::power) {
            has_core = true;
            break;
        }
    }
    if (not has_core) {
        destroyed_ = true;
    }
}



void Island::set_hidden(bool hidden)
{
    bool was_hidden = hidden_;

    hidden_ = hidden;

    if (not hidden_ and was_hidden) {
        repaint();
    } else if (hidden_ and not was_hidden) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                PLATFORM.set_tile(layer(), x, y, 0);
            }
        }
    }
}



Optional<Platform::DynamicTexturePtr> Island::fire_texture()
{
    return fire_.texture_;
}



bool Island::fire_present(const RoomCoord& coord) const
{
    if (not fire_.texture_) {
        // If the fire texture doesn't exist, then we know the island isn't
        // burning, and don't need to do a lookup.
        return false;
    }
    return fire_.positions_.get(coord.x, coord.y);
}



void Island::fires_extinguish()
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            fire_extinguish({(u8)x, (u8)y});
        }
    }
}



void Island::fire_extinguish(const RoomCoord& coord)
{
    u8 x = clamp((int)coord.x, 0, 15);
    u8 y = clamp((int)coord.y, 0, 15);

    if (not fire_.positions_.get(x, y)) {
        return;
    }

    if (this == &player_island()) {
        time_stream::event::PlayerFireExtinguished e;
        e.x_ = x;
        e.y_ = y;
        APP.push_time_stream(e);
    } else {
        time_stream::event::OpponentFireExtinguished e;
        e.x_ = x;
        e.y_ = y;
        APP.push_time_stream(e);
    }

    fire_.positions_.set(x, y, false);
}



void Island::fire_create(const RoomCoord& coord)
{
    u8 x = clamp((int)coord.x, 0, 15);
    u8 y = clamp((int)coord.y, 0, 15);


    if (fire_.positions_.get(x, y)) {
        return;
    }

    if (this == &player_island()) {
        time_stream::event::PlayerFireCreated e;
        e.x_ = x;
        e.y_ = y;
        APP.push_time_stream(e);
    } else {
        time_stream::event::OpponentFireCreated e;
        e.x_ = x;
        e.y_ = y;
        APP.push_time_stream(e);
    }

    fire_.positions_.set(x, y, true);
}



void Island::FireState::update(Island& island, Time delta)
{
    damage_timer_ += delta;
    spread_timer_ += delta;

    if (spread_timer_ > fire_spread_time) {
        spread_timer_ -= fire_spread_time;

        auto mat = allocate_dynamic<bool[16][16]>("fire-spread-paths");
        bool plotted = false;

        Bitmatrix<16, 16> old_positions = positions_;

        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (old_positions.get(x, y)) {

                    if (not plotted) {
                        island.plot_walkable_zones(*mat, nullptr);
                        plotted = true;
                    }

                    u32 props = 0;

                    if (auto room = island.get_room({x, y})) {
                        props = (*room->metaclass())->properties();

                        if (room->cast<Portal>()) {
                            for (auto& r : island.rooms()) {
                                if (r->cast<Portal>()) {
                                    if (rng::choice<3>(rng::critical_state) ==
                                        0) {
                                        island.fire_create(r->position());
                                    }
                                }
                            }
                        }
                    }

                    if (not(*mat)[x][y] and
                        not(props & RoomProperties::highly_flammable)) {
                        island.fire_extinguish({x, y});
                    }

                    auto try_spread = [&](u8 x, u8 y) {
                        if (auto room = island.get_room({x, y})) {
                            auto props = (*room->metaclass())->properties();
                            if (props & RoomProperties::fireproof or
                                (not(*mat)[x][y] and
                                 not(props &
                                     RoomProperties::highly_flammable))) {
                                return;
                            }
                        } else if (not(*mat)[x][y]) {
                            island.fire_extinguish({x, y});
                            if (is_player_island(&island) or
                                not player_island().fire_texture()) {
                                minimap::schedule_repaint();
                            }
                            return;
                        }
                        if (not old_positions.get(x, y)) {
                            island.fire_create({x, y});
                            if (is_player_island(&island) or
                                not player_island().fire_texture()) {
                                minimap::schedule_repaint();
                            }
                        }
                    };

                    if (x > 0) {
                        try_spread(x - 1, y);
                    }

                    if (x < 15) {
                        try_spread(x + 1, y);
                    }

                    if (y > 0) {
                        try_spread(x, y - 1);

                        if (x > 0) {
                            try_spread(x - 1, y - 1);
                        }
                        if (x < 15) {
                            try_spread(x + 1, y - 1);
                        }
                    }

                    if (y < 15) {
                        try_spread(x, y + 1);

                        if (x > 0) {
                            try_spread(x - 1, y + 1);
                        }
                        if (x < 15) {
                            try_spread(x + 1, y + 1);
                        }
                    }
                }
            }
        }
    }

    if (damage_timer_ > seconds(2)) {

        damage_timer_ -= seconds(2);

        bool fire_present = false;

        Buffer<RoomCoord, 32> spread_queue;

        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (positions_.get(x, y)) {
                    fire_present = true;
                    if (auto room = island.get_room({x, y})) {
                        if ((*room->metaclass())->properties() &
                            RoomProperties::fireproof) {
                            island.fire_extinguish({x, y});
                        } else {
                            room->burn_damage(2);

                            auto try_spread = [&](u8 x, u8 y) {
                                if (island.fire_present({x, y})) {
                                    return;
                                }
                                auto room = island.get_room({x, y});
                                if (room) {
                                    auto props =
                                        (*room->metaclass())->properties();
                                    if (props &
                                        RoomProperties::highly_flammable) {
                                        spread_queue.push_back({x, y});
                                    }
                                }
                            };

                            // If fire just destroyed a room, and there are
                            // un-lit highly-flammable rooms nearby, try to
                            // spread.
                            if (room->health() == 0) {
                                if (x > 0) {
                                    try_spread(x - 1, y);
                                }

                                if (x < 15) {
                                    try_spread(x + 1, y);
                                }

                                if (y > 0) {
                                    try_spread(x, y - 1);
                                }

                                if (y < 15) {
                                    try_spread(x, y + 1);
                                }
                            }
                        }
                    } else {
                        island.fire_extinguish({x, y});
                    }
                }
            }
        }

        for (auto& c : spread_queue) {
            island.fire_create(c);
        }

        if (not fire_present) {
            spread_timer_ = 0;
        }

        if (fire_present and not texture_) {

            texture_ = fire_alloc_texture(island);

        } else if (not fire_present) {
            texture_.reset();
        }
    }

    anim_timer_ += delta;
    if (anim_timer_ > milliseconds(100)) {
        anim_timer_ -= milliseconds(100);

        ++anim_index_;
        if (anim_index_ == 6) {
            anim_index_ = 0;
        }

        if (texture_ and
            // We only want to maintain a single reference to the fire texture,
            // to save vram. Both islands share a texture for the fire effect,
            // and only one island updates the tile glyph.
            (texture_->strong_count() == 1 or is_player_island(&island))) {
            (*texture_)->remap(154 + anim_index_);
        }
    }
}



void Island::FireState::display(Island& island)
{
    if (not texture_) {
        return;
    }

    auto o = ivec(island.visual_origin());

    auto batch = allocate_dynamic<Buffer<Vec2<s32>, 64>>("fire-spr-buffer");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            if (positions_.get(x, y)) {
                batch->push_back({o.x + x * 16, o.y + y * 16 - 16});
            }
        }
    }

    PLATFORM.screen().draw_batch((*texture_)->mapping_index() * 2, *batch);
}



void Island::update_simple(Time dt)
{
    timer_ += dt;

    if (not hidden_ and show_flag_ and flag_pos_) {
        flag_anim_timer_ += dt;
        if (flag_anim_timer_ > milliseconds(80)) {
            flag_anim_timer_ = 0;
            auto current = flag_anim_index_;
            if (current < Tile::flag_end) {
                ++flag_anim_index_;
            } else {
                flag_anim_index_ = Tile::flag_start;
            }
        }
    }

    if (not hidden_ and chimney_loc_) {
        chimney_spawn_timer_ += dt;

        if (chimney_spawn_timer_ > milliseconds(600)) {
            chimney_spawn_timer_ = 0;

            auto o = origin();

            o.x += Fixnum::from_integer(chimney_loc_->x * 16 + 8);
            o.y += Fixnum::from_integer(chimney_loc_->y * 16 - 4);

            if (auto e = APP.alloc_entity<SmokePuff>(o, smoke_sprite())) {
                APP.effects().push(std::move(e));
            }
        }
    }

    s8 ambient_offset = 4 * float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
                        std::numeric_limits<s16>::max();

    if (mountain_terrain_) {
        ambient_offset = 0;
    }

    ambient_movement_ = ambient_offset;

    PLATFORM.set_scroll(layer(),
                        -get_position().x.as_integer(),
                        -get_position().y.as_integer() - ambient_offset);
}



lisp::Value* wrap_island(Island* isle);



void Island::update(Time dt)
{
    update_simple(dt);

    if (should_recompute_deflector_shields_) {
        should_recompute_deflector_shields_ = false;
        recompute_deflector_shields();
    }

    const bool movement_ready = all_characters_awaiting_movement_;
    all_characters_awaiting_movement_ = true;

    is_boarded_ = false;


    auto on_character_died = [&](Character& c) {
        if (not PLATFORM.network_peer().is_connected() and
            c.owner() == &APP.player()) {

            auto fn = lisp::get_var("on-crew-died");
            if (fn->type() == lisp::Value::Type::function) {
                lisp::push_op(lisp::make_integer(c.id()));
                lisp::safecall(fn, 1);
                lisp::pop_op(); // result
            }
        }
        c.finalize();
        time_stream::event::CharacterDied e;
        e.x_ = c.grid_position().x;
        e.y_ = c.grid_position().y;
        e.id_.set(c.id());
        e.owned_by_player_ = c.owner() == &APP.player();
        e.near_ = is_player_island(this);
        e.race_ = (int)c.get_race();
        e.is_replicant_ = c.is_replicant();
        e.icon_ = c.get_icon();
        e.max_health_ = c.get_max_health();
        e.health_ = c.health();
        e.stats_ = c.stats().info_;
        APP.push_time_stream(e);
    };


    character_count_ = 0;

    auto update_characters = [&](Room* room, auto& chr_list, bool exterior) {
        for (auto it = chr_list.begin(); it not_eq chr_list.end();) {
            if (not(*it)->alive()) {

                network::packet::ChrDiedV2 packet;
                packet.chr_id_.set((*it)->id());
                packet.near_island_ = this not_eq &APP.player_island();
                network::transmit(packet);

                on_character_died(**it);

                const auto pos = (*it)->sprite().get_position();
                if (auto e = alloc_entity<Ghost>(pos)) {
                    APP.effects().push(std::move(e));
                }

                if (room) {
                    room->update_description();
                }

                it = chr_list.erase(it);
            } else {

                if ((*it)->is_awaiting_movement()) {
                    if (movement_ready) {
                        (*it)->set_can_move();
                    }
                } else {
                    all_characters_awaiting_movement_ = false;
                }
                if ((*it)->owner() not_eq &owner()) {
                    if (not exterior) {
                        is_boarded_ = true;
                    }
                } else {
                    ++character_count_;
                }

                (*it)->update(dt, room);
                ++it;
            }
        }
    };


    static constexpr const auto sync_delay = milliseconds(100);


    for (auto it = drones_.begin(); it not_eq drones_.end();) {
        if (not(*it)->alive()) {

            auto sync = [x = (*it)->position().x,
                         y = (*it)->position().y,
                         near = is_player_island((*it)->destination())]() {
                network::packet::DroneDestroyed destroyed;
                destroyed.drone_x_ = x;
                destroyed.drone_y_ = y;
                destroyed.destination_near_ = near;

                network::transmit(destroyed);
            };

            if (PLATFORM.network_peer().is_connected()) {
                if (not APP.on_timeout(sync_delay, sync)) {
                    sync();
                }
            }

            medium_explosion((*it)->sprite().get_position());

            minimap::schedule_repaint();
            ExploSpawner::create((*it)->sprite().get_position());
            it = drones_.erase(it);
        } else {
            (*it)->update(dt);
            ++it;
        }
    }



    // Before I added the ArcGun weapon, which chains to multiple rooms, there
    // was no reason to do any optimizations on certain operations, because only
    // one room would be destroyed per frame, except in truely rare occasions.
    int destroyed_count = 0;
    bool core_destroyed = false;


    bool do_repaint = false;
    if (schedule_repaint_) {
        schedule_repaint_ = false;
        do_repaint = true;
    }


    bulk_timer_.update(dt);


    resolve_cancelled_dispatch();


    Room* last_drawfirst_ = drawfirst_;


    Room* room = dispatch_list_;
    dispatch_list_ = nullptr;
    drawfirst_ = nullptr;


    while (room) {

        const auto next = room->dispatch_next();

        if (room->health() == 0) {

            schedule_recompute_deflector_shields();

            auto props = (*room->metaclass())->properties();

            const bool quiet = props & RoomProperties::destroy_quietly;
            const bool big_explo =
                (props & RoomProperties::oversize_explosion) or
                (props & RoomProperties::habitable);


            if (++destroyed_count < 5 and not quiet) {
                // Five rooms destroyed on the same island in the same frame! If
                // we create tons of huge explosions all at once, we'll lag the
                // game and use lots of entities.
                big_explosion(room->center(),
                              BigExplosionConfig{.centerflash_ = big_explo});
            }

            if (str_eq(room->name(), "power-core") or
                str_eq(room->name(), "reactor") or
                str_eq(room->name(), "war-engine")) {
                core_destroyed = true;
            }

            if (destroyed_count < 2 and
                APP.game_mode() not_eq App::GameMode::multiplayer and
                APP.game_mode() not_eq App::GameMode::co_op) {
                PLATFORM.sleep(2);
            }

            const auto pos = room->position();

            auto mt = room->metaclass_index();

            auto sync = [x = pos.x,
                         y = pos.y,
                         near = &owner() not_eq &APP.player(),
                         mt]() {
                network::packet::RoomDestroyed packet;
                packet.room_x_ = x;
                packet.room_y_ = y;
                packet.near_island_ = near;
                packet.metaclass_index_.set(mt);
                network::transmit(packet);
            };

            for (auto& chr : room->characters()) {
                // The room was destroyed, along with any inhabitants.
                on_character_died(*chr);
            }


            // Running a callback for each room destroyed and then resetting the
            // delta clock would ruin multiplayer sync. We have no need to
            // register an on-room-destroyed callback in multiplayer anyway...
            if (APP.game_mode() not_eq App::GameMode::multiplayer and
                APP.game_mode() not_eq App::GameMode::co_op and
                not str_eq(room->name(), "mycelium")) {

                // This is quite expensive! But it's convenient to be able to be
                // able to register a callback when a room's destroyed.
                auto fn = lisp::get_var("on-room-destroyed");
                if (fn->type() == lisp::Value::Type::function) {
                    // NOTE: fn is in a global var, as we accessed it through
                    // get_var. So there's no need to protect fn from the gc, as
                    // it's already attached to a gc root.
                    lisp::push_op(wrap_island(this));
                    lisp::push_op(lisp::make_symbol(room->name()));
                    lisp::push_op(lisp::make_integer(room->position().x));
                    lisp::push_op(lisp::make_integer(room->position().y));
                    lisp::safecall(fn, 4);
                    lisp::pop_op(); // result

                    // We cannot know how much latency that the custom script
                    // added. i.e. reset the clock in case some scripted code
                    // spent too long executing custom lisp code. Otherwise
                    // collision checking and other stuff could get messed up!
                    PLATFORM.delta_clock().reset();
                } else if (destroyed_count == 0) {
                    // No pauses during multiplayer, of course.
                    if (not PLATFORM.network_peer().is_connected()) {
                        // Sleep a few frames when a block is destroyed.
                        if (is_player_island(this)) {
                            PLATFORM.sleep(4);
                        } else {
                            PLATFORM.sleep(2);
                        }
                    }
                }
            }

            const auto group = room->group();

            room->finalize();

            if (&owner() == &APP.player()) {
                if (room->is_powered_down()) {
                    // To trigger the recording of the room's powered-down state
                    // in rewinding logic.
                    room->set_powerdown(false);
                }
                if (group not_eq Room::Group::none) {
                    time_stream::event::PlayerRoomDestroyedWithGroup p;
                    p.x_ = pos.x;
                    p.y_ = pos.y;
                    p.type_ = mt;
                    p.group_ = (u8)group;
                    APP.push_time_stream(p);
                } else {
                    time_stream::event::PlayerRoomDestroyed p;
                    p.x_ = pos.x;
                    p.y_ = pos.y;
                    p.type_ = mt;
                    APP.push_time_stream(p);
                }
            } else {
                time_stream::event::OpponentRoomDestroyed p;
                p.x_ = pos.x;
                p.y_ = pos.y;
                p.type_ = mt;
                APP.push_time_stream(p);
            }


            if (destroyed_count < 2 and not quiet and
                // Core and Reactor play a loud sound on their own, playing an
                // explosion simultaneously causes clipping on some consoles.
                not core_destroyed) {
                PLATFORM.speaker().play_sound("explosion1", 2);
            }

            if (PLATFORM.network_peer().is_connected()) {
                if (not APP.on_timeout(sync_delay, sync)) {
                    // Explanation: we don't want to transmit the room destroyed
                    // packet right away. We only transmit room destroyed
                    // packets in the first place to keep games in sync. If one
                    // game happens to be a frame or two ahead of another game,
                    // we might end up destroying a room before it is hit by a
                    // projectile that it was already hit by in the other game,
                    // in which case, the projectile might keep going and
                    // destroy an additional room, appearing to the other player
                    // as though the projectile did twice as much damage as it
                    // should have. So, instead, we attempt to enqueue a
                    // deferred callback, which transmits the RoomDestroyed
                    // packet at some point a frame or two in the future, at
                    // which point, both games should already be synchronized,
                    // and there is in fact nothing to destroy. If we fail to
                    // enqueue the deferred callback for some reason, invoke
                    // immediately. I want to stress: we only transmit the
                    // room-destroyed packet as a last resort, in case the games
                    // somehow got out of sync. This packet should in almost all
                    // cases be meaningless to the other game.
                    sync();
                }
            }


            APP.player().on_room_destroyed(*room);

            [&] {
                for (auto it = rooms_.begin(); it not_eq rooms_.end(); ++it) {
                    if (it->get() == room) {
                        rooms_.erase(it);
                        return;
                    }
                }
                Platform::fatal("dispatch list erase room not in roomsTable");
            }();

            owner().rooms_lost_++;

            on_layout_changed(pos);

            check_destroyed();

            if (is_destroyed() and not PLATFORM.network_peer().is_connected()) {
                PLATFORM.sleep(3);
            }

            recalculate_power_usage();

            do_repaint = true;

            update_target_queues();

        } else {
            if (dt not_eq 0) {
                // Do not update a room if the game is stopped.
                room->update(dt);
            } else {
                // If a room was ready, and we didn't update it, then it's still
                // ready.
                dispatch_room(room);

                if (room == last_drawfirst_) {
                    drawfirst_ = room;
                }
            }

            update_characters(room, room->edit_characters(), false);
        }

        room = next;
    }

    if (do_repaint) {
        repaint();
        schedule_repaint_partial_ = false;
    } else if (schedule_repaint_partial_) {
        schedule_repaint_partial_ = false;
        repaint_partial();
    }


    update_characters(nullptr, characters_, true);


    update_entities(dt, projectiles_);


    if (drift_ not_eq 0.0_fixed) {
        position_.x += drift_ * Fixnum::from_integer(dt);
    }

    fire_.update(*this, dt);
}



void Island::update_target_queues()
{
    for (auto& r : APP.player_island().rooms()) {
        if (auto w = r->cast_weapon()) {
            w->update_targets();
        }
    }

    for (auto& drone : APP.player_island().drones()) {
        drone->update_targets();
    }

    APP.with_opponent_island([](auto& isle) {
        for (auto& drone : isle.drones()) {
            drone->update_targets();
        }
    });
}



void Island::schedule_recompute_deflector_shields()
{
    should_recompute_deflector_shields_ = true;
}



u16 Island::script_userdata_tag() const
{
    if (is_player_island(this)) {
        return 1;
    } else {
        return 2;
    }
}



void Island::on_layout_changed(const RoomCoord& room_added_removed_coord)
{
    check_destroyed();

    bool buffer[16][16];

    checksum_ = 0;
    dark_smoke_ = false;

    for (auto& room : rooms()) {
        if (room->metaclass() == chaos_core_mt) {
            dark_smoke_ = true;
        }
        for (auto& chr : room->characters()) {
            if (auto path = chr->get_movement_path()) {
                plot_walkable_zones(buffer, chr.get());
                int pos = 0;
                for (auto& node : *path) {
                    if (get_room(node) == nullptr or
                        not buffer[node.x][node.y]) {
                        // Ok, a character is moving, but the room in the
                        // character's movement path no longer exists. We want
                        // to crop the path after the nonexistent point.
                        ++pos;
                        while (pos) {
                            --pos;
                            path->erase(path->begin());
                        }
                        break; // NOTE: we just deallocated the path, so we
                               // cannot run the enclosing loop over the path
                               // nodes anymore.
                    } else {
                        ++pos;
                    }
                }
            }
        }

        // NOTE: the checksum must incorporate room type as well as position,
        // otherwise, in some scenarios, the checksum may be the same after a
        // block is removed. Checksums can still collide, of course.
        checksum_ +=
            room->metaclass_index() + room->position().y + room->position().x;
    }
}



static constexpr const int screen_limit_y = 700;



void Island::display()
{
    if (hidden_) {
        return;
    }

    if (flag_pos_ and show_flag_) {

        Optional<u16> palette;

        // NOTE: the player can design his/her own flag, so we reserve a
        // specific palette bank just for the flag image. Untimately, doing so
        // simplifies things. The opponent island doesn't necessarily need the
        // custom flag palette, but why not keep things simple and consistent...
        palette = 12;

        PLATFORM.set_tile(
            layer_, flag_pos_->x, flag_pos_->y, flag_anim_index_, palette);
    }

    for (auto& c : characters_) {
        // The interior floor is two pixels thick. But our character is now
        // standing outside, where there's no floor, so we need to shift the
        // character down by two pixels.
        auto pos = c->sprite().get_position();
        if (pos.y.as_integer() < screen_limit_y) {
            Character::DrawTransform t;
            t.y_displace_ = 2.0_fixed;
            c->draw(PLATFORM.screen(), t);
        }
    }

    for (auto& p : projectiles_) {
        PLATFORM.screen().draw(p->sprite());
    }


    resolve_cancelled_dispatch();

    if (drawfirst_) {
        drawfirst_->display(PLATFORM.screen());
    }

    Room* room = dispatch_list_;
    while (room) {
        if (room not_eq drawfirst_) {
            room->display(PLATFORM.screen());
        }
        room = room->dispatch_next();
    }
}



void Island::display_fires()
{
    fire_.display(*this);
}



HitBox Island::hitbox() const
{
    Vec2<Fixnum> hitbox_pos = this->origin();
    HitBox island_hitbox;
    island_hitbox.position_ = &hitbox_pos;
    island_hitbox.dimension_.size_.x = terrain_.size() * 16;
    island_hitbox.dimension_.size_.y = 16 * 16;
    return island_hitbox;
}



void Island::test_collision(Entity& entity)
{
    if (phase_ == 1) {
        return;
    }

    Vec2<Fixnum> hitbox_pos = this->origin();
    HitBox island_hitbox;
    island_hitbox.position_ = &hitbox_pos;
    island_hitbox.dimension_.size_.x = terrain_.size() * 16;
    island_hitbox.dimension_.size_.y = 16 * 16;

    if (not island_hitbox.overlapping(entity.hitbox())) {
        return;
    }

    // Calculate the position of the entity in terms of the island's grid
    // coordinates.
    auto entity_pos = ivec((entity.sprite().get_position() - this->origin()));
    entity_pos.x /= 16;
    entity_pos.y /= 16;

    for (int x = entity_pos.x - 1; x < entity_pos.x + 2; ++x) {
        for (int y = entity_pos.y - 1; y < entity_pos.y + 2; ++y) {
            if (x < 0 or y < 0 or x > 15 or y > 15) {
                continue;
            }

            if (rooms_plot_.get(x, y)) {
                if (auto room = get_room({(u8)x, (u8)y})) {
                    static constexpr const int tile_size = 16;

                    auto hitbox_pos = this->origin();
                    hitbox_pos.x +=
                        Fixnum::from_integer(room->position().x * tile_size);

                    hitbox_pos.y +=
                        Fixnum::from_integer(room->position().y * tile_size);

                    HitBox room_hitbox;
                    room_hitbox.position_ = &hitbox_pos;
                    room_hitbox.dimension_.size_.x = room->size().x * tile_size;
                    room_hitbox.dimension_.size_.y = room->size().y * tile_size;

                    if (room_hitbox.overlapping(entity.hitbox())) {
                        entity.on_collision(*room, Vec2<u8>{(u8)x, (u8)y});
                        return;
                    }
                }
            }
        }
    }

    for (auto& drone_sp : drones_) {
        if (entity.hitbox().overlapping((drone_sp)->hitbox())) {
            entity.on_collision(*drone_sp);
            (drone_sp)->on_collision(entity);
        }
    }
}



void show_phase()
{
    Buffer<Layer, 4> translucent_layers;
    if (APP.player_island().phase()) {
        translucent_layers.push_back(APP.player_island().layer());
    }

    APP.with_opponent_island([&](auto& isle) {
        if (isle.phase()) {
            translucent_layers.push_back(isle.layer());
        }
    });

    PLATFORM_EXTENSION(enable_translucence, translucent_layers);
}



void hide_translucence()
{
    Buffer<Layer, 4> translucent_layers;
    PLATFORM_EXTENSION(enable_translucence, translucent_layers);
}



void Island::set_phase(u8 phase)
{
    if (phase_ not_eq phase) {
        time_stream::event::IslePhaseChange e;
        e.prev_phase_ = phase_;
        e.near_ = is_player_island(this);
        APP.push_time_stream(e);
    }

    phase_ = phase;

    for (auto& r : rooms()) {
        for (auto& chr : r->characters()) {
            chr->set_phase(phase);
        }
    }

    if (APP.game_speed() not_eq GameSpeed::rewind) {
        for (auto& bird : APP.birds()) {
            if (bird->island() == this) {
                bird->signal();
            }
        }
    }

    show_phase();
}



u8 Island::phase() const
{
    return phase_;
}



void Island::set_mountain_terrain(bool enabled)
{
    mountain_terrain_ = enabled;
}



void Island::render_terrain()
{
    auto load_tile = [&](u16 t) {
        return layer_ == Layer::map_0_ext ? PLATFORM.map_tile0_chunk(t)
                                          : PLATFORM.map_tile1_chunk(t);
    };

    if (mountain_terrain_) {
        auto sl = load_tile(Tile::terrain_slope_left);
        auto ll = load_tile(Tile::terrain_ledge_left);
        auto sr = load_tile(Tile::terrain_slope_right);
        auto lr = load_tile(Tile::terrain_ledge_right);
        auto cn = load_tile(Tile::terrain_center);
        auto mt = load_tile(Tile::terrain_mountain_top);
        auto mb = load_tile(Tile::terrain_mountain_bottom);

        for (u32 i = 0; i < terrain_.size(); ++i) {
            auto fill = cn;
            if (i == 0) {
                PLATFORM.set_tile(layer_, i, 15, sl);
                fill = ll;
            } else if (i == terrain_.size() - 1) {
                PLATFORM.set_tile(layer_, i, 15, sr);
                fill = lr;
            } else {
                PLATFORM.set_tile(layer_, i, 15, mt);
            }

            PLATFORM.set_tile(layer_, i, 0, fill);
            PLATFORM.set_tile(layer_, i, 1, fill);
            PLATFORM.set_tile(layer_, i, 2, fill);
            PLATFORM.set_tile(layer_, i, 3, mb);
        }

        return;
    }

    for (u32 i = 0; i < terrain_.size(); ++i) {
        auto tile_handle = layer_ == Layer::map_0_ext
                               ? PLATFORM.map_tile0_chunk(terrain_[i])
                               : PLATFORM.map_tile1_chunk(terrain_[i]);

        PLATFORM.set_tile(layer_, i, 15, tile_handle);
    }

    for (int i = terrain_.size(); i < 16; ++i) {
        PLATFORM.set_tile(layer_, i, 15, 0);
    }
}



const Vec2<Fixnum>& Island::get_position() const
{
    return position_;
}



void Island::set_position(const Vec2<Fixnum>& position)
{
    position_ = position;
}



void Island::render()
{
    if (interior_visible_) {
        render_interior();
    } else {
        render_exterior();
    }
}



void Island::render_interior_fast()
{
    if (interior_visible_) {
        repaint();
        return;
    }

    render_interior();
}



void set_glow_color();



void Island::render_interior()
{
    interior_visible_ = true;

    if (layer_ == Layer::map_0_ext) {
        auto t = APP.environment().player_island_interior_texture();
        PLATFORM.load_tile0_texture(t);
        set_glow_color();
    } else {
        auto t = APP.environment().opponent_island_interior_texture();
        PLATFORM.load_tile1_texture(t);
    }

    auto canvas_mti = metaclass_index("canvas");
    for (auto& room : rooms()) {
        if (room->metaclass_index() == canvas_mti) {
            if (auto c = room->cast<Canvas>()) {
                c->publish_tiles();
            }
        }
    }

    // When rendering the interior/exterior of a castle, we've swapped the
    // tileset texture, so all tiles mapped into memory need to be discarded,
    // allowing the repaint() code to insert new mappings into vram.
    layer_ == Layer::map_0_ext ? PLATFORM.clear_tile0_mappings()
                               : PLATFORM.clear_tile1_mappings();

    repaint();
}



void Island::render_exterior()
{
    interior_visible_ = false;

    if (layer_ == Layer::map_0_ext) {
        PLATFORM.load_tile0_texture(APP.environment().player_island_texture());
        set_glow_color();
    } else {
        PLATFORM.load_tile1_texture(
            APP.environment().opponent_island_texture());
    }

    auto canvas_mti = metaclass_index("canvas");
    for (auto& room : rooms()) {
        if (room->metaclass_index() == canvas_mti) {
            if (auto c = room->cast<Canvas>()) {
                c->publish_tiles();
            }
        }
    }

    layer_ == Layer::map_0_ext ? PLATFORM.clear_tile0_mappings()
                               : PLATFORM.clear_tile1_mappings();

    repaint();
}



void Island::plot_rooms(u8 matrix[16][16]) const
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            matrix[x][y] = 0;
        }
    }

    for (auto& room : rooms_) {
        auto pos = room->position();
        auto sz = room->size();

        int val = 2;
        if (not((*room->metaclass())->properties() &
                RoomProperties::roof_hidden)) {
            val = 1;
        } else if ((*room->metaclass())->properties() &
                   RoomProperties::disallow_chimney) {
            val = 3;
        }

        for (int x = 0; x < sz.x; ++x) {
            for (int y = 0; y < sz.y; ++y) {
                matrix[x + pos.x][y + pos.y] = val;
            }
        }
    }
}


void Island::recalculate_power_usage()
{
    power_supply_ = 0;
    power_drain_ = 0;

    for (auto& room : rooms_) {
        auto power = room->power_usage();

        if (power < 0) {
            power_supply_ += -power;
        } else {
            power_drain_ += power;
        }
    }
}



bool Island::add_character(EntityRef<Character> character)
{
    if (auto room = get_room(character->grid_position())) {
        return room->add_occupant(std::move(character));
    } else {
        characters_.push(std::move(character));
        return true;
    }
}



void Island::move_room(const RoomCoord& from, const RoomCoord& to)
{
    for (auto it = rooms_.begin(); it not_eq rooms_.end(); ++it) {
        if (not(*it)->hidden() and (*it)->position() == from) {
            auto room = std::move(*it);
            it = rooms_.erase(it);

            room->__set_position(to);
            const auto sz = room->size();

            const int x_off = to.x - from.x;
            const int y_off = to.y - from.y;

            for (auto& chr : room->characters()) {
                auto p = chr->grid_position();
                p.x = p.x + x_off;
                p.y = p.y + y_off;

                chr->set_grid_position(p);
                chr->drop_movement_path();
            }

            if (APP.game_speed() == GameSpeed::stopped and
                not room->ai_aware()) {
                // The room is cloaked, and we're moving it. We want to update
                // visibility in case the room was moved out of the range of a
                // cloaking field.
                room->init_ai_awareness_upon_unpause();
            }

            rooms_.insert_room(std::move(room));

            recalculate_power_usage();
            unamplify_blocks();
            on_layout_changed(from);
            schedule_recompute_deflector_shields();

            schedule_repaint_ = true;

            if (is_player_island(this)) {
                time_stream::event::PlayerRoomMoved e;
                e.x_ = to.x;
                e.y_ = to.y;
                e.prev_x_ = from.x;
                e.prev_y_ = from.y;
                APP.push_time_stream(e);
            } else {
                time_stream::event::OpponentRoomMoved e;
                e.x_ = to.x;
                e.y_ = to.y;
                e.prev_x_ = from.x;
                e.prev_y_ = from.y;
                APP.push_time_stream(e);
            }

            Buffer<Vec2<u8>, 16> fire_respawn_locs;
            for (u8 x = 0; x < sz.x; ++x) {
                for (u8 y = 0; y < sz.y; ++y) {
                    u8 ox = x + from.x;
                    u8 oy = y + from.y;
                    u8 tx = x + to.x;
                    u8 ty = y + to.y;
                    if (fire_present({ox, oy})) {
                        fire_extinguish({ox, oy});
                        fire_respawn_locs.push_back({tx, ty});
                    }
                }
            }
            for (auto& l : fire_respawn_locs) {
                fire_create({l.x, l.y});
            }

            for (auto& r : rooms()) {
                r->parent_layout_changed(from, to);
            }
        }
    }
}



void Island::plot_walkable_zones(bool matrix[16][16],
                                 Character* for_character) const
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            matrix[x][y] = 0;
        }
    }

    // TODO: label outdoor grass areas as walkable.

    for (auto& room : rooms_) {
        auto props = (*room->metaclass())->properties();

        if (props & RoomProperties::habitable) {
            room->plot_walkable_zones(matrix, for_character);
        }
    }
}



// I suppose this function needs to be called whenever a block is destroyed,
// moved, or created...
void Island::recompute_deflector_shields()
{
    for (auto& room : rooms()) {
        room->set_shielded(false);
    }

    for (auto& drone : drones_) {
        drone->set_shielded(false);
    }

    for (auto& room : rooms()) {
        room->project_deflector_shield();
    }
}



void Island::plot_construction_zones(bool matrix[16][16]) const
{
    u8 matrix_temp[16][16];

    plot_rooms(matrix_temp);

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            matrix[x][y] =
                matrix_temp[x][y] == 0 and y < 15 and matrix_temp[x][y + 1];
        }
    }

    for (int x = 0; x < 16; ++x) {
        matrix[x][15] = false;
    }

    for (u32 x = 0; x < terrain_.size(); ++x) {
        if (matrix_temp[x][14] == 0) {
            matrix[x][14] = true;
        }
    }

    for (auto& sp : drones_) {
        auto pos = sp->position();
        matrix[pos.x][pos.y] = false;
        matrix[pos.x][pos.y + 1] = false;
    }
}



bool Island::repaint_alloc_tiles(TileId buffer[16][16], bool retry)
{
    for (int x = 0; x < 16; ++x) {
        // NOTE: only handle 15 rows because render_terrain() takes care of the
        // last row.
        for (int y = 0; y < 15; ++y) {

            if (buffer[x][y] >= Tile::canvas_tiles_begin and
                buffer[x][y] < Tile::canvas_tiles_begin + 15) {
                PLATFORM.set_tile(layer_, x, y, buffer[x][y]);
                // PLATFORM.set_palette(layer_, x, y, 12);
                continue;
            }

            auto tile_handle = layer_ == Layer::map_0_ext
                                   ? PLATFORM.map_tile0_chunk(buffer[x][y])
                                   : PLATFORM.map_tile1_chunk(buffer[x][y]);

            if (min_y_ == 0 and tile_handle) {
                min_y_ = y;
            }

            if (tile_handle == 112 and not retry) {

                // We ran out of vram for storing tiles! Clear out all mapped
                // tiles, and attempt to reconstruct. A bit of a lazy
                // brute-force solution. Alternatively, we could attempt to
                // automatically clean up tile mappings when a room's destroyed,
                // but then, all tiles would need to be reference-counted, and
                // running out of tiles is a rare edge-case, so it's not worth
                // optimizing at the moment.

                layer_ == Layer::map_0_ext ? PLATFORM.clear_tile0_mappings()
                                           : PLATFORM.clear_tile1_mappings();

                return false;
            }

            PLATFORM.set_tile(layer_, x, y, tile_handle);
        }
    }
    return true;
}



void Island::repaint_partial()
{
    struct Memory
    {
        TileId tiles[16][16];
    };

    auto mem = allocate_dynamic<Memory>("repaint partial buffer");

    for (u32 x = 0; x < terrain_.size(); ++x) {
        for (int y = 0; y < 16; ++y) {
            mem->tiles[x][y] = 0;
        }
    }

    for (auto& r : rooms_) {
        if (r->poll_repaint()) {
            if (interior_visible_) {
                r->render_interior(&APP, mem->tiles);
            } else {
                r->render_exterior(&APP, mem->tiles);
            }
        }
    }

    for (auto& r : rooms_) {
        r->render_cloak(mem->tiles);
    }

    for (u32 x = 0; x < terrain_.size(); ++x) {
        for (u32 y = 0; y < 15; ++y) {
            if (mem->tiles[x][y] not_eq 0) {
                auto tile_handle =
                    layer_ == Layer::map_0_ext
                        ? PLATFORM.map_tile0_chunk(mem->tiles[x][y])
                        : PLATFORM.map_tile1_chunk(mem->tiles[x][y]);

                if (min_y_ == 0 and tile_handle) {
                    min_y_ = y;
                }

                PLATFORM.set_tile(layer_, x, y, tile_handle);
            }
        }
    }

    for (auto& room : rooms_) {
        if (room->is_powered_down()) {

            // Optional<u16> pal = 9;
            auto p = room->position();
            for (int x = 0; x < room->size().x; ++x) {
                for (int y = 0; y < room->size().y; ++y) {
                    PLATFORM.set_palette(Layer::map_0_ext, p.x + x, p.y + y, 9);
                }
            }

            const auto tile = (6 * 4 - 1) + 4;
            if (layer_ == Layer::map_0_ext) {
                auto [x, y] = room->position();
                PLATFORM.set_raw_tile(Layer::map_0, x * 2, y * 2, tile);
            }
        } else if (show_powerdown_opts_ and room->allows_powerdown()) {
            const auto tile = (6 * 4 - 1) + 4 + 5;
            if (layer_ == Layer::map_0_ext) {
                auto [x, y] = room->position();
                PLATFORM.set_raw_tile(Layer::map_0, x * 2, y * 2, tile);
            }
        }
    }
}



void Island::repaint()
{
    if (hidden_) {
        return;
    }

    set_glow_color();

    // The engine only knows how to draw an island wholistically, because some
    // tiles need to be joined etc., so whenever the island changes, the whole
    // things needs to be repainted. I could try to only re-render the changed
    // parts, doing so would be quite complex, as the positions of the flag,
    // chimney, roof tiles, etc. all sort of depend on the shape of the island
    // as a whole. Anyway, this function performs all of the tile rendering.

    struct Memory
    {
        u8 mat[16][16];
        TileId tiles[16][16];
    };

    auto mem = allocate_dynamic<Memory>("repaint-ctx");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            mem->tiles[x][y] = 0;
        }
    }

    min_y_ = 0;

    flag_pos_.reset();

    if (interior_visible_) {
        for (auto& room : rooms()) {
            room->render_interior(&APP, mem->tiles);
        }
    } else {
        for (auto& room : rooms()) {
            room->render_exterior(&APP, mem->tiles);
        }
    }

    for (auto& room : rooms()) {
        room->render_scaffolding(mem->tiles);
    }

    plot_rooms(mem->mat);

    Buffer<RoomCoord, decltype(terrain_)::capacity()> chimney_locs;

    has_radar_ = false;
    core_count_ = 0;
    offensive_capabilities_ = 0;
    for (auto& room : rooms_) {
        if (room->is_powered_down()) {
            continue;
        }
        if ((*room->metaclass())->properties() & RoomProperties::has_chimney) {
            chimney_locs.push_back(room->position());
        }
        auto metac = room->metaclass();
        if (str_cmp((*metac)->name(), "radar") == 0) {
            has_radar_ = true;
        } else if ((*room->metaclass())->category() == Room::Category::power) {
            critical_core_x_ = room->position().x;
            critical_core_y_ = room->position().y;
            ++core_count_;
        } else if ((*metac)->category() == Room::Category::weapon or
                   (character_count_ and
                    str_eq((*metac)->name(), "transporter")) or
                   str_eq((*metac)->name(), "drone-bay")) {
            ++offensive_capabilities_;
        }
    }

    std::sort(chimney_locs.begin(),
              chimney_locs.end(),
              [&](const auto& lhs, const auto& rhs) { return lhs.y < rhs.y; });

    chimney_loc_.reset();

    bool placed_flag = false;
    bool placed_chimney = false;

    rooms_plot_.clear();

    for (u8 x = 0; x < 16; ++x) {
        for (int y = 15; y > -1; --y) {

            rooms_plot_.set(x, y, mem->mat[x][y]);

            if (mem->mat[x][y] == 0 and y < 15 and mem->mat[x][y + 1] == 1) {
                bool block_chimney = false;
                if (mem->tiles[x][y] == Tile::strut) {
                    block_chimney = true;
                    mem->tiles[x][y] = Tile::roof_strut;
                } else if (mem->tiles[x][y] == Tile::strut_top) {
                    block_chimney = true;
                    mem->tiles[x][y] = Tile::roof_strut_joined;
                } else if (mem->tiles[x][y] == Tile::scaffolding_angled_l or
                           mem->tiles[x][y] == Tile::scaffolding_angled_r) {
                    block_chimney = true;
                    mem->tiles[x][y] = Tile::roof_strut_joined;
                } else {
                    mem->tiles[x][y] = Tile::roof_plain;
                }

                bool placed_chimney_this_tile = false;
                if (not block_chimney and not placed_chimney and y > 5) {
                    for (auto& loc : chimney_locs) {
                        if (loc.x == x and loc.y >= y) {
                            mem->tiles[x][y] = Tile::roof_chimney;
                            chimney_loc_ = RoomCoord{u8(x), u8(y)};
                            placed_chimney = true;
                            placed_chimney_this_tile = true;
                        }
                    }
                }
                // NOTE: when placing a flag, we need to make sure that the slot
                // above the current tile is empty, because the flag is two
                // tiles tall.
                if (y > 0 and mem->mat[x][y - 1] == 0 and
                    mem->tiles[x][y - 2] == 0) {
                    if (not placed_chimney_this_tile and show_flag_ and
                        not placed_flag and y > 5) {
                        placed_flag = true;
                        mem->tiles[x][y] = Tile::roof_flag;
                        mem->tiles[x][y - 1] = Tile::flag_start;
                        flag_pos_ = {x, u8(y - 1)};
                    }
                }
            } else if (y == 14 and mem->tiles[x][y] == 0 and
                       x < (int)terrain_.size()) {
                if (not mountain_terrain_ or
                    (x > 0 and x < terrain_.size() - 1)) {
                    mem->tiles[x][y] = Tile::grass;
                }
            } else if (mem->mat[x][y] == 0 and mem->mat[x][y + 1] == 2) {
                bool placed_chimney_this_tile = false;
                if (not placed_chimney and y > 5) {
                    for (auto& loc : chimney_locs) {
                        if (loc.x == x and loc.y >= y) {
                            mem->tiles[x][y] = Tile::tin_chimney;
                            placed_chimney = true;
                            chimney_loc_ = RoomCoord{u8(x), u8(y)};
                            placed_chimney_this_tile = true;
                        }
                    }
                }
                if (y > 0 and mem->mat[x][y - 1] == 0) {
                    if (not placed_chimney_this_tile and show_flag_ and
                        not placed_flag and y > 1 and mem->mat[x][y - 1] == 0) {
                        if (auto room = get_room({x, (u8)(y + 1)})) {
                            if ((*room->metaclass())->properties() &
                                    RoomProperties::flag_mount and
                                y > 5) {
                                placed_flag = true;
                                mem->tiles[x][y] = Tile::flag_mount;
                                mem->tiles[x][y - 1] = Tile::flag_start;
                                flag_pos_ = {x, u8(y - 1)};
                            }
                        }
                    }
                }
            }
        }
    }

    if (not interior_visible_) {
        // Clean up connections between stacked rooms
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 15; ++y) {
                auto t1 = mem->tiles[x][y];
                auto t2 = mem->tiles[x][y + 1];

                if (t1 == Tile::wall_plain_1) {
                    if (t2 == Tile::wall_window_1) {
                        t2 = Tile::wall_window_middle_1;
                    }
                    if (t2 == Tile::wall_plain_1) {
                        t2 = Tile::wall_plain_middle;
                    }
                    mem->tiles[x][y + 1] = t2;
                }
                if (t1 == Tile::wall_window_2) {
                    if (t2 == Tile::wall_window_1) {
                        mem->tiles[x][y] = Tile::wall_window_middle_2;
                        mem->tiles[x][y + 1] = Tile::wall_window_middle_1;
                    } else if (t2 == Tile::wall_plain_1) {
                        mem->tiles[x][y] = Tile::wall_window_middle_2;
                        mem->tiles[x][y + 1] = Tile::wall_plain_middle;
                    }
                } else if (t1 == Tile::wall_plain_2) {
                    if (t2 == Tile::wall_window_1) {
                        mem->tiles[x][y] = Tile::wall_plain_middle;
                        mem->tiles[x][y + 1] = Tile::wall_window_middle_1;
                    } else if (t2 == Tile::wall_plain_1) {
                        mem->tiles[x][y] = Tile::wall_plain_middle;
                        mem->tiles[x][y + 1] = Tile::wall_plain_middle;
                    }
                }
            }
        }

        if (APP.environment().is_night()) {
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 15; ++y) {
                    switch (mem->tiles[x][y]) {
                    case Tile::wall_window_middle_1:
                        mem->tiles[x][y] = Tile::window_lit_1;
                        break;
                    case Tile::wall_window_middle_2:
                        mem->tiles[x][y] = Tile::window_lit_2;
                        break;
                    case Tile::wall_window_1:
                        mem->tiles[x][y] = Tile::wall_window_lit_1;
                        break;
                    case Tile::wall_window_2:
                        mem->tiles[x][y] = Tile::wall_window_lit_2;
                        break;

                        // NOTE: these roof tiles need dedicated slots for the
                        // nighttime scenes because I wanted to have the windows
                        // lit, but didn't have enough color palette indices for
                        // the opponent island exterior, so I used the dark
                        // color index for the roof and used the roof color
                        // index to color the window panes. What are the odds
                        // that I forget that I did this and break stuff later?
                        // Very possible...
                    case Tile::roof_plain:
                        mem->tiles[x][y] = Tile::roof_plain_night;
                        break;
                    case Tile::roof_chimney:
                        mem->tiles[x][y] = Tile::roof_chimney_night;
                        break;
                    case Tile::roof_flag:
                        mem->tiles[x][y] = Tile::roof_flag_night;
                        break;
                    case Tile::roof_strut:
                        mem->tiles[x][y] = Tile::roof_strut_night;
                        break;
                    case Tile::roof_strut_joined:
                        mem->tiles[x][y] = Tile::roof_strut_joined_night;
                        break;
                    }
                }
            }
        }
    }

    for (auto& r : rooms_) {
        r->render_cloak(mem->tiles);
    }

    if (flag_pos_) {
        auto t = mem->tiles[flag_pos_->x][flag_pos_->y];
        if (t == Tile::cloaked or t == Tile::null) {
            flag_pos_.reset();
        }
        t = mem->tiles[flag_pos_->x][flag_pos_->y + 1];
        if (t == Tile::null) {
            flag_pos_.reset();
        }
    }

    if (chimney_loc_) {
        auto t = mem->tiles[chimney_loc_->x][chimney_loc_->y];
        if (t == Tile::cloaked or t == Tile::null) {
            chimney_loc_.reset();
        }
    }

    if (not repaint_alloc_tiles(mem->tiles, false)) {
        repaint_alloc_tiles(mem->tiles, true);
    }

    if (layer_ == Layer::map_0_ext and flag_pos_) {
        PLATFORM.set_palette(layer_, flag_pos_->x, flag_pos_->y, 12);
    }

    render_terrain();


    for (auto& room : rooms_) {
        if ((*room->metaclass())->category() == Room::Category::weapon) {
            auto pos = room->position();
            pos.y += room->size().y - 1;

            if (room->group() not_eq Room::Group::none) {
                // NOTE: 8x8px tile, so start index = 4 * 16ptile + 1.
                // +1 to skip the none enumeration.
                const auto tile = (6 * 4 - 1) + (int)room->group();
                if (layer_ == Layer::map_0_ext) {
                    PLATFORM.set_raw_tile(
                        Layer::map_0, pos.x * 2, pos.y * 2 + 1, tile);
                }
            }
        }
        if (room->is_powered_down()) {

            auto p = room->position();
            for (int x = 0; x < room->size().x; ++x) {
                for (int y = 0; y < room->size().y; ++y) {
                    PLATFORM.set_palette(Layer::map_0_ext, p.x + x, p.y + y, 9);
                }
            }

            const auto tile = (6 * 4 - 1) + 4;
            if (layer_ == Layer::map_0_ext) {
                auto [x, y] = room->position();
                PLATFORM.set_raw_tile(Layer::map_0, x * 2, y * 2, tile);
            }
        } else if (show_powerdown_opts_ and room->allows_powerdown()) {
            const auto tile = (6 * 4 - 1) + 4 + 5;
            if (layer_ == Layer::map_0_ext) {
                auto [x, y] = room->position();
                PLATFORM.set_raw_tile(Layer::map_0, x * 2, y * 2, tile);
            }
        }
    }
}



u8 Island::instance_count(MetaclassIndex idx) const
{
    u8 count = 0;

    for (auto& room : rooms_) {
        if (room->metaclass_index() == idx) {
            ++count;
        }
    }

    return count;
}



u8 Island::workshop_count() const
{
    return instance_count(metaclass_index("workshop"));
}



u8 Island::manufactory_count() const
{
    return instance_count(metaclass_index("manufactory"));
}



void Island::set_drift(Fixnum drift)
{
    if (APP.opponent_island() and this == APP.opponent_island()) {

        time_stream::event::OpponentIslandDriftChanged e;
        e.previous_speed__data_.set(drift_.data());

        APP.push_time_stream(e);
    }

    if (is_player_island(this)) {
        Platform::fatal("player island not intended to change position");
    }

    drift_ = drift;
}



Vec2<Fixnum> Island::origin() const
{
    return {position_.x, position_.y};
}



Vec2<Fixnum> Island::visual_origin() const
{
    return {position_.x, position_.y + Fixnum::from_integer(ambient_movement_)};
}



Optional<SharedEntityRef<Drone>> Island::get_drone(const RoomCoord& coord)
{
    for (auto& drone_sp : drones()) {
        if (drone_sp->position() == coord) {
            return drone_sp;
        }
    }

    return {};
}



Room* Island::get_room(const RoomCoord& coord)
{
    return rooms_.get_room(coord);
}



void Island::destroy_room(const RoomCoord& coord)
{
    for (auto& room : rooms_) {
        if (coord.x >= room->position().x and coord.y >= room->position().y and
            coord.x < room->position().x + room->size().x and
            coord.y < room->position().y + room->size().y) {

            room->finalize();
            rooms_.erase(&room);
            owner().rooms_lost_++;

            on_layout_changed(coord);
            schedule_recompute_deflector_shields();
            unamplify_blocks();

            repaint();
            recalculate_power_usage();
            return;
        }
    }
}



void Island::init_ai_awareness()
{
    for (auto& r : rooms()) {
        r->init_ai_awareness();
    }
}



void Island::dispatch_room(Room* room)
{
    room->dispatch_update(dispatch_list_);
    dispatch_list_ = room;
}



void Island::drawfirst(Room* room)
{
    drawfirst_ = room;
}



void Island::cancel_dispatch()
{
    dispatch_cancelled_ = true;
}



void Island::resolve_cancelled_dispatch()
{
    // If a room was destroyed, we could try to fix dangling pointer issues
    // around the dispatch list, but it's simpler just to destroy the list and
    // recreate it.
    if (UNLIKELY(dispatch_cancelled_)) {
        dispatch_list_ = nullptr;
        drawfirst_ = nullptr;
        for (auto& room : rooms_) {
            dispatch_room(room.get());
        }
        dispatch_cancelled_ = false;
    }
}



void Island::clear()
{
    clear_rooms();

    characters_.clear();
    projectiles_.clear();
    drones_.clear();
}



void Island::clear_rooms()
{
    for (auto& room : rooms_) {
        room->finalize();
    }

    cancel_dispatch();

    rooms_.clear();
}



void Island::set_float_timer(Time value)
{
    timer_ = value;
}



u8 Island::offensive_capabilities() const
{
    return offensive_capabilities_;
}



u8 Island::character_count() const
{
    return character_count_;
}



void show_island(Island* island)
{
    if (island) {
        if (island->interior_visible()) {
            show_island_interior(island);
        } else {
            show_island_exterior(island);
        }
    }
    set_glow_color();
}



void show_island_interior(Island* island)
{
    if (island) {
        island->render_interior();

        if (auto gfx = island->custom_flag_graphics()) {
            FlagPixels px;
            px.load_custom(island->layer(), gfx);
            vram_write_flag(px, island->layer());
        }
    }

    write_custom_graphics();
}



void show_island_exterior(Island* island)
{
    if (island) {
        island->render_exterior();

        if (auto gfx = island->custom_flag_graphics()) {
            FlagPixels px;
            px.load_custom(island->layer(), gfx);
            vram_write_flag(px, island->layer());
        }
    }

    write_custom_graphics();
}



u8 Island::min_y() const
{
    return min_y_;
}



Island& player_island()
{
    return APP.player_island();
}



Island* opponent_island()
{
    return APP.opponent_island();
}



bool speaker_data_store(Island& island, const char* path)
{
    Vector<char> data;

    for (auto& room : island.rooms()) {
        if (auto speaker = room->cast<Speaker>()) {
            u8 coord = 0;
            coord |= (speaker->position().x << 4);
            coord |= 0x0f & (speaker->position().y);
            data.push_back(coord);

            auto ef = speaker->effect_flags().vector_.data();

            // Store coordinate, followed by effect flags bitvector.

            for (u32 i = 0; i < sizeof *ef; ++i) {
                data.push_back(((u8*)ef)[i]);
            }

            for (u32 i = 0; i < sizeof speaker->settings_; ++i) {
                data.push_back(((u8*)&speaker->settings_)[i]);
            }
        }
    }

    if (data.size() not_eq 0) {
        return flash_filesystem::store_file_data_binary(path, data);
    } else {
        flash_filesystem::unlink_file(path);
    }

    return true;
}



bool speaker_data_load(Island& island, const char* path)
{
    Vector<char> data;

    auto bytes = flash_filesystem::read_file_data_binary(path, data);

    if (bytes) {
        auto current = data.begin();
        while (current not_eq data.end()) {
            u8 coord = *current;
            const u8 x = coord >> 4;
            const u8 y = coord & 0x0f;
            ++current;

            // flash_filesystem library automatically appends a null byte to the
            // end of whatever you read from it.
            if (coord == 0) {
                break;
            }

            if (auto room = island.get_room({x, y})) {
                if (auto speaker = room->cast<Speaker>()) {

                    Speaker::EffectVector v;

                    for (u32 i = 0; i < sizeof v; ++i) {
                        if (current == data.end()) {
                            info("effect flags truncated!");
                            return false;
                        }

                        ((u8*)&v)[i] = *current;

                        ++current;
                    }

                    memcpy(
                        speaker->effect_flags().vector_.data(), &v, sizeof v);


                    Speaker::Settings settings;

                    for (u32 i = 0; i < sizeof settings; ++i) {
                        if (current == data.end()) {
                            info("effect flags truncated!");
                            return false;
                        }

                        ((u8*)&settings)[i] = *current;

                        ++current;
                    }

                    speaker->settings_ = settings;

                } else {
                    info("target not a speaker!");
                    return false;
                }
            } else {
                info("room dne while loading speaker!");
                return false;
            }
        }
    }
    return true;
}



bool synth_notes_store(Island& island, const char* path)
{
    Vector<char> data;

    for (auto& room : island.rooms()) {
        if (auto synth = room->cast<Synth>()) {
            u8 coord = 0;
            coord |= (synth->position().x << 4);
            coord |= 0x0f & (synth->position().y);
            data.push_back(coord);

            for (int i = 0; i < 16; ++i) {
                static_assert(sizeof(*synth->notes()) == sizeof(u8));
                data.push_back(((u8*)synth->notes())[i]);
            }

            for (int i = 0; i < 16; ++i) {
                data.push_back(synth->effect_parameters()[i].value_);
            }
        }
    }

    if (data.size() not_eq 0) {
        return flash_filesystem::store_file_data_binary(path, data);
    } else {
        flash_filesystem::unlink_file(path);
    }

    return true;
}



bool synth_notes_load(Island& island, const char* path)
{
    Vector<char> data;

    auto bytes = flash_filesystem::read_file_data_binary(path, data);

    if (bytes) {
        auto current = data.begin();
        while (current not_eq data.end()) {
            u8 coord = *current;
            const u8 x = coord >> 4;
            const u8 y = coord & 0x0f;
            ++current;

            if (coord == 0) {
                break;
            }

            if (auto room = island.get_room({x, y})) {
                if (auto synth = room->cast<Synth>()) {
                    for (int i = 0; i < 16; ++i) {
                        if (current == data.end()) {
                            info("synth notes truncated");
                            return false;
                        }

                        static_assert(sizeof(*synth->notes()) == sizeof(u8));

                        u8 val = *current;

                        ((u8*)synth->notes())[i] = val;

                        ++current;
                    }
                    for (int i = 0; i < 16; ++i) {
                        if (current == data.end()) {
                            info("synth effects truncated");
                            return false;
                        }

                        u8 val = *current;
                        synth->effect_parameters()[i].value_ = val;

                        ++current;
                    }

                } else {
                    info("target not a synth!");
                    return false;
                }
            } else {
                info("room dne while loading synth");
                return false;
            }
        }
    }

    return true;
}



bool is_player_island(const Island* isle)
{
    return isle == &APP.player_island();
}



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y);



void collect_outer_rooms(Island& isle, Vector<Room*>& output)
{
    u8 matrix[16][16];
    isle.plot_rooms(matrix);

    auto add_set = [&](auto& isle, u8 x, u8 y) {
        auto room = isle.get_room({x, y});
        if (not room) {
            return;
        }
        for (auto& r : output) {
            if (r == room) {
                return;
            }
        }
        output.push_back(room);
    };

    // Shift blocks right by 1 so that there is empty space to the left
    // for the flood fill computation.
    for (int x = 14; x > -1; --x) {
        for (int y = 0; y < 16; ++y) {
            matrix[x + 1][y] = matrix[x][y];
        }
    }
    for (int y = 0; y < 16; ++y) {
        matrix[0][y] = 0;
    }
    for (int x = 0; x < 16; ++x) {
        matrix[x][15] = 1;
    }

    static const int outer_fill = 99;

    flood_fill(matrix, outer_fill, 0, 0);

    for (u8 x = 0; x < isle.terrain().size() + 1; ++x) {
        for (u8 y = 0; y < 15; ++y) {
            if (matrix[x][y] > 0 and matrix[x][y] not_eq outer_fill) {
                if (matrix[x][y - 1] == outer_fill) {
                    add_set(isle, x - 1, y);
                    continue;
                }
                if (matrix[x - 1][y] == outer_fill) {
                    add_set(isle, x - 1, y);
                    continue;
                }
                if (matrix[x + 1][y] == outer_fill) {
                    add_set(isle, x - 1, y);
                    continue;
                }
                if (matrix[x][y - 1] == outer_fill) {
                    add_set(isle, x - 1, y);
                    continue;
                }
            }
        }
    }
}



BlockChecksum opponent_island_checksum()
{
    if (APP.opponent_island()) {
        return APP.opponent_island()->checksum();
    } else {
        return 0;
    }
}



BlockChecksum island_checksums()
{
    return APP.player_island().checksum() + opponent_island_checksum();
}



} // namespace skyland
