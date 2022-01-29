#include "island.hpp"
#include "alloc_entity.hpp"
#include "entity/explosion/explosion.hpp"
#include "entity/misc/smokePuff.hpp"
#include "entity/projectile/projectile.hpp"
#include "globals.hpp"
#include "network.hpp"
#include "number/random.hpp"
#include "roomPool.hpp"
#include "room_metatable.hpp"
#include "rooms/core.hpp"
#include "skyland.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "tile.hpp"



namespace skyland {



void Island::init_terrain(Platform& pfrm, int width)
{
    if (width < 3) {
        return;
    }

    terrain_.clear();

    terrain_.push_back(13), --width;

    for (int i = 0; i < width - 1; ++i) {
        terrain_.push_back(12);
    }

    terrain_.push_back(14);

    render_terrain(pfrm);
}



Island::Island(Platform& pfrm, Layer layer, u8 width, Player& owner)
    : layer_(layer), timer_(0), interior_visible_(false),
      characters_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      projectiles_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      drones_(std::get<SkylandGlobalData>(globals()).entity_node_pool_),
      owner_(&owner)
{
    init_terrain(pfrm, width);
}



Island::Rooms& Island::rooms()
{
    return rooms_;
}


void Island::remove_character(const Vec2<u8>& location)
{
    if (auto room = get_room(location)) {
        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {
            if ((*it)->grid_position() == location) {
                room->characters().erase(it);
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


BasicCharacter* Island::character_at_location(const Vec2<u8>& loc)
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



std::pair<BasicCharacter*, Room*> Island::find_character_by_id(CharacterId id)
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



void Island::rewind(Platform& pfrm, App& app, Microseconds delta)
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

            o.x += chimney_loc_->x * 16 + 8;
            o.y += chimney_loc_->y * 16 - 4;

            if (drift_ == 0.f) {
                // FIXME: faking the rewound smoke effects doesn't work if the
                // island is moving. So just don't spawn them. We'll need to
                // think of another way.
                if (auto e = app.alloc_entity<SmokePuff>(pfrm, o)) {
                    e->jump_to_end();
                    app.effects().push(std::move(e));
                }
            }
        }
    }

    auto& projectiles = this->projectiles();
    for (auto it = projectiles.begin(); it not_eq projectiles.end();) {
        if ((*it)->health() == 0) {
            it = projectiles.erase(it);
        } else {
            // FIXME: I was casting before Entity implemented
            // rewind(). Projectile implemented an interface for rewind() before
            // Entity did. Now, this check is unnecessary.
            if (auto p = dynamic_cast<Projectile*>(&**it)) {
                p->rewind(pfrm, app, delta);
            } else {
                // Raise error: why is a non-projectile in the island's
                // projectile list?
            }
            ++it;
        }
    }

    for (auto& room : rooms_) {
        room->rewind(pfrm, app, delta);
        for (auto& chr : room->characters()) {
            chr->rewind(pfrm, app, delta);
        }
    }

    if (drift_) {
        position_.x -= drift_ * delta;
    }


    for (auto it = drones_.begin(); it not_eq drones_.end();) {
        if (auto ptr = (*it).promote()) {

            if (not(*ptr)->alive()) {
                it = drones_.erase(it);
            } else {
                (*ptr)->rewind(pfrm, app, delta);
                ++it;
            }
        } else {
            it = drones_.erase(it);
        }
    }


    u8 ambient_offset = 4 * float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
                        std::numeric_limits<s16>::max();

    ambient_movement_ = ambient_offset;


    pfrm.set_scroll(layer(),
                    -get_position().cast<u16>().x,
                    -get_position().cast<u16>().y - ambient_offset);
}



void Island::check_destroyed()
{
    destroyed_ = false;
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



void Island::update(Platform& pfrm, App& app, Microseconds dt)
{
    timer_ += dt;

    if (show_flag_ and flag_pos_) {
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

    if (chimney_loc_) {
        chimney_spawn_timer_ += dt;

        if (chimney_spawn_timer_ > milliseconds(600)) {
            chimney_spawn_timer_ = 0;

            auto o = origin();

            o.x += chimney_loc_->x * 16 + 8;
            o.y += chimney_loc_->y * 16 - 4;

            if (auto e = app.alloc_entity<SmokePuff>(pfrm, o)) {
                app.effects().push(std::move(e));
            }
        }
    }

    u8 ambient_offset = 4 * float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
                        std::numeric_limits<s16>::max();

    ambient_movement_ = ambient_offset;


    const bool movement_ready = all_characters_awaiting_movement_;
    all_characters_awaiting_movement_ = true;

    is_boarded_ = false;


    auto update_characters = [&](auto& chr_list) {
        for (auto it = chr_list.begin(); it not_eq chr_list.end();) {
            if (not(*it)->alive()) {

                network::packet::CharacterDied packet;
                packet.chr_x_ = (*it)->grid_position().x;
                packet.chr_y_ = (*it)->grid_position().y;
                // NOTE: some fields intentionally inverted, everything is sort
                // of flipped from the receiver's perspective.
                packet.near_island_ = &owner() not_eq &app.player();
                packet.chr_owned_by_player_ =
                    (*it)->owner() not_eq &app.player();
                network::transmit(pfrm, packet);


                time_stream::event::CharacterDied e;
                e.x_ = (*it)->grid_position().x;
                e.y_ = (*it)->grid_position().y;
                e.id_.set((*it)->id());
                e.owned_by_player_ = (*it)->owner() == &app.player();
                e.near_ = this == &app.player_island();
                e.is_replicant_ = (*it)->is_replicant();
                app.time_stream().push(pfrm, app.level_timer(), e);


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
                    is_boarded_ = true;
                }
                (*it)->update(pfrm, app, dt);
                ++it;
            }
        }
    };


    static const auto sync_delay = milliseconds(100);


    for (auto it = drones_.begin(); it not_eq drones_.end();) {
        if (auto ptr = (*it).promote()) {

            if (not(*ptr)->alive()) {

                auto sync = [x = (*ptr)->position().x,
                             y = (*ptr)->position().y,
                             near = (*ptr)->destination() ==
                                    &app.player_island()](Platform& pfrm,
                                                          App& app) {
                    network::packet::DroneDestroyed destroyed;
                    destroyed.drone_x_ = x;
                    destroyed.drone_y_ = y;
                    destroyed.destination_near_ = near;

                    network::transmit(pfrm, destroyed);
                };

                if (pfrm.network_peer().is_connected()) {
                    if (not app.on_timeout(pfrm, sync_delay, sync)) {
                        sync(pfrm, app);
                    }
                }

                it = drones_.erase(it);
            } else {
                (*ptr)->update(pfrm, app, dt);
                ++it;
            }
        } else {
            it = drones_.erase(it);
        }
    }


    // Before I added the ArcGun weapon, which chains to multiple rooms, there
    // was no reason to do any optimizations on certain operations, because only
    // one room would be destroyed per frame, except in truely rare occasions.
    int destroyed_count = 0;


    bool do_repaint = false;


    for (auto it = rooms_.begin(); it not_eq rooms_.end();) {
        if ((*it)->health() == 0) {
            if (++destroyed_count < 5) {
                // Five rooms destroyed on the same island in the same frame! If
                // we create tons of huge explosions all at once, we'll lag the
                // game and use lots of entities.
                big_explosion(pfrm, app, (*it)->center());
            }

            if (destroyed_count < 2 and
                app.game_mode() not_eq App::GameMode::multiplayer) {
                pfrm.sleep(2);
            }

            const auto pos = (*it)->position();

            auto mt = metaclass_index((*(*it)->metaclass())->name());

            auto sync = [x = pos.x,
                         y = pos.y,
                         near = &owner() not_eq &app.player(),
                         mt](Platform& pfrm, App& app) {
                network::packet::RoomDestroyed packet;
                packet.room_x_ = x;
                packet.room_y_ = y;
                packet.near_island_ = near;
                packet.metaclass_index_.set(mt);
                network::transmit(pfrm, packet);
            };


            if (&owner() == &app.player()) {
                time_stream::event::PlayerRoomDestroyed p;
                p.x_ = pos.x;
                p.y_ = pos.y;
                p.type_ = mt;
                app.time_stream().push(pfrm, app.level_timer(), p);
            } else {
                time_stream::event::OpponentRoomDestroyed p;
                p.x_ = pos.x;
                p.y_ = pos.y;
                p.type_ = mt;
                app.time_stream().push(pfrm, app.level_timer(), p);
            }


            if (destroyed_count < 2) {
                pfrm.speaker().play_sound("explosion1", 2);
            }

            if (pfrm.network_peer().is_connected()) {
                if (not app.on_timeout(pfrm, sync_delay, sync)) {
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
                    // which case, both games should already be synchronized,
                    // and there is in fact nothing to destroy. If we fail to
                    // enqueue the deferred callback for some reason, invoke
                    // immediately. I want to stress: we only transmit the
                    // room-destroyed packet as a last resort, in case the games
                    // somehow got out of sync. This packet should in almost all
                    // cases be meaningless to the other game.
                    sync(pfrm, app);
                }
            }


            app.player().on_room_destroyed(pfrm, app, **it);

            it = rooms_.erase(it);

            owner().rooms_lost_++;

            on_layout_changed(app, pos);

            check_destroyed();

            recalculate_power_usage();

            do_repaint = true;
        } else {
            if (dt not_eq 0) {
                // Do not update a room if the game is stopped.
                (*it)->update(pfrm, app, dt);
            }

            update_characters((*it)->characters());

            ++it;
        }
    }


    if (do_repaint) {
        repaint(pfrm, app);
    }


    update_characters(characters_);


    update_entities(pfrm, app, dt, projectiles_);

    if (drift_) {
        position_.x += drift_ * dt;
    }

    pfrm.set_scroll(layer(),
                    -get_position().cast<u16>().x,
                    -get_position().cast<u16>().y - ambient_offset);
}



void Island::on_layout_changed(App& app,
                               const Vec2<u8>& room_added_removed_coord)
{
    check_destroyed();

    bool buffer[16][16];
    plot_walkable_zones(app, buffer);

    for (auto& room : rooms()) {
        for (auto& chr : room->characters()) {
            if (auto path = chr->get_movement_path()) {
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
    }
}



static const int screen_limit_y = 700;



void Island::display(Platform& pfrm)
{
    if (flag_pos_ and show_flag_) {

        std::optional<u16> palette;

        if (layer_ == Layer::map_0_ext) {
            // NOTE: the player can design his/her own flag, so we reserve a
            // specific palette bank just for the flag image. Untimately, doing
            // so simplifies things.
            palette = 12;
        }
        pfrm.set_tile(
            layer_, flag_pos_->x, flag_pos_->y, flag_anim_index_, palette);
    }

    for (auto& c : characters_) {
        // The interior floor is two pixels thick. But our character is now
        // standing outside, where there's no floor, so we need to shift the
        // character down by two pixels.
        Sprite cpy = c->sprite();
        auto pos = cpy.get_position();
        if (pos.y < screen_limit_y) {
            pos.y += 2;
            cpy.set_position(pos);
            pfrm.screen().draw(cpy);
        }
    }

    for (auto& p : projectiles_) {
        pfrm.screen().draw(p->sprite());
    }


    for (auto& room : rooms_) {
        room->display(pfrm.screen());
    }
}



HitBox Island::hitbox() const
{
    Vec2<Float> hitbox_pos = this->origin();
    HitBox island_hitbox;
    island_hitbox.position_ = &hitbox_pos;
    island_hitbox.dimension_.size_.x = terrain_.size() * 16;
    island_hitbox.dimension_.size_.y = 16 * 16;
    return island_hitbox;
}



void Island::test_collision(Platform& pfrm, App& app, Entity& entity)
{
    // Calculate the position of the entity in terms of the island's grid
    // coordinates.
    auto entity_pos =
        (entity.sprite().get_position() - this->origin()).cast<int>();
    entity_pos.x /= 16;
    entity_pos.y /= 16;

    for (int x = entity_pos.x - 1; x < entity_pos.x + 2; ++x) {
        for (int y = entity_pos.y - 1; y < entity_pos.y + 2; ++y) {
            if (x < 0 or y < 0 or x > 15 or y > 15) {
                continue;
            }

            if (rooms_plot_.get(x, y)) {
                if (auto room = get_room({(u8)x, (u8)y})) {
                    static const int tile_size = 16;

                    auto hitbox_pos = this->origin();
                    hitbox_pos.x += room->position().x * tile_size;
                    hitbox_pos.y += room->position().y * tile_size;

                    HitBox room_hitbox;
                    room_hitbox.position_ = &hitbox_pos;
                    room_hitbox.dimension_.size_.x = room->size().x * tile_size;
                    room_hitbox.dimension_.size_.y = room->size().y * tile_size;

                    if (room_hitbox.overlapping(entity.hitbox())) {
                        entity.on_collision(pfrm, app, *room);
                        room->on_collision(pfrm, app, entity);
                        return;
                    }
                }
            }
        }
    }

    Vec2<Float> hitbox_pos = this->origin();
    HitBox island_hitbox;
    island_hitbox.position_ = &hitbox_pos;
    island_hitbox.dimension_.size_.x = terrain_.size() * 16;
    island_hitbox.dimension_.size_.y = 16 * 16;

    if (island_hitbox.overlapping(entity.hitbox())) {
        for (auto& drone_wp : drones_) {
            if (auto drone_sp = drone_wp.promote()) {
                if (entity.hitbox().overlapping((*drone_sp)->hitbox())) {
                    entity.on_collision(pfrm, app, **drone_sp);
                    (*drone_sp)->on_collision(pfrm, app, entity);
                }
            }
        }
    }
}



void Island::render_terrain(Platform& pfrm)
{
    for (u32 i = 0; i < terrain_.size(); ++i) {
        pfrm.set_tile(layer_, i, 15, terrain_[i]);
    }

    for (int i = terrain_.size(); i < 16; ++i) {
        pfrm.set_tile(layer_, i, 15, 0);
    }
}



const Vec2<Float>& Island::get_position() const
{
    return position_;
}



void Island::set_position(const Vec2<Float>& position)
{
    position_ = position;
}



void Island::render_interior(Platform& pfrm, App& app)
{
    interior_visible_ = true;

    repaint(pfrm, app);
}



void Island::render_exterior(Platform& pfrm, App& app)
{
    interior_visible_ = false;

    repaint(pfrm, app);
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
        if (room->has_roof()) {
            val = 1;
        } else if (room->disallow_chimney()) {
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
        auto metac = room->metaclass();
        auto power = (*metac)->consumes_power();

        if (power < 0) {
            power_supply_ += -power;
        } else {
            power_drain_ += power;
        }
    }
}



bool Island::add_character(EntityRef<BasicCharacter> character)
{
    if (auto room = get_room(character->grid_position())) {
        return room->add_occupant(std::move(character));
    } else {
        characters_.push(std::move(character));
        return true;
    }
}



void Island::plot_walkable_zones(App& app, bool matrix[16][16]) const
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            matrix[x][y] = 0;
        }
    }

    // TODO: label outdoor grass areas as walkable.

    for (auto& room : rooms_) {
        room->plot_walkable_zones(app, matrix);
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

    for (auto& drone : drones_) {
        if (auto sp = drone.promote()) {
            auto pos = (*sp)->position();
            matrix[pos.x][pos.y] = false;
            matrix[pos.x][pos.y + 1] = false;
        }
    }
}



void Island::repaint(Platform& pfrm, App& app)
{
    u8 matrix[16][16];
    u8 buffer[16][16]; // TODO: move this off of the stack!?

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            buffer[x][y] = 0;
        }
    }

    flag_pos_.reset();

    if (interior_visible_) {
        for (auto& room : rooms()) {
            room->render_interior(app, buffer);
        }
    } else {
        for (auto& room : rooms()) {
            room->render_exterior(app, buffer);
        }
    }

    plot_rooms(matrix);

    Buffer<u8, terrain_.capacity()> chimney_locs;

    has_radar_ = false;
    foundry_count_ = 0;
    workshop_count_ = 0;
    for (auto& room : rooms_) {
        if (room->has_chimney()) {
            chimney_locs.push_back(room->position().x);
        }
        auto metac = room->metaclass();
        if (str_cmp((*metac)->name(), "radar") == 0) {
            has_radar_ = true;
        } else if (str_cmp((*metac)->name(), "workshop") == 0) {
            ++workshop_count_;
        } else if (str_cmp((*metac)->name(), "foundry") == 0) {
            ++foundry_count_;
        }
    }

    chimney_loc_.reset();

    bool placed_flag = false;
    bool placed_chimney = false;

    std::optional<Vec2<u8>> flag_loc;

    rooms_plot_.clear();

    for (u8 x = 0; x < 16; ++x) {
        for (int y = 15; y > -1; --y) {

            rooms_plot_.set(x, y, matrix[x][y]);

            if (matrix[x][y] == 0 and y < 15 and matrix[x][y + 1] == 1) {
                buffer[x][y] = Tile::roof_plain;
                bool placed_chimney_this_tile = false;
                if (not placed_chimney) {
                    for (auto& loc : chimney_locs) {
                        if (loc == x) {
                            buffer[x][y] = Tile::roof_chimney;
                            chimney_loc_ = Vec2<u8>{u8(x), u8(y)};
                            placed_chimney = true;
                            placed_chimney_this_tile = true;
                        }
                    }
                }
                // NOTE: when placing a flag, we need to make sure that the slot
                // above the current tile is empty, because the flag is two
                // tiles tall.
                if (y > 0 and matrix[x][y - 1] == 0) {
                    if (not placed_chimney_this_tile and show_flag_ and
                        not placed_flag) {
                        placed_flag = true;
                        buffer[x][y] = Tile::roof_flag;
                        buffer[x][y - 1] = Tile::flag_start;
                        flag_pos_ = {x, u8(y - 1)};
                    }
                }
            } else if (y == 14 and matrix[x][y] == 0 and
                       x < (int)terrain_.size()) {
                buffer[x][y] = Tile::grass;
            } else if (matrix[x][y] == 0 and matrix[x][y + 1] == 2) {
                bool placed_chimney_this_tile = false;
                if (not placed_chimney) {
                    for (auto& loc : chimney_locs) {
                        if (loc == x) {
                            buffer[x][y] = Tile::tin_chimney;
                            placed_chimney = true;
                            chimney_loc_ = Vec2<u8>{u8(x), u8(y)};
                            placed_chimney_this_tile = true;
                        }
                    }
                }
                if (y > 0 and matrix[x][y - 1] == 0) {
                    if (not placed_chimney_this_tile and show_flag_ and
                        not placed_flag and y > 1 and matrix[x][y - 1] == 0) {
                        if (auto room = get_room({x, (u8)(y + 1)})) {
                            if (str_eq((*room->metaclass())->name(), "hull") or
                                str_eq((*room->metaclass())->name(),
                                       "masonry")) {
                                placed_flag = true;
                                buffer[x][y] = Tile::flag_mount;
                                buffer[x][y - 1] = Tile::flag_start;
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
                auto t1 = buffer[x][y];
                auto t2 = buffer[x][y + 1];

                if (t1 == Tile::wall_window_2) {
                    if (t2 == Tile::wall_window_1) {
                        buffer[x][y] = Tile::wall_window_middle_2;
                        buffer[x][y + 1] = Tile::wall_window_middle_1;
                    } else if (t2 == Tile::wall_plain_1) {
                        buffer[x][y] = Tile::wall_window_middle_2;
                        buffer[x][y + 1] = Tile::wall_plain_middle;
                    }
                } else if (t1 == Tile::wall_plain_2) {
                    if (t2 == Tile::wall_window_1) {
                        buffer[x][y] = Tile::wall_plain_middle;
                        buffer[x][y + 1] = Tile::wall_window_middle_1;
                    } else if (t2 == Tile::wall_plain_1) {
                        buffer[x][y] = Tile::wall_plain_middle;
                        buffer[x][y + 1] = Tile::wall_plain_middle;
                    }
                }
            }
        }
    }

    for (int x = 0; x < 16; ++x) {
        // NOTE: only handle 15 rows because render_terrain() takes care of the
        // last row.
        for (int y = 0; y < 15; ++y) {
            pfrm.set_tile(layer_, x, y, buffer[x][y]);
            if (buffer[x][y] >= Tile::dlc_tiles_begin) {
                pfrm.set_palette(layer_, x, y, 12);
            }
        }
    }

    if (layer_ == Layer::map_0_ext and flag_pos_) {
        pfrm.set_palette(layer_, flag_pos_->x, flag_pos_->y - 1, 12);
    }

    render_terrain(pfrm);
}



void Island::set_drift(Platform& pfrm, App& app, Float drift)
{
    if (app.opponent_island() and this == &*app.opponent_island()) {

        time_stream::event::OpponentIslandDriftChanged e;
        memcpy(e.previous_speed_, &drift_, sizeof drift_);

        app.time_stream().push(pfrm, app.level_timer(), e);
    }

    if (this == &app.player_island()) {
        Platform::fatal("player island not intended to change position");
    }

    drift_ = drift;
}



Vec2<Float> Island::origin() const
{
    return {position_.x, position_.y};
}



Vec2<Float> Island::visual_origin() const
{
    return {position_.x, position_.y + ambient_movement_};
}



std::optional<SharedEntityRef<Drone>> Island::get_drone(const Vec2<u8>& coord)
{
    for (auto& drone_wp : drones()) {
        if (auto drone_sp = drone_wp.promote();
            drone_sp and (*drone_sp)->position() == coord) {
            return *drone_sp;
        }
    }

    return {};
}



Room* Island::get_room(const Vec2<u8>& coord)
{
    return rooms_.get_room(coord);
}



void Island::destroy_room(Platform& pfrm, App& app, const Vec2<u8>& coord)
{
    for (auto& room : rooms_) {
        if (coord.x >= room->position().x and coord.y >= room->position().y and
            coord.x < room->position().x + room->size().x and
            coord.y < room->position().y + room->size().y) {

            rooms_.erase(&room);
            owner().rooms_lost_++;

            on_layout_changed(app, coord);

            repaint(pfrm, app);
            recalculate_power_usage();
            return;
        }
    }
}



void Island::set_float_timer(Microseconds value)
{
    timer_ = value;
}



} // namespace skyland
