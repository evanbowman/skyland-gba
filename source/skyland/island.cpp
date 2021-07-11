#include "island.hpp"
#include "alloc_entity.hpp"
#include "entity/explosion/explosion.hpp"
#include "entity/misc/smokePuff.hpp"
#include "globals.hpp"
#include "network.hpp"
#include "number/random.hpp"
#include "roomPool.hpp"
#include "room_metatable.hpp"
#include "rooms/core.hpp"
#include "skyland.hpp"
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



void Island::flip(Platform& pfrm)
{
    int max = 0;
    for (auto& room : rooms_) {
        if (max < room->position().x) {
            max = room->position().x;
        }
    }

    for (auto& room : rooms_) {
        room->__position().x =
            (room->position().x - max) - (room->size().x - 1);
    }

    repaint(pfrm);
}



void Island::update(Platform& pfrm, App& app, Microseconds dt)
{
    timer_ += dt;

    if (show_flag_ and flag_pos_) {
        flag_anim_timer_ += dt;
        if (flag_anim_timer_ > milliseconds(80)) {
            flag_anim_timer_ = 0;
            auto current = pfrm.get_tile(layer_, flag_pos_->x, flag_pos_->y);
            if (current < Tile::flag_end) {
                pfrm.set_tile(layer_, flag_pos_->x, flag_pos_->y, current + 1);
            } else {
                pfrm.set_tile(
                    layer_, flag_pos_->x, flag_pos_->y, Tile::flag_start);
            }
        }
    }

    owner_->update(pfrm, app, dt);

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

    ambient_movement_ = 4 * float(sine(4 * 3.14f * 0.0005f * timer_ + 180)) /
                        std::numeric_limits<s16>::max();


    const bool movement_ready = all_characters_awaiting_movement_;
    all_characters_awaiting_movement_ = true;

    is_boarded_ = false;

    auto update_characters = [&](auto& chr_list) {
        for (auto it = chr_list.begin(); it not_eq chr_list.end();) {
            if (not(*it)->alive()) {

                network::packet::CharacterDied packet;
                packet.chr_x_ = (*it)->grid_position().x;
                packet.chr_y_ = (*it)->grid_position().y;
                packet.near_island_ = &owner() not_eq &app.player();
                packet.chr_owned_by_player_ =
                    (*it)->owner() not_eq &app.player();
                network::transmit(pfrm, packet);

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


    for (auto it = rooms_.begin(); it not_eq rooms_.end();) {
        if ((*it)->health() == 0) {
            big_explosion(pfrm, app, (*it)->center());

            const auto pos = (*it)->position();

            network::packet::RoomDestroyed packet;
            packet.room_x_ = pos.x;
            packet.room_y_ = pos.y;
            packet.near_island_ = &owner() not_eq &app.player();
            packet.metaclass_index_.set(
                metaclass_index((*(*it)->metaclass())->name()));
            network::transmit(pfrm, packet);


            it = rooms_.erase(it);

            owner().rooms_lost_++;

            on_layout_changed(pos);

            bool has_core = false;
            for (auto& room : rooms_) {
                if (dynamic_cast<Core*>(&*room)) {
                    has_core = true;
                    break;
                }
            }
            if (not has_core) {
                destroyed_ = true;
            }

            recalculate_power_usage();

            repaint(pfrm);
        } else {
            (*it)->update(pfrm, app, dt);

            update_characters((*it)->characters());

            ++it;
        }
    }


    update_characters(characters_);


    update_entities(pfrm, app, dt, projectiles_);

    if (drift_) {
        position_.x += drift_ * dt;
    }

    pfrm.set_scroll(layer(),
                    -get_position().cast<u16>().x,
                    -get_position().cast<u16>().y - get_ambient_movement());
}



void Island::on_layout_changed(const Vec2<u8>& room_added_removed_coord)
{

    for (auto& room : rooms()) {
        if (str_cmp((*room->metaclass())->name(), "power-core") == 0) {
            destroyed_ = false;
        }
        for (auto& chr : room->characters()) {
            if (auto path = chr->get_movement_path()) {
                for (auto& node : *path) {
                    if (get_room(node) == nullptr) {
                        // Ok, a character is moving, but the room in the
                        // character's movement path no longer exists. We should
                        // remove the movement path, otherwise, the character
                        // will try to walk into a non-existing room, fail to
                        // re-attach itself to a new room structure, and be
                        // deallocated.
                        chr->drop_movement_path();
                        break; // NOTE: we just deallocated the path, so we
                               // cannot run the enclosing loop over the path
                               // nodes anymore.
                    }
                }
            }
        }
    }
}



static const int screen_limit_y = 700;



void Island::display(Platform& pfrm)
{
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

    if (interior_visible()) {
        for (auto& room : rooms_) {
            for (auto& c : room->characters()) {
                const auto& pos = c->sprite().get_position();
                if (pos.y < screen_limit_y) {
                    pfrm.screen().draw(c->sprite());
                }
            }
        }
    }
}



void Island::test_collision(Platform& pfrm, App& app, Entity& entity)
{
    // First, check whether the hitbox for the entity intersects with the
    // x-value range of the island. If not, then there's no need to check
    // collisions with individual rooms.

    Vec2<Float> hitbox_pos = this->origin();
    HitBox island_hitbox;
    island_hitbox.position_ = &hitbox_pos;
    island_hitbox.dimension_.size_.x = terrain_.size() * 16;
    island_hitbox.dimension_.size_.y = 16 * 16;

    if (island_hitbox.overlapping(entity.hitbox())) {
        // Now, the Entity must be within the larger bounding box for the
        // island. Lets scan through each of the island's rooms until we find a
        // collision.
        for (auto& room : rooms_) {

            static const int tile_size = 16;

            hitbox_pos = this->origin();
            hitbox_pos.x += room->position().x * tile_size;
            hitbox_pos.y += room->position().y * tile_size;

            HitBox room_hitbox;
            room_hitbox.position_ = &hitbox_pos;
            room_hitbox.dimension_.size_.x = room->size().x * tile_size;
            room_hitbox.dimension_.size_.y = room->size().y * tile_size;

            if (room_hitbox.overlapping(entity.hitbox())) {
                // TODO: deliver collisions to room and to entity.
                // TODO: actually test this code to see if it even works.
                entity.on_collision(pfrm, app, *room);
                room->on_collision(pfrm, app, entity);
                return;
            }
        }
    }
}



void Island::render_terrain(Platform& pfrm)
{
    for (u32 i = 0; i < terrain_.size(); ++i) {
        pfrm.set_tile(layer_, i, 15, terrain_[i]);
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



void Island::render_interior(Platform& pfrm)
{
    for (auto& room : rooms()) {
        room->render_interior(pfrm, layer_);
    }

    interior_visible_ = true;
}



void Island::render_exterior(Platform& pfrm)
{
    for (auto& room : rooms()) {
        room->render_exterior(pfrm, layer_);
    }

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 15; ++y) {
            auto t1 = pfrm.get_tile(layer_, x, y);
            auto t2 = pfrm.get_tile(layer_, x, y + 1);

            if (t1 == Tile::wall_window_2) {
                if (t2 == Tile::wall_window_1) {
                    pfrm.set_tile(layer_, x, y, Tile::wall_window_middle_2);
                    pfrm.set_tile(layer_, x, y + 1, Tile::wall_window_middle_1);
                } else if (t2 == Tile::wall_plain_1) {
                    pfrm.set_tile(layer_, x, y, Tile::wall_window_middle_2);
                    pfrm.set_tile(layer_, x, y + 1, Tile::wall_plain_middle);
                }
            } else if (t1 == Tile::wall_plain_2) {
                if (t2 == Tile::wall_window_1) {
                    pfrm.set_tile(layer_, x, y, Tile::wall_plain_middle);
                    pfrm.set_tile(layer_, x, y + 1, Tile::wall_window_middle_1);
                } else if (t2 == Tile::wall_plain_1) {
                    pfrm.set_tile(layer_, x, y, Tile::wall_plain_middle);
                    pfrm.set_tile(layer_, x, y + 1, Tile::wall_plain_middle);
                }
            }
        }
    }

    interior_visible_ = false;
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



void Island::plot_walkable_zones(bool matrix[16][16]) const
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            matrix[x][y] = 0;
        }
    }

    // TODO: label outdoor grass areas as walkable.

    for (auto& room : rooms_) {
        room->plot_walkable_zones(matrix);
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
}



void Island::repaint(Platform& pfrm)
{
    u8 matrix[16][16];

    flag_pos_.reset();

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(layer_, x, y, 0);
        }
    }

    render_terrain(pfrm);

    if (interior_visible_) {
        render_interior(pfrm);
    } else {
        render_exterior(pfrm);
    }

    plot_rooms(matrix);

    Buffer<u8, terrain_.capacity()> chimney_locs;

    has_radar_ = false;
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
        }
    }

    chimney_loc_.reset();

    bool placed_flag = false;
    bool placed_chimney = false;

    for (u8 x = 0; x < 16; ++x) {
        for (int y = 15; y > -1; --y) {
            if (matrix[x][y] == 0 and y < 15 and matrix[x][y + 1] == 1) {
                pfrm.set_tile(layer_, x, y, Tile::roof_plain);
                bool placed_chimney_this_tile = false;
                if (not placed_chimney) {
                    for (auto& loc : chimney_locs) {
                        if (loc == x) {
                            pfrm.set_tile(layer_, x, y, Tile::roof_chimney);
                            chimney_loc_ = Vec2<u8>{u8(x), u8(y)};
                            placed_chimney = true;
                            placed_chimney_this_tile = true;
                        }
                    }
                }
                if (not placed_chimney_this_tile and show_flag_ and
                    not placed_flag) {
                    placed_flag = true;
                    pfrm.set_tile(layer_, x, y, Tile::roof_flag);
                    pfrm.set_tile(layer_, x, y - 1, Tile::flag_start);
                    flag_pos_ = {x, u8(y - 1)};
                }
            } else if (y == 14 and matrix[x][y] == 0 and
                       x < (int)terrain_.size()) {
                pfrm.set_tile(layer_, x, y, Tile::grass);
            } else if (matrix[x][y] == 0 and matrix[x][y + 1] == 2) {
                bool placed_chimney_this_tile = false;
                if (not placed_chimney) {
                    for (auto& loc : chimney_locs) {
                        if (loc == x) {
                            pfrm.set_tile(layer_, x, y, Tile::tin_chimney);
                            placed_chimney = true;
                            chimney_loc_ = Vec2<u8>{u8(x), u8(y)};
                            placed_chimney_this_tile = true;
                        }
                    }
                }
                if (not placed_chimney_this_tile and show_flag_ and
                    not placed_flag and y > 1 and matrix[x][y - 1] == 0) {
                    if (auto room = get_room({x, (u8)(y + 1)})) {
                        if (str_cmp((*room->metaclass())->name(), "hull") ==
                            0) {
                            placed_flag = true;
                            pfrm.set_tile(layer_, x, y, Tile::flag_mount);
                            pfrm.set_tile(layer_, x, y - 1, Tile::flag_start);
                            flag_pos_ = {x, u8(y - 1)};
                        }
                    }
                }
            }
        }
    }
}



void Island::set_drift(Float drift)
{
    drift_ = drift;
}



Vec2<Float> Island::origin() const
{
    return {position_.x, position_.y + ambient_movement_};
}



Room* Island::get_room(const Vec2<u8>& coord)
{
    for (auto& room : rooms_) {
        if (coord.x >= room->position().x and coord.y >= room->position().y and
            coord.x < room->position().x + room->size().x and
            coord.y < room->position().y + room->size().y) {

            return room.get();
        }
    }

    return nullptr;
}



void Island::destroy_room(Platform& pfrm, const Vec2<u8>& coord)
{
    for (auto& room : rooms_) {
        if (coord.x >= room->position().x and coord.y >= room->position().y and
            coord.x < room->position().x + room->size().x and
            coord.y < room->position().y + room->size().y) {

            rooms_.erase(&room);
            owner().rooms_lost_++;

            on_layout_changed(coord);

            repaint(pfrm);
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
