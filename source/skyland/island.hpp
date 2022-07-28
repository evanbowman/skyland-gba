////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"
#include "entity.hpp"
#include "entity/character/basicCharacter.hpp"
#include "entity/drones/drone.hpp"
#include "memory/buffer.hpp"
#include "player/player.hpp"
#include "power.hpp"
#include "room.hpp"
#include "roomTable.hpp"
#include "room_alloc.hpp"



namespace skyland
{


class Island
{
public:
    Island(Platform& pfrm, Layer layer, u8 width, Player& owner);
    Island(const Island&) = delete;


    using Rooms = RoomTable<92, 16>;


    bool add_room(Platform& pfrm,
                  App& app,
                  RoomPtr<Room> insert,
                  bool do_repaint = true)
    {
        auto result = rooms_.insert_room(std::move(insert));
        if (do_repaint) {
            repaint(pfrm, app);
        }
        recalculate_power_usage(app);
        on_layout_changed(app, insert->position());
        return result;
    }


    template <typename T, typename... Args>
    bool add_room(Platform& pfrm,
                  App& app,
                  const RoomCoord& position,
                  bool do_repaint,
                  Args&&... args)
    {
        if (auto room = room_pool::alloc<T>(
                this, position, std::forward<Args>(args)...)) {
            if (rooms_.insert_room({room.release(), room_pool::deleter})) {
                if (do_repaint) {
                    repaint(pfrm, app);
                }
                recalculate_power_usage(app);
                on_layout_changed(app, position);
                return true;
            }
        }
        pfrm.fatal("room pool exhausted");
        return false;
    }


    void move_room(Platform& pfrm,
                   App& app,
                   const RoomCoord& from,
                   const RoomCoord& to);


    void init_terrain(Platform& pfrm, int width, bool render = true);


    bool add_character(EntityRef<BasicCharacter> character);


    void remove_character(const RoomCoord& location);


    Rooms& rooms();


    void clear_rooms(Platform&, App&);


    void clear(Platform&, App&);


    void update(Platform&, App&, Microseconds delta);


    void rewind(Platform&, App&, Microseconds delta);


    void display(Platform&);


    const Vec2<Fixnum>& get_position() const;


    void set_position(const Vec2<Fixnum>& position);


    Room* get_room(const RoomCoord& coord);


    std::optional<SharedEntityRef<Drone>> get_drone(const RoomCoord& coord);


    void destroy_room(Platform& pfrm, App& app, const RoomCoord& coord);


    u8 get_ambient_movement()
    {
        return ambient_movement_;
    }


    Layer layer() const
    {
        return layer_;
    }


    void set_hidden(Platform& pfrm, App& app, bool hidden);


    void render_interior(Platform& pfrm, App& app);


    void render_exterior(Platform& pfrm, App& app);


    void render(Platform&, App&);


    void plot_rooms(u8 matrix[16][16]) const;


    void plot_construction_zones(bool matrix[16][16]) const;


    void plot_walkable_zones(App& app, bool matrix[16][16]) const;


    BasicCharacter* character_at_location(const RoomCoord& loc);


    std::pair<BasicCharacter*, Room*> find_character_by_id(CharacterId id);


    // NOTE: generally, you should use render() intead of repaint().
    void repaint(Platform& pfrm, App& app);


    bool interior_visible() const
    {
        return interior_visible_;
    }


    // The origin used for collision checking and important stuff.
    Vec2<Fixnum> origin() const;


    // The origin with some added ambient movement, looks nice, but not
    // sufficient for calculating collision checking or anything like that,
    // mostly due to multiplayer, continuously moving things can get out of
    // sync.
    Vec2<Fixnum> visual_origin() const;


    using Terrain = Buffer<u8, 16>;


    Terrain& terrain()
    {
        return terrain_;
    }


    void render_terrain(Platform& pfrm);


    void set_float_timer(Microseconds value);


    void show_flag(bool show)
    {
        show_flag_ = show;
    }


    void set_drift(Platform& pfrm, App& app, Fixnum drift);


    Fixnum get_drift() const
    {
        return drift_;
    }


    u8 workshop_count() const
    {
        return workshop_count_;
    }


    u8 manufactory_count() const
    {
        return manufactory_count_;
    }


    u8 core_count() const
    {
        return core_count_;
    }


    EntityList<Entity>& projectiles()
    {
        return projectiles_;
    }


    void test_collision(Platform&, App&, Entity& entity);


    Player& owner()
    {
        return *owner_;
    }


    bool is_destroyed()
    {
        return destroyed_;
    }


    std::optional<RoomCoord> chimney_loc() const
    {
        return chimney_loc_;
    }


    Power power_supply() const
    {
        return power_supply_;
    }


    Power power_drain() const
    {
        return power_drain_;
    }


    void set_owner(Player& player)
    {
        owner_ = &player;
    }


    bool has_radar() const
    {
        return has_radar_;
    }


    bool is_boarded() const
    {
        return is_boarded_;
    }


    void on_layout_changed(App& app, const RoomCoord& room_added_removed_coord);


    SharedEntityList<Drone>& drones()
    {
        return drones_;
    }



    HitBox hitbox() const;


    std::optional<RoomCoord> flag_pos()
    {
        return flag_pos_;
    }


    const Bitmatrix<16, 16>& rooms_plot() const
    {
        return rooms_plot_;
    }


    void dispatch_room(Room* room);


    void cancel_dispatch();


    void show_groups(bool enabled);


    // The number of ways that the enemy has of attacking the player (weapons,
    // transporters, etc.).
    u8 offensive_capabilities() const;


    u8 character_count() const;


    // The y value of the top-most tile on the island.
    u8 min_y() const;


    void schedule_repaint()
    {
        schedule_repaint_ = true;
    }


    std::optional<Platform::DynamicTexturePtr> fire_texture();


    bool fire_present(const RoomCoord& coord) const;
    void fire_extinguish(Platform& pfrm, App& app, const RoomCoord& coord);
    void fire_create(Platform& pfrm, App& app, const RoomCoord& coord);


    const EntityList<BasicCharacter>& outdoor_characters()
    {
        return characters_;
    }


    RoomCoord critical_core_loc() const
    {
        return {critical_core_x_, critical_core_y_};
    }


private:
    void resolve_cancelled_dispatch();


    void recalculate_power_usage(App& app);


    void check_destroyed();


    Power power_supply_ = 0;
    Power power_drain_ = 0;

    Rooms rooms_;
    Room* dispatch_list_ = nullptr;
    const Layer layer_;
    Terrain terrain_;
    Vec2<Fixnum> position_;
    u8 ambient_movement_;
    Microseconds timer_;
    Fixnum drift_ = 0;

    bool interior_visible_ : 1;
    bool show_flag_ : 1;
    bool dispatch_cancelled_ : 1;
    bool show_groups_ : 1;
    bool schedule_repaint_ : 1;

    bool has_radar_ : 1;
    bool is_boarded_ : 1;
    bool hidden_ : 1;

    struct FireState
    {
        Bitmatrix<16, 16> positions_;
        std::optional<Platform::DynamicTexturePtr> texture_;
        Microseconds spread_timer_ = 0;
        Microseconds damage_timer_ = 0;
        Microseconds anim_timer_ = 0;
        s8 anim_index_ = 0;

        void
        update(Platform& pfrm, App& app, Island& island, Microseconds delta);

        void
        rewind(Platform& pfrm, App& app, Island& island, Microseconds delta);

        void display(Platform& pfrm, Island& island);

    } fire_;

    u8 workshop_count_ = 0;
    u8 manufactory_count_ = 0;
    u8 core_count_ = 0;
    u8 min_y_ = 0;

    // These parameters represent the location where a power core might possibly
    // be. Used during the death animation when placing the center of the
    // explosion effect, as the power core no longer exists.
    u8 critical_core_x_ : 4;
    u8 critical_core_y_ : 4;

    u8 character_count_ = 0;
    u8 offensive_capabilities_ = 0;

    bool destroyed_ = false;
    bool all_characters_awaiting_movement_ = false;

    // During repaint(), the game caches the results of plot_rooms() in this
    // matrix of bitflags. We use the result to speed up collision checking.
    Bitmatrix<16, 16> rooms_plot_;

    std::optional<RoomCoord> flag_pos_;
    Microseconds flag_anim_timer_ = 0;
    int flag_anim_index_;

    EntityList<BasicCharacter> characters_;
    EntityList<Entity> projectiles_;
    SharedEntityList<Drone> drones_;

    Player* owner_;

    std::optional<RoomCoord> chimney_loc_;
    Microseconds chimney_spawn_timer_ = 0;
};



void show_island_interior(Platform& pfrm, App& app, Island* island);
void show_island_exterior(Platform& pfrm, App& app, Island* island);



Island& player_island(App& app);
Island* opponent_island(App& app);



bool synth_notes_store(Platform& pfrm, Island& island, const char* path);



bool synth_notes_load(Platform& pfrm, Island& island, const char* path);



bool speaker_data_store(Platform& pfrm, Island& island, const char* path);



bool speaker_data_load(Platform& pfrm, Island& island, const char* path);


} // namespace skyland
