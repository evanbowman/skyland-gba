////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"
#include "blockChecksum.hpp"
#include "bulkTimer.hpp"
#include "entity.hpp"
#include "entity/character/character.hpp"
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
    Island(Layer layer, u8 width, Player& owner);
    Island(const Island&) = delete;


    using Rooms = RoomTable<92, 16>;


    bool add_room(RoomPtr<Room> insert, bool do_repaint = true)
    {
        if (rooms().full()) {
            return false;
        }
        auto result = rooms_.insert_room(std::move(insert));
        if (do_repaint) {
            repaint();
        }
        recalculate_power_usage();
        on_layout_changed(insert->position());
        schedule_recompute_deflector_shields();
        return result;
    }


    template <typename T, typename... Args>
    bool add_room(const RoomCoord& position, bool do_repaint, Args&&... args)
    {
        if (rooms().full()) {
            return false;
        }
        if (auto room = room_pool::alloc<T>(
                this, position, std::forward<Args>(args)...)) {
            if (rooms_.insert_room({room.release(), room_pool::deleter})) {
                if (do_repaint) {
                    repaint();
                }
                recalculate_power_usage();
                on_layout_changed(position);
                schedule_recompute_deflector_shields();
                return true;
            }
        }
        PLATFORM.fatal("room pool exhausted");
        return false;
    }


    void move_room(const RoomCoord& from, const RoomCoord& to);


    void init_terrain(int width, bool render = true);


    bool add_character(EntityRef<Character> character);


    void remove_character(const RoomCoord& location);


    Rooms& rooms();


    void clear_rooms();


    void clear();


    void update(Time delta);
    void update_simple(Time delta);


    void rewind(Time delta);


    void display();
    void display_fires();

    const Vec2<Fixnum>& get_position() const;


    void set_position(const Vec2<Fixnum>& position);


    Room* get_room(const RoomCoord& coord);


    Optional<SharedEntityRef<Drone>> get_drone(const RoomCoord& coord);


    void destroy_room(const RoomCoord& coord);


    s8 get_ambient_movement()
    {
        return ambient_movement_;
    }


    Layer layer() const
    {
        return layer_;
    }


    void set_hidden(bool hidden);


    void render_interior();
    void render_interior_fast();


    void render_exterior();


    void render();


    void plot_rooms(u8 matrix[16][16]) const;


    void plot_construction_zones(bool matrix[16][16]) const;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) const;


    Character* character_at_location(const RoomCoord& loc);


    std::pair<Character*, Room*> find_character_by_id(CharacterId id);


    // NOTE: generally, you should use render() intead of repaint().
    void repaint();


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


    void render_terrain();


    void set_float_timer(Time value);


    void show_flag(bool show)
    {
        show_flag_ = show;
    }


    void set_drift(Fixnum drift);


    Fixnum get_drift() const
    {
        return drift_;
    }


    u8 instance_count(MetaclassIndex idx) const;


    u8 workshop_count() const;


    u8 manufactory_count() const;


    u8 core_count() const
    {
        return core_count_;
    }


    EntityList<Entity>& projectiles()
    {
        return projectiles_;
    }


    void test_collision(Entity& entity);


    Player& owner()
    {
        return *owner_;
    }


    bool is_destroyed()
    {
        return destroyed_;
    }


    Optional<RoomCoord> chimney_loc() const
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


    void on_layout_changed(const RoomCoord& room_added_removed_coord);


    SharedEntityList<Drone>& drones()
    {
        return drones_;
    }



    HitBox hitbox() const;


    Optional<RoomCoord> flag_pos()
    {
        return flag_pos_;
    }


    const Bitmatrix<16, 16>& rooms_plot() const
    {
        return rooms_plot_;
    }


    void dispatch_room(Room* room);
    void drawfirst(Room* room);


    void cancel_dispatch();


    // The number of ways that the enemy has of attacking the player (weapons,
    // transporters, etc.).
    u8 offensive_capabilities() const;


    u8 character_count() const;


    // The y value of the top-most tile on the island.
    u8 min_y() const;


    u16 script_userdata_tag() const;


    void schedule_repaint()
    {
        schedule_repaint_ = true;
    }


    void schedule_repaint_partial()
    {
        schedule_repaint_partial_ = true;
    }


    Optional<Platform::DynamicTexturePtr> fire_texture();


    bool fire_present(const RoomCoord& coord) const;
    void fire_extinguish(const RoomCoord& coord);
    void fire_create(const RoomCoord& coord);

    void fires_extinguish();


    const EntityList<Character>& outdoor_characters()
    {
        return characters_;
    }


    RoomCoord critical_core_loc() const
    {
        return {critical_core_x_, critical_core_y_};
    }


    BlockChecksum checksum() const
    {
        return checksum_;
    }


    void init_ai_awareness();


    BulkTimer& bulk_timer()
    {
        return bulk_timer_;
    }


    void set_custom_flag_graphics(u8 val)
    {
        custom_flag_graphics_ = val;
    }


    u8 custom_flag_graphics() const
    {
        return custom_flag_graphics_;
    }


    void recalculate_power_usage();


    void show_powerdown_opts(bool show);


    void schedule_recompute_deflector_shields();


    void set_phase(u8 phase);


    u8 phase() const;


    void unamplify_blocks();


    void set_mountain_terrain(bool enabled);


private:
    void recompute_deflector_shields();


    void repaint_partial();


    bool repaint_alloc_tiles(TileId buffer[16][16], bool retry);


    void resolve_cancelled_dispatch();


    void check_destroyed();


    void update_target_queues();


    int smoke_sprite();


    BulkTimer bulk_timer_;

    EntityList<Character> characters_;
    EntityList<Entity> projectiles_;
    SharedEntityList<Drone> drones_;

    Player* owner_;

    Rooms rooms_;
    Room* dispatch_list_ = nullptr;
    Room* drawfirst_ = nullptr;
    const Layer layer_;
    Terrain terrain_;
    Time chimney_spawn_timer_ = 0;
    Time flag_anim_timer_ = 0;
    Time timer_;
    Vec2<Fixnum> position_;
    Fixnum drift_ = 0.0_fixed;

    struct FireState
    {
        Bitmatrix<16, 16> positions_;
        Optional<Platform::DynamicTexturePtr> texture_;
        Time spread_timer_ = 0;
        Time damage_timer_ = 0;
        Time anim_timer_ = 0;
        s8 anim_index_ = 0;

        void update(Island& island, Time delta);

        void rewind(Island& island, Time delta);

        void display(Island& island);

    } fire_;

    Power power_supply_ = 0;
    Power power_drain_ = 0;

    BlockChecksum checksum_ = 0;

    u8 flag_anim_index_;
    u8 core_count_ = 0;
    u8 min_y_ = 0;
    s8 ambient_movement_;

    // These parameters represent the location where a power core might possibly
    // be. Used during the death animation when placing the center of the
    // explosion effect, as the power core no longer exists.
    u8 critical_core_x_ : 4;
    u8 critical_core_y_ : 4;

    u8 character_count_ = 0;
    u8 offensive_capabilities_ = 0;
    u8 custom_flag_graphics_ = 0;

    bool destroyed_ = false;
    bool all_characters_awaiting_movement_ = false;

    bool interior_visible_ : 1;
    bool show_flag_ : 1;
    bool dispatch_cancelled_ : 1;
    bool schedule_repaint_ : 1;
    bool schedule_repaint_partial_ : 1;

    bool has_radar_ : 1;
    bool is_boarded_ : 1;
    bool hidden_ : 1;
    bool show_powerdown_opts_ : 1 = false;
    bool should_recompute_deflector_shields_ : 1 = false;
    bool dark_smoke_ : 1 = false;
    bool mountain_terrain_ : 1 = false;
    u8 phase_ : 1 = 0;

    // During repaint(), the game caches the results of plot_rooms() in this
    // matrix of bitflags. We use the result to speed up collision checking.
    Bitmatrix<16, 16> rooms_plot_;

    Optional<RoomCoord> flag_pos_;

    Optional<RoomCoord> chimney_loc_;
};



void show_island_interior(Island* island);
void show_island_exterior(Island* island);

void show_island(Island* island);



Island& player_island();
Island* opponent_island();



bool is_player_island(const Island* isle);



BlockChecksum opponent_island_checksum();
BlockChecksum island_checksums();



bool synth_notes_store(Island& island, const char* path);



bool synth_notes_load(Island& island, const char* path);



bool speaker_data_store(Island& island, const char* path);



bool speaker_data_load(Island& island, const char* path);



void collect_outer_rooms(Island& isle, Vector<Room*>& output);



void show_phase();


void hide_translucence();


} // namespace skyland
