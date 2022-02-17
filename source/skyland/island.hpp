#pragma once

#include "bulkAllocator.hpp"
#include "entity.hpp"
#include "entity/character/basicCharacter.hpp"
#include "entity/drones/drone.hpp"
#include "memory/buffer.hpp"
#include "player/player.hpp"
#include "power.hpp"
#include "room.hpp"
#include "roomTable.hpp"
#include "room_alloc.hpp"
#include "skyland/tile.hpp"



namespace skyland {


class Island {
public:
    Island(Platform& pfrm, Layer layer, u8 width, Player& owner);
    Island(const Island&) = delete;


    using Rooms = RoomTable<92, 16>;


    bool add_room(Platform& pfrm, App& app, RoomPtr<Room> insert)
    {
        auto result = rooms_.insert_room(std::move(insert));
        repaint(pfrm, app);
        recalculate_power_usage();
        on_layout_changed(app, insert->position());
        return result;
    }


    template <typename T, typename... Args>
    bool
    add_room(Platform& pfrm, App& app, const Vec2<u8>& position, Args&&... args)
    {
        if (auto room = room_pool::alloc<T>(
                this, position, std::forward<Args>(args)...)) {
            if (rooms_.insert_room({room.release(), room_pool::deleter})) {
                repaint(pfrm, app);
                recalculate_power_usage();
                on_layout_changed(app, position);
                return true;
            }
        }
        pfrm.fatal("room pool exhausted");
        return false;
    }


    void init_terrain(Platform& pfrm, int width);


    bool add_character(EntityRef<BasicCharacter> character);


    void remove_character(const Vec2<u8>& location);


    Rooms& rooms();


    void clear_rooms(Platform&, App&);


    void update(Platform&, App&, Microseconds delta);


    void rewind(Platform&, App&, Microseconds delta);


    void display(Platform&);


    const Vec2<Float>& get_position() const;


    void set_position(const Vec2<Float>& position);


    Room* get_room(const Vec2<u8>& coord);


    std::optional<SharedEntityRef<Drone>> get_drone(const Vec2<u8>& coord);


    void destroy_room(Platform& pfrm, App& app, const Vec2<u8>& coord);


    u8 get_ambient_movement()
    {
        return ambient_movement_;
    }


    Layer layer() const
    {
        return layer_;
    }


    void render_interior(Platform& pfrm, App& app);


    void render_exterior(Platform& pfrm, App& app);


    void plot_rooms(u8 matrix[16][16]) const;


    void plot_construction_zones(bool matrix[16][16]) const;


    void plot_walkable_zones(App& app, bool matrix[16][16]) const;


    BasicCharacter* character_at_location(const Vec2<u8>& loc);


    std::pair<BasicCharacter*, Room*> find_character_by_id(CharacterId id);


    void repaint(Platform& pfrm, App& app);


    bool interior_visible() const
    {
        return interior_visible_;
    }


    // The origin used for collision checking and important stuff.
    Vec2<Float> origin() const;


    // The origin with some added ambient movement, looks nice, but not
    // sufficient for calculating collision checking or anything like that,
    // mostly due to multiplayer, continuously moving things can get out of
    // sync.
    Vec2<Float> visual_origin() const;


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


    void set_drift(Platform& pfrm, App& app, Float drift);


    Float get_drift() const
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


    std::optional<Vec2<u8>> chimney_loc() const
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


    void on_layout_changed(App& app, const Vec2<u8>& room_added_removed_coord);


    SharedEntityList<Drone>& drones()
    {
        return drones_;
    }



    HitBox hitbox() const;


    std::optional<Vec2<u8>> flag_pos()
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


private:
    void resolve_cancelled_dispatch();


    void recalculate_power_usage();


    void check_destroyed();


    Power power_supply_ = 0;
    Power power_drain_ = 0;

    Rooms rooms_;
    Room* dispatch_list_ = nullptr;
    const Layer layer_;
    Terrain terrain_;
    Vec2<Float> position_;
    u8 ambient_movement_;
    Microseconds timer_;
    Float drift_ = 0;

    bool interior_visible_;
    bool show_flag_ = false;
    bool dispatch_cancelled_ = false;
    bool show_groups_ = false;

    bool has_radar_ = false;
    bool is_boarded_ = false;
    u8 workshop_count_ = 0;
    u8 manufactory_count_ = 0;
    u8 core_count_ = 0;

    bool destroyed_ = false;
    bool all_characters_awaiting_movement_ = false;

    // During repaint(), the game caches the results of plot_rooms() in this
    // matrix of bitflags. We use the result to speed up collision checking.
    Bitmatrix<16, 16> rooms_plot_;

    std::optional<Vec2<u8>> flag_pos_;
    Microseconds flag_anim_timer_ = 0;
    int flag_anim_index_ = Tile::flag_start;

    EntityList<BasicCharacter> characters_;
    EntityList<Entity> projectiles_;
    SharedEntityList<Drone> drones_;

    Player* owner_;

    std::optional<Vec2<u8>> chimney_loc_;
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
