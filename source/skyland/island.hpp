#pragma once

#include "bulkAllocator.hpp"
#include "entity.hpp"
#include "entity/character/basicCharacter.hpp"
#include "memory/buffer.hpp"
#include "player.hpp"
#include "power.hpp"
#include "room.hpp"
#include "roomPool.hpp"



namespace skyland {


class Island {
public:
    Island(Platform& pfrm, Layer layer, u8 width, Player& owner);
    Island(const Island&) = delete;


    using Rooms = Buffer<RoomPtr<Room>, 40>;


    bool add_room(Platform& pfrm, RoomPtr<Room> insert)
    {
        auto result = rooms_.push_back(std::move(insert));
        repaint(pfrm);
        recalculate_power_usage();
        on_layout_changed(insert->position());
        return result;
    }


    template <typename T>
    bool add_room(Platform& pfrm, const Vec2<u8>& position)
    {
        if (auto room = room_pool::alloc<T>(this, position)) {
            if (rooms_.push_back({room.release(), room_pool::deleter})) {
                repaint(pfrm);
                recalculate_power_usage();
                on_layout_changed(position);
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


    void update(Platform&, App&, Microseconds delta);


    void display(Platform&);


    const Vec2<Float>& get_position() const;


    void set_position(const Vec2<Float>& position);


    Room* get_room(const Vec2<u8>& coord);


    void destroy_room(Platform& pfrm, const Vec2<u8>& coord);


    u8 get_ambient_movement()
    {
        return ambient_movement_;
    }


    Layer layer() const
    {
        return layer_;
    }


    void flip(Platform& pfrm);


    void render_interior(Platform& pfrm);


    void render_exterior(Platform& pfrm);


    void plot_rooms(u8 matrix[16][16]) const;


    void plot_construction_zones(bool matrix[16][16]) const;


    void plot_walkable_zones(bool matrix[16][16]) const;


    BasicCharacter* character_at_location(const Vec2<u8>& loc);


    void repaint(Platform& pfrm);


    bool interior_visible() const
    {
        return interior_visible_;
    }


    Vec2<Float> origin() const;


    using Terrain = Buffer<u8, 11>;


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


    void set_drift(Float drift);


    Float get_drift() const
    {
        return drift_;
    }


    int workshop_count()
    {
        return workshop_count_;
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


    void on_layout_changed(const Vec2<u8>& room_added_removed_coord);


private:
    void recalculate_power_usage();


    Power power_supply_ = 0;
    Power power_drain_ = 0;

    Rooms rooms_;
    const Layer layer_;
    Buffer<u8, 11> terrain_;
    Vec2<Float> position_;
    u8 ambient_movement_;
    Microseconds timer_;
    Float drift_ = 0;

    int power_available_ = 0;
    int power_used_ = 0;

    bool interior_visible_;
    bool show_flag_ = false;

    bool has_radar_ = false;
    bool is_boarded_ = false;
    int workshop_count_ = 0;

    bool destroyed_ = false;
    bool all_characters_awaiting_movement_ = false;

    std::optional<Vec2<u8>> flag_pos_;
    Microseconds flag_anim_timer_ = 0;

    EntityList<BasicCharacter> characters_;
    EntityList<Entity> projectiles_;

    Player* owner_;

    std::optional<Vec2<u8>> chimney_loc_;
    Microseconds chimney_spawn_timer_ = 0;
};



} // namespace skyland
