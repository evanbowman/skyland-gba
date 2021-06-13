#pragma once

#include "memory/buffer.hpp"
#include "room.hpp"
#include "bulkAllocator.hpp"
#include "roomPool.hpp"



namespace skyland {



class Island {
public:
    Island(Platform& pfrm, Layer layer, u8 width);


    using Rooms = Buffer<RoomPtr<Room>, 20>;


    template <typename T>
    bool add_room(Platform& pfrm, const Vec2<u8>& position)
    {
        if (auto room = room_pool::alloc<T>(this, position)) {
            if (rooms_.push_back({room.release(), room_pool::deleter})) {
                repaint(pfrm);
                return true;
            }
        }
        return false;
    }


    Rooms& rooms();


    void update(Platform&, App&, Microseconds delta);


    const Vec2<Float>& get_position() const;


    void set_position(const Vec2<Float>& position);


    u8 get_ambient_movement()
    {
        return ambient_movement_;
    }


    Layer layer() const
    {
        return layer_;
    }


    void render_interior(Platform& pfrm);


    void render_exterior(Platform& pfrm);


    void plot_rooms(bool matrix[16][16]) const;


    void plot_construction_zones(bool matrix[16][16]) const;


    void repaint(Platform& pfrm);


    bool interior_visible() const
    {
        return interior_visible_;
    }


private:


    void render_terrain(Platform& pfrm);


    Rooms rooms_;
    const Layer layer_;
    Buffer<u8, 10> terrain_;
    Vec2<Float> position_;
    u8 ambient_movement_;
    Microseconds timer_;

    bool interior_visible_;
};



}
