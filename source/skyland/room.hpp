#pragma once

#include <memory>
#include "number/numeric.hpp"
#include "platform/layer.hpp"



class Platform;



namespace skyland {



class App;
class Entity;
class Island;



class Room {
public:
    virtual ~Room() {}

    Room(Island* parent, const Vec2<u8>& size, const Vec2<u8>& position) :
        parent_(parent),
        size_(size),
        position_(position)
    {
    }


    virtual bool add_occupant(Entity* entity)
    {
        return false;
    }


    virtual void render_interior(Platform& pfrm, Layer layer) = 0;
    virtual void render_exterior(Platform& pfrm, Layer layer) = 0;


    virtual void update(Platform& pfrm, App&, Microseconds delta) = 0;


    const Vec2<u8>& position() const
    {
        return position_;
    }


    const Vec2<u8>& size() const
    {
        return size_;
    }


private:
    Island* parent_;
    Vec2<u8> size_;
    Vec2<u8> position_;
};



template <typename T>
using RoomPtr = std::unique_ptr<T, void (*)(Room*)>;



}
