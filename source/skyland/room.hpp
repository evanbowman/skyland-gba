#pragma once

#include "number/numeric.hpp"
#include "platform/layer.hpp"
#include "scene.hpp"
#include <memory>
#include "entity.hpp"



class Platform;



namespace skyland {



class App;
class Entity;
class Island;
class RoomMeta;



class Room {
public:
    using Health = int;


    virtual ~Room()
    {
    }


    Room(Island* parent,
         const char* name,
         const Vec2<u8>& size,
         const Vec2<u8>& position,
         Health health);


    virtual bool add_occupant(Entity* entity)
    {
        return false;
    }


    virtual void render_interior(Platform& pfrm, Layer layer) = 0;
    virtual void render_exterior(Platform& pfrm, Layer layer) = 0;


    void set_injured(Platform& pfrm);


    virtual void update(Platform& pfrm, App&, Microseconds delta);


    Island* other_island(App&);


    virtual bool has_roof()
    {
        return true;
    }


    virtual bool has_chimney()
    {
        return false;
    }


    const Vec2<u8>& position() const
    {
        return position_;
    }


    const Vec2<u8>& size() const
    {
        return size_;
    }


    virtual ScenePtr<Scene> select()
    {
        return null_scene();
    }


    virtual void set_target(const Vec2<u8>& target)
    {
    }


    RoomMeta* metaclass()
    {
        return metaclass_;
    }


    Vec2<Float> origin() const;


    Island* parent() const
    {
        return parent_;
    }


private:
    Island* parent_;
    RoomMeta* metaclass_;
    EntityList characters_;
    Vec2<u8> size_;
    Vec2<u8> position_;
    Health health_;
    Microseconds injured_timer_;
};



template <typename T> using RoomPtr = std::unique_ptr<T, void (*)(Room*)>;



} // namespace skyland
