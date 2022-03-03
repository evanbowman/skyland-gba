#pragma once

#include "skyland/entity.hpp"



namespace skyland {



class Island;



class Bird : public Entity {
public:
    using Entity::Entity;


    virtual ~Bird()
    {
    }


    virtual void signal(Platform& pfrm, App& app)
    {
    }


    virtual Island* island(App&)
    {
        return nullptr;
    }


    virtual Vec2<u8> coordinate()
    {
        return {};
    }
};



} // namespace skyland
