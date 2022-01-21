#pragma once


#include "skyland/entity.hpp"



namespace skyland {



class Projectile : public Entity {
public:
    Projectile(const HitBox::Dimension& dimension) : Entity(dimension)
    {
    }


    virtual void rewind(Platform& pfrm, App& app, Microseconds delta)
    {
    }
};



} // namespace skyland
