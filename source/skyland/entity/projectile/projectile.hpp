#pragma once


#include "skyland/entity.hpp"



namespace skyland {



class Projectile : public Entity {
public:
    Projectile(const HitBox::Dimension& dimension) : Entity(dimension)
    {
    }
};



} // namespace skyland
