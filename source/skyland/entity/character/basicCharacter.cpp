#include "basicCharacter.hpp"
#include "skyland/island.hpp"



namespace skyland {



BasicCharacter::BasicCharacter(Island* parent, const Vec2<u8>& position) :
    Entity({{}, {}}),
    parent_(parent),
    grid_position_(position)
{
    sprite_.set_texture_index(26);
    sprite_.set_size(Sprite::Size::w16_h32);
}



void BasicCharacter::update(Platform&, App&, Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16;

    if (not outdoors_) {
        o.y -= 2; // The floor is two pixels thick
    }

    sprite_.set_position(o);
}



}
