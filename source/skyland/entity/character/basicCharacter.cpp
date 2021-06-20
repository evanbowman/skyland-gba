#include "basicCharacter.hpp"
#include "skyland/island.hpp"



namespace skyland {



BasicCharacter::BasicCharacter(Island* parent, const Vec2<u8>& position)
    : Entity({{}, {}}), parent_(parent), grid_position_(position)
{
    sprite_.set_texture_index(26);
    sprite_.set_size(Sprite::Size::w16_h32);

    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 2;

    sprite_.set_position(o);
}



void BasicCharacter::update(Platform&, App&, Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 2;


    if (movement_path_) {

        static const auto movement_step_duration = milliseconds(400);

        movement_timer_ += delta;


        if (not(*movement_path_)->empty()) {
            auto dest_grid_pos = (*movement_path_)->back();
            auto dest = parent_->origin();
            dest.x += dest_grid_pos.x * 16;
            dest.y += dest_grid_pos.y * 16 - 2; // floor is two pixels thick

            sprite_.set_position(interpolate(
                dest, o, Float(movement_timer_) / movement_step_duration));
        }

        if (movement_timer_ > movement_step_duration) {
            movement_timer_ = 0;

            if (not(*movement_path_)->empty()) {
                grid_position_ = (*movement_path_)->back();
                (*movement_path_)->pop_back();
            } else {
                movement_path_.reset();
            }
        }
    } else {
        sprite_.set_position(o);
    }
}



} // namespace skyland
