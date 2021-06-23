#include "basicCharacter.hpp"
#include "skyland/island.hpp"



namespace skyland {



BasicCharacter::BasicCharacter(Island* parent,
                               Player* owner,
                               const Vec2<u8>& position)
    : Entity({{}, {}}),
      parent_(parent),
      owner_(owner),
      grid_position_(position)
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

        if (awaiting_movement_ and not can_move_) {
            // ... we're waiting to be told that we can move. Because movement
            // is grid-based
            sprite_.set_position(o);
        } else {
            awaiting_movement_ = false;
            can_move_ = false;

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

                awaiting_movement_ = true;
                can_move_ = false;

                if (not(*movement_path_)->empty()) {
                    grid_position_ = (*movement_path_)->back();
                    (*movement_path_)->pop_back();
                } else {
                    movement_path_.reset();
                }
            }
        }
    } else {
        awaiting_movement_ = true;
        sprite_.set_position(o);
    }
}



} // namespace skyland
