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


    switch (state_) {
    case State::fighting:
        awaiting_movement_ = true;
        can_move_ = false;
        break;

    case State::moving_or_idle:
        if (movement_path_) {
            if (awaiting_movement_ and not can_move_) {
                // ... we're waiting to be told that we can move. Because movement
                // is grid-based
                sprite_.set_position(o);
            } else {
                movement_step(delta);
            }
        } else {
            awaiting_movement_ = true;
            can_move_ = false;
            sprite_.set_position(o);
        }
        break;
    }
}


void BasicCharacter::movement_step(Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 2;

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
            const auto current_position = (*movement_path_)->back();

            // Now, we've moved to a new location. We want to re-assign
            // ourself, having moved from one room to another.
            reassign_room(grid_position_, current_position);

            (*movement_path_)->pop_back();
        } else {
            movement_path_.reset();
        }
    }
}



void BasicCharacter::reassign_room(const Vec2<u8>& old_coord,
                                   const Vec2<u8>& new_coord)
{
    if (auto room = parent_->get_room(old_coord)) {

        std::optional<EntityRef<BasicCharacter>> self;

        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            if (it->get() == this) {
                self = std::move(*it);
                it = room->characters().erase(it);
            } else {
                ++it;
            }
        }

        if (self) {
            if (auto room = parent_->get_room(new_coord)) {
                room->characters().push(std::move(*self));
            }
        }
    }

    // Yup. Aliasing between member var and fn arg. You better believe the
    // compiler isn't doing certain optimizations on my garbage code.
    grid_position_ = new_coord;
}



} // namespace skyland
