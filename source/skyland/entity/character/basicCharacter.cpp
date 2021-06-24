#include "basicCharacter.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "localization.hpp"



namespace skyland {



static u16 base_frame(BasicCharacter* character, App& app)
{
    if (character->owner() == &app.player()) {
        return 35;
    } else {
        return 42;
    }
}



BasicCharacter::BasicCharacter(Island* parent,
                               Player* owner,
                               const Vec2<u8>& position)
    : Entity({{}, {}}),
      parent_(parent),
      owner_(owner),
      grid_position_(position)
{
    sprite_.set_texture_index(40);
    sprite_.set_size(Sprite::Size::w16_h32);

    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    sprite_.set_position(o);

    awaiting_movement_ = true;
    can_move_ = false;
}



void BasicCharacter::set_can_move()
{
    // if (not can_move_ and
    //     state_ == State::moving_or_idle) {
    //     state_ = State::check_surroundings;
    // }

    can_move_ = true;
}



void BasicCharacter::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    switch (state_) {
    case State::fighting:
        update_attack(delta, app);
        break;

    case State::moving_or_idle:
        if (movement_path_) {
            if (awaiting_movement_ and not can_move_) {
                // ... we're waiting to be told that we can move. Because movement
                // is grid-based
                sprite_.set_position(o);
            } else {
                timer_ += delta;
                movement_step(delta);
                anim_timer_ += delta;
                if (anim_timer_ > milliseconds(100)) {
                    anim_timer_ = 0;
                    auto index = sprite_.get_texture_index();
                    if (index == base_frame(this, app) + 5) {
                        index = base_frame(this, app) + 4;
                    } else {
                        index = base_frame(this, app) + 5;
                    }
                    sprite_.set_texture_index(index);
                }
            }
        } else {
            awaiting_movement_ = true;
            can_move_ = false;
            sprite_.set_position(o);
            sprite_.set_texture_index(base_frame(this, app) + 5);
        }
        break;
    }
}


void BasicCharacter::update_attack(Microseconds delta, App& app)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    awaiting_movement_ = true;
    can_move_ = false;
    sprite_.set_position(o);
    timer_ += delta;
    if (timer_ > milliseconds(300)) {
        timer_ = 0;
        if (sprite_.get_texture_index() == base_frame(this, app)) {
            sprite_.set_texture_index(base_frame(this, app) + 5);
        } else {
            sprite_.set_texture_index(base_frame(this, app));
        }
    }
    if (movement_path_) {
        state_ = State::moving_or_idle;
        timer_ = 0;
    }
}



void BasicCharacter::movement_step(Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    awaiting_movement_ = false;
    can_move_ = false;

    static const auto movement_step_duration = milliseconds(300);


    if (not(*movement_path_)->empty()) {
        auto dest_grid_pos = (*movement_path_)->back();
        auto dest = parent_->origin();
        dest.x += dest_grid_pos.x * 16;
        dest.y += dest_grid_pos.y * 16 - 3; // floor is two pixels thick

        if (dest_grid_pos.x < grid_position_.x) {
            sprite_.set_flip({false, false});
        } else if (dest_grid_pos.x > grid_position_.x) {
            sprite_.set_flip({true, false});
        }

        sprite_.set_position(interpolate(
            dest, o, Float(timer_) / movement_step_duration));
    }

    if (timer_ > movement_step_duration) {
        timer_ = 0;

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
