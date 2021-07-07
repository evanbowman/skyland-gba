#include "basicCharacter.hpp"
#include "localization.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



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
                               const Vec2<u8>& position,
                               bool is_replicant)
    : Entity({{}, {}}), parent_(parent), owner_(owner), grid_position_(position)
{
    sprite_.set_texture_index(40);
    sprite_.set_size(Sprite::Size::w16_h32);

    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    sprite_.set_position(o);

    awaiting_movement_ = true;
    can_move_ = false;

    is_replicant_ = is_replicant;

    health_ = 255;
}



void BasicCharacter::set_can_move()
{
    can_move_ = true;
}



void BasicCharacter::transported()
{
    state_ = State::after_transport;
    anim_timer_ = 0;
    sprite_.set_mix({ColorConstant::electric_blue, 255});
    idle_count_ = 0;
}



void BasicCharacter::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto o = parent_->origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;


    auto idle = [&] {
        sprite_.set_texture_index(base_frame(this, app) + 5);
        state_ = State::moving_or_idle;
        timer_ = 0;
        idle_count_ = 0;
    };

    auto has_opponent = [&](Room* room) {
        for (auto& character : room->characters()) {
            if (character->owner() not_eq owner() and
                character->grid_position() == grid_position_) {
                return true;
            }
        }
        return false;
    };


    switch (state_) {
    case State::fighting:
        sprite_.set_flip({});
        update_attack(delta, app);
        break;

    case State::after_transport: {
        anim_timer_ += delta;
        if (anim_timer_ < milliseconds(500)) {
            u8 fade_amt =
                255 *
                interpolate(255, 0, Float(anim_timer_) / milliseconds(500));
            sprite_.set_mix({ColorConstant::electric_blue, fade_amt});
        } else {
            anim_timer_ = 0;
            sprite_.set_mix({});
            state_ = State::moving_or_idle;
        }

        sprite_.set_position(o);
        break;
    }

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
        } else /* no movement path */ {
            awaiting_movement_ = true;
            can_move_ = false;
            sprite_.set_position(o);
            sprite_.set_texture_index(base_frame(this, app) + 5);
            ++idle_count_;

            if (auto room = parent_->get_room(grid_position_)) {


                auto metac = room->metaclass();

                if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                } else {
                    const bool is_plundered =
                        str_cmp((*metac)->name(), "plundered-room") == 0;
                    const bool is_stairwell =
                        str_cmp((*metac)->name(), "stairwell") == 0;

                    if (&room->parent()->owner() not_eq owner() and
                        not is_plundered and not is_stairwell) {
                        state_ = State::plunder_room;
                        timer_ = 0;
                    } else if (&room->parent()->owner() == owner() and
                               not is_plundered and
                               room->health() < room->max_health()) {
                        state_ = State::repair_room;
                        anim_timer_ = 0;
                    }
                }
            }
        }
        break;

    case State::plunder_room:
        awaiting_movement_ = true;
        can_move_ = false;

        sprite_.set_position(o);

        if (movement_path_) {
            idle();
            break;
        }

        sprite_.set_flip({});

        anim_timer_ += delta;
        if (anim_timer_ > milliseconds(200)) {
            anim_timer_ = 0;
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this, app) + 5) {
                index = base_frame(this, app) + 1;
            } else {
                index = base_frame(this, app) + 5;
            }
            sprite_.set_texture_index(index);
        }

        timer_ += delta;
        if (timer_ > milliseconds(500)) {
            timer_ = 0;
            if (auto room = parent_->get_room(grid_position_)) {
                auto metac = room->metaclass();
                const bool is_plundered =
                    str_cmp((*metac)->name(), "plundered-room") == 0;

                if (is_plundered) { // We've successfully ransacked the room,
                                    // let's try moving on to somewhere else.
                    idle();
                    break;
                }

                room->plunder(pfrm, app, 2);

                if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                    break;
                }
            }
        }

        break;


    case State::repair_room:
        awaiting_movement_ = true;
        can_move_ = false;

        sprite_.set_flip({});

        sprite_.set_position(o);

        anim_timer_ += delta;
        if (anim_timer_ > milliseconds(200)) {
            anim_timer_ = 0;
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this, app) + 5) {
                index = base_frame(this, app) + 1;
            } else {
                index = base_frame(this, app) + 5;
            }
            sprite_.set_texture_index(index);
        }

        if (movement_path_) {
            idle();
            break;
        }

        timer_ += delta;
        if (timer_ > milliseconds(500)) {
            timer_ = 0;
            if (auto room = parent_->get_room(grid_position_)) {
                if (room->health() not_eq room->max_health()) {
                    room->heal(2);
                } else {
                    idle();
                    break;
                }

                if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                    break;
                }
            }
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

        auto get_opponent = [&]() -> BasicCharacter* {
            if (auto room = parent_->get_room(grid_position_)) {
                for (auto& chr : room->characters()) {
                    if (chr->grid_position() == grid_position_ and
                        chr->owner() not_eq owner()) {
                        return chr.get();
                    }
                }
            }
            return nullptr;
        };

        if (auto chr = get_opponent()) {
            chr->apply_damage(4);
        } else {
            sprite_.set_texture_index(base_frame(this, app) + 5);
            state_ = State::moving_or_idle;
            timer_ = 0;
            idle_count_ = 0;
        }

        timer_ = 0;
        if (sprite_.get_texture_index() == base_frame(this, app)) {
            sprite_.set_texture_index(base_frame(this, app) + 5);
        } else {
            sprite_.set_texture_index(base_frame(this, app));
        }
    }
    if (movement_path_) {
        sprite_.set_texture_index(base_frame(this, app) + 5);
        state_ = State::moving_or_idle;
        timer_ = 0;
        idle_count_ = 0;
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

        sprite_.set_position(
            interpolate(dest, o, Float(timer_) / movement_step_duration));
    }

    if (timer_ > movement_step_duration) {
        timer_ = 0;

        awaiting_movement_ = true;
        can_move_ = false;

        if (not(*movement_path_)->empty()) {
            const auto current_position = (*movement_path_)->back();

            // Now, we've moved to a new location. We want to re-assign
            // ourself, having (potentially) moved from one room to another.
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
