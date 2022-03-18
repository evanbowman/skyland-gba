#include "basicCharacter.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static const auto movement_step_duration = milliseconds(300);



static u16 base_frame(BasicCharacter* character, App& app)
{
    if (character->owner() == &app.player()) {
        return 35;
    } else {
        return 42;
    }
}



static CharacterId character_id_generator = 1;



static CharacterId alloc_character_id()
{
    return character_id_generator++;
}



BasicCharacter::BasicCharacter(Island* parent,
                               Player* owner,
                               const Vec2<u8>& position,
                               bool is_replicant)
    : Entity({{}, {}}), parent_(parent), owner_(owner),
      grid_position_(position), id_(alloc_character_id())
{
    sprite_.set_texture_index(40);
    sprite_.set_size(Sprite::Size::w16_h32);

    auto o = parent_->visual_origin();
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



void BasicCharacter::rewind(Platform&, App& app, Microseconds delta)
{
    auto o = parent_->visual_origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    sprite_.set_position(o);

    sprite_.set_texture_index(base_frame(this, app) + 5);

    auto has_opponent = [&](Room* room) {
        for (auto& character : room->characters()) {
            if (character->owner() not_eq owner() and
                character->grid_position() == grid_position_) {
                return true;
            }
        }
        return false;
    };

    if (movement_path_ and not(*movement_path_)->empty()) {
        auto dest_grid_pos = (*movement_path_)->back();
        auto dest = parent_->visual_origin();
        dest.x += dest_grid_pos.x * 16;
        dest.y += dest_grid_pos.y * 16 - 3; // floor is two pixels thick

        if (dest_grid_pos.x < grid_position_.x) {
            sprite_.set_flip({false, false});
        } else if (dest_grid_pos.x > grid_position_.x) {
            sprite_.set_flip({true, false});
        }

        timer_ -= delta;

        if (timer_ > 0) {
            sprite_.set_position(
                interpolate(dest, o, Float(timer_) / movement_step_duration));
        }

        anim_timer_ -= delta;
        if (anim_timer_ < 0) {
            anim_timer_ = milliseconds(100);
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this, app) + 5) {
                index = base_frame(this, app) + 4;
            } else {
                index = base_frame(this, app) + 5;
            }
            sprite_.set_texture_index(index);
        }

    } else {
        if (auto room = parent_->get_room(grid_position_)) {
            if (has_opponent(room)) {
                sprite_.set_texture_index(base_frame(this, app));
                sprite_.set_flip({});
            } else if (room->health() not_eq room->max_health()) {
                sprite_.set_texture_index(base_frame(this, app) + 1);
                sprite_.set_flip({});
            } else {
                sprite_.set_texture_index(base_frame(this, app) + 5);
            }
        }
    }
}



void BasicCharacter::set_idle(App& app)
{
    sprite_.set_texture_index(base_frame(this, app) + 5);
    state_ = State::moving_or_idle;
    timer_ = 0;
    idle_count_ = 0;
}



void BasicCharacter::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto o = parent_->visual_origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;


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
        update_attack(pfrm, app, delta);
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
                movement_step(pfrm, app, delta);
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
                        str_eq((*metac)->name(), "plundered-room");
                    const bool is_stairwell =
                        str_eq((*metac)->name(), "stairwell");
                    const bool is_bridge = str_eq((*metac)->name(), "bridge");

                    if (&room->parent()->owner() not_eq owner() and
                        not is_plundered and not is_stairwell and
                        not is_bridge) {
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
            this->set_idle(app);
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
                    this->set_idle(app);
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
            this->set_idle(app);
            break;
        }

        timer_ += delta;
        if (timer_ > milliseconds(500)) {
            timer_ = 0;
            if (auto room = parent_->get_room(grid_position_)) {
                if (room->health() not_eq room->max_health()) {
                    room->heal(pfrm, app, 2);
                } else {
                    this->set_idle(app);
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


void BasicCharacter::update_attack(Platform& pfrm, App& app, Microseconds delta)
{
    auto o = parent_->visual_origin();
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
            chr->apply_damage(pfrm, app, 4);
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



void BasicCharacter::movement_step(Platform& pfrm, App& app, Microseconds delta)
{
    auto o = parent_->visual_origin();
    o.x += grid_position_.x * 16;
    o.y += grid_position_.y * 16 - 3;

    awaiting_movement_ = false;
    can_move_ = false;


    if (not(*movement_path_)->empty()) {
        auto dest_grid_pos = (*movement_path_)->back();
        auto dest = parent_->visual_origin();
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

            time_stream::event::CharacterMoved e;
            e.id_.set(id_);
            e.previous_x_ = grid_position_.x;
            e.previous_y_ = grid_position_.y;
            e.near_ = parent_ == &app.player_island();

            app.time_stream().push(pfrm, app.level_timer(), e);

            // Now, we've moved to a new location. We want to re-assign
            // ourself, having (potentially) moved from one room to another.
            reassign_room(grid_position_, current_position);

            (*movement_path_)->pop_back();
        } else {
            movement_path_.reset();
        }
    }
}



void BasicCharacter::set_movement_path(Platform& pfrm, App& app, Path path)
{
    time_stream::event::CharacterMovementPathAssigned e;
    e.id_.set(id_);
    e.near_ = parent_ == &app.player_island();
    app.time_stream().push(pfrm, app.level_timer(), e);

    movement_path_ = std::move(path);
}



void BasicCharacter::rewind_movement_step(Platform& pfrm,
                                          const Vec2<u8>& new_pos)
{
    if (not movement_path_) {
        movement_path_.emplace(allocate_dynamic<PathBuffer>("path-buffer"));
    }

    (*movement_path_)->push_back(grid_position_);

    if (new_pos.x < grid_position_.x) {
        sprite_.set_flip({false, false});
    } else if (new_pos.x > grid_position_.x) {
        sprite_.set_flip({true, false});
    }

    reassign_room(grid_position_, new_pos);

    state_ = State::moving_or_idle;
    timer_ = movement_step_duration;
    awaiting_movement_ = false;
    can_move_ = false;
    idle_count_ = 0;
}



void BasicCharacter::heal(Platform& pfrm, App& app, int amount)
{
    if (is_replicant_) {
        // Replicants cannot heal
        return;
    }

    time_stream::event::CharacterHealthChanged e;
    e.id_.set(id_);
    e.owned_by_player_ = owner_ == &app.player();
    e.near_ = parent_ == &app.player_island();
    e.previous_health_.set(health_);
    app.time_stream().push(pfrm, app.level_timer(), e);

    if (health_ + amount > 255) {
        health_ = 255;
    } else {
        health_ += amount;
    }
}



void BasicCharacter::apply_damage(Platform& pfrm, App& app, Health damage)
{
    time_stream::event::CharacterHealthChanged e;
    e.id_.set(id_);
    e.owned_by_player_ = owner_ == &app.player();
    e.near_ = parent_ == &app.player_island();
    e.previous_health_.set(health_);
    app.time_stream().push(pfrm, app.level_timer(), e);

    int current = health_;
    current -= damage;

    if (current < 0) {
        health_ = 0;
    } else {
        health_ = current;
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
                room->ready();
            }
        }
    }

    grid_position_ = new_coord;
}



} // namespace skyland
