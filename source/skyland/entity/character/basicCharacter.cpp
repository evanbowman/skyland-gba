////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "basicCharacter.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/infirmary.hpp"
#include "skyland/rooms/mindControl.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static Time movement_step_duration(int race)
{
    if (race == 2) {
        return milliseconds(100);
    } else {
        return milliseconds(300);
    }
}



static u16 base_frame(BasicCharacter* character)
{
    if (character->owner() == &APP.player()) {
        return 35;
    } else {
        return 42;
    }
}



void BasicCharacter::set_race(int gfx)
{
    race_ = gfx;
}


int BasicCharacter::get_race() const
{
    return race_;
}



static CharacterId character_id_generator = 1;



static CharacterId alloc_character_id()
{
    return character_id_generator++;
}



void BasicCharacter::__reset_ids(int start_id)
{
    character_id_generator = start_id;
}



void BasicCharacter::__rebase_ids(CharacterId id)
{
    if (character_id_generator < id) {
        character_id_generator = id;
    }
}



BasicCharacter::BasicCharacter(Island* parent,
                               Player* owner,
                               const RoomCoord& position,
                               bool is_replicant)
    : Entity({{}, {}}), parent_(parent), owner_(owner),
      grid_position_(position), id_(alloc_character_id()), race_(0), icon_(0)
{
    sprite_.set_texture_index(40);
    sprite_.set_size(Sprite::Size::w16_h32);

    ai_mark_ = false;

    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    sprite_.set_position(o);

    awaiting_movement_ = true;
    can_move_ = false;

    co_op_locked_ = 0;

    is_replicant_ = is_replicant;

    health_ = max_health;

    ai_automated_ = true;
    superpinned_ = false;
}



void BasicCharacter::unpin()
{
    ai_automated_ = true;
}



void BasicCharacter::pin()
{
    ai_automated_ = false;
}



void BasicCharacter::un_superpin()
{
    superpinned_ = false;
}



void BasicCharacter::superpin(bool drop_path)
{
    if (drop_path) {
        drop_movement_path();
    }
    superpinned_ = true;
}



bool BasicCharacter::is_superpinned() const
{
    return superpinned_;
}



void BasicCharacter::finalize()
{
    // if (mind_controlled_) {
    //     mind_controlled_ = false;
    //     for (auto& room : APP.player_island().rooms()) {
    //         if (auto mc = room->cast<MindControl>()) {
    //             if (mc->bound_character() == id()) {
    //                 stop_mind_control(app,
    //                                   &mc->other_island()->owner(),
    //                                   mc);
    //                 return;
    //             }
    //         }
    //     }
    //     if (APP.opponent_island()) {
    //         for (auto& room : APP.opponent_island()->rooms()) {
    //             if (auto mc = room->cast<MindControl>()) {
    //                 if (mc->bound_character() == id()) {
    //                     stop_mind_control(app,
    //                                       &mc->other_island()->owner(),
    //                                       mc);
    //                     return;
    //                 }
    //             }
    //         }
    //     }
    // }
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
    unpin();
}



void BasicCharacter::rewind(Time delta)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    ai_automated_ = true;

    if (radiation_counter_) {
        sprite_.set_mix({});
    }
    radiation_counter_ = 0;

    sprite_.set_position(o);

    sprite_.set_texture_index(base_frame(this) + 5);

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
        dest.x += Fixnum::from_integer(dest_grid_pos.x * 16);
        dest.y += Fixnum::from_integer(dest_grid_pos.y * 16 -
                                       3); // floor is two pixels thick

        if (dest_grid_pos.x < grid_position_.x) {
            sprite_.set_flip({false, false});
        } else if (dest_grid_pos.x > grid_position_.x) {
            sprite_.set_flip({true, false});
        }

        timer_ -= delta;

        if (timer_ > 0) {
            auto fpos =
                interpolate(fvec(dest),
                            fvec(o),
                            Float(timer_) / movement_step_duration(race_));

            sprite_.set_position(Vec2<Fixnum>{Fixnum(fpos.x), Fixnum(fpos.y)});
        }

        anim_timer_ -= delta;
        if (anim_timer_ < 0) {
            anim_timer_ = milliseconds(100);
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this) + 5) {
                index = base_frame(this) + 4;
            } else {
                index = base_frame(this) + 5;
            }
            sprite_.set_texture_index(index);
        }

    } else {
        if (auto room = parent_->get_room(grid_position_)) {
            if (has_opponent(room)) {
                sprite_.set_texture_index(base_frame(this));
                sprite_.set_flip({});
            } else if (room->parent()->fire_present(grid_position())) {
                sprite_.set_texture_index(base_frame(this) + 2);
                sprite_.set_flip({});
            } else if (room->health() not_eq room->max_health()) {
                sprite_.set_texture_index(base_frame(this) + 1);
                sprite_.set_flip({});
            } else {
                sprite_.set_texture_index(base_frame(this) + 5);
            }
        }
    }
}



void BasicCharacter::set_idle()
{
    sprite_.set_texture_index(base_frame(this) + 5);
    state_ = State::moving_or_idle;
    timer_ = 0;
    idle_count_ = 0;
}



bool BasicCharacter::has_opponent(Room* room)
{
    for (auto& character : room->characters()) {
        if (character->owner() not_eq owner() and
            character->grid_position() == grid_position_) {
            return true;
        }
    }
    return false;
}



void BasicCharacter::update(Time delta)
{
    Platform::fatal("BasicCharacter::update() called... "
                    "Use other update method instead.");
}



bool BasicCharacter::ai_automated() const
{
    return ai_automated_;
}



void BasicCharacter::update(Time delta, Room* room)
{
    // const auto t1 = PLATFORM.delta_clock().sample();

    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    if (radiation_counter_) {
        if (radiation_counter_) {
            radiation_counter_ -= std::min((u8)4, radiation_counter_);
            sprite_.set_mix({custom_color(0xe81858), radiation_counter_});
        }
    } else {
        sprite_.set_mix({});
    }

    switch (state_) {
    case State::fighting:
        sprite_.set_flip({});
        update_attack(delta);
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

    case State::moving_or_idle: {

        if (movement_path_) {
            if (race_ == 2) {
                set_can_move();
            }
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
                    if (index == base_frame(this) + 5) {
                        index = base_frame(this) + 4;
                    } else {
                        index = base_frame(this) + 5;
                    }
                    sprite_.set_texture_index(index);
                }
            }
        } else /* no movement path */ {
            awaiting_movement_ = true;
            can_move_ = false;
            sprite_.set_position(o);
            sprite_.set_texture_index(base_frame(this) + 5);
            ++idle_count_;

            if (room) {

                auto metac = room->metaclass();

                if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                } else {
                    if (&room->owner()->owner() not_eq owner()) {
                        const char* name = (*metac)->name();
                        if (not str_eq(name, "plundered-room") and
                            not str_eq(name, "stairwell") and
                            not str_eq(name, "bridge") and
                            not str_eq(name, "ladder") and
                            not str_eq(name, "ladder+") and
                            not str_eq(name, "stairwell+") and
                            not str_eq(name, "stairwell++") and
                            not APP.opponent().is_friendly()) {
                            state_ = State::plunder_room;
                            timer_ = 0;
                        }
                    } else {
                        if (room->parent()->fire_present(grid_position())) {
                            state_ = State::extinguish_fire;
                            timer_ = 0;
                            anim_timer_ = 0;
                        } else if (room->health() < room->max_health() and
                                   not str_eq((*metac)->name(),
                                              "plundered-room")) {
                            state_ = State::repair_room;
                            anim_timer_ = 0;
                        } else {
                            if (room->is_powered_down() or
                                (not ai_automated_ and
                                 (not room->cast<Infirmary>() or
                                  health() == max_health) and
                                 not room->cast<Replicator>() and
                                 not room->cast<Decimator>() and
                                 (not room->cast<Transporter>() or
                                  (room->cast<Transporter>()->ready())))) {
                                // At this point, the character has absolutely
                                // nothing to do at its current location.
                                ai_automated_ = true;
                            }
                        }
                    }
                }
            }
        }
        break;
    }

    case State::plunder_room:
        awaiting_movement_ = true;
        can_move_ = false;

        sprite_.set_position(o);

        if (movement_path_) {
            this->set_idle();
            break;
        }

        sprite_.set_flip({});

        anim_timer_ += delta;
        if (anim_timer_ > milliseconds(200)) {
            anim_timer_ = 0;
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this) + 5) {
                index = base_frame(this) + 1;
            } else {
                index = base_frame(this) + 5;
            }
            sprite_.set_texture_index(index);
        }

        timer_ += delta;
        if (timer_ > milliseconds(500)) {
            timer_ = 0;

            if (&parent_->owner() == owner_) {
                // Wouldn't happen under normal circumstances, but if we're in a
                // plundering state and rewind a transport, the character can
                // end up in the player's island while continuing to plunder as
                // if it were in the opponent's castle.
                this->set_idle();
                break;
            }

            if (room) {
                auto metac = room->metaclass();
                const bool is_plundered =
                    str_cmp((*metac)->name(), "plundered-room") == 0;

                if (is_plundered) { // We've successfully ransacked the room,
                                    // let's try moving on to somewhere else.
                    this->set_idle();
                    ai_automated_ = true;
                    break;
                }

                room->plunder(2);

                if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                    break;
                }
            }
        }

        break;


    case State::extinguish_fire:
        awaiting_movement_ = true;
        can_move_ = false;

        sprite_.set_flip({});

        sprite_.set_position(o);

        anim_timer_ += delta;
        if (anim_timer_ > milliseconds(200)) {
            anim_timer_ = 0;
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this) + 5) {
                index = base_frame(this) + 2;
            } else {
                index = base_frame(this) + 5;
            }
            sprite_.set_texture_index(index);
        }

        if (movement_path_) {
            this->set_idle();
            break;
        }

        timer_ += delta;
        if (timer_ > milliseconds(3000)) {
            timer_ = 0;
            if (room) {
                room->parent()->fire_extinguish(grid_position_);
                if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                    break;
                } else {
                    this->set_idle();
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
            if (index == base_frame(this) + 5) {
                index = base_frame(this) + 1;
            } else {
                index = base_frame(this) + 5;
            }
            sprite_.set_texture_index(index);
        }

        if (movement_path_) {
            this->set_idle();
            break;
        }

        timer_ += delta;
        if (timer_ > milliseconds(500)) {
            timer_ = 0;
            if (room) {
                if (room->health() not_eq room->max_health()) {
                    if (race_ == 2) {
                        room->heal(1);
                    } else {
                        room->heal(2);
                    }
                } else {
                    this->set_idle();
                    break;
                }

                if (room->parent()->fire_present(grid_position_)) {
                    state_ = State::extinguish_fire;
                    timer_ = 0;
                    anim_timer_ = 0;
                } else if (has_opponent(room)) {
                    state_ = State::fighting;
                    timer_ = 0;
                    anim_timer_ = 0;
                    break;
                }
            }
        }

        break;
    }

    // const auto t2 = PLATFORM.delta_clock().sample();

    // if (PLATFORM.keyboard().pressed<Key::select>()) {
    //     Platform::fatal(format("%",
    //                            t2 - t1).c_str());
    // }
}



Sprite BasicCharacter::prepare_sprite() const
{
    Sprite ret = sprite_;
    if (is_replicant_) {
        switch (ret.get_texture_index()) {
        case 39:
            ret.set_texture_index(123);
            break;

        case 40:
            ret.set_texture_index(124);
            break;
        }
    } else if (race_) {
        switch (race_) {
        case 1:
            switch (ret.get_texture_index()) {
            case 39:
                ret.set_texture_index(38);
                break;

            case 40:
                ret.set_texture_index(41);
                break;
            }
            break;

        case 2:
            switch (ret.get_texture_index()) {
            case 40:
                ret.set_texture_index(99);
                break;

            case 39:
                ret.set_texture_index(100);
                break;
            }
            break;
        }
    }

    return ret;
}



void BasicCharacter::update_attack(Time delta)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

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
            sprite_.set_texture_index(base_frame(this) + 5);
            state_ = State::moving_or_idle;
            timer_ = 0;
            idle_count_ = 0;
        }

        timer_ = 0;
        if (sprite_.get_texture_index() == base_frame(this)) {
            sprite_.set_texture_index(base_frame(this) + 5);
        } else {
            sprite_.set_texture_index(base_frame(this));
        }
    }
    if (movement_path_) {
        sprite_.set_texture_index(base_frame(this) + 5);
        state_ = State::moving_or_idle;
        timer_ = 0;
        idle_count_ = 0;
    }
}



void BasicCharacter::movement_step(Time delta)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    awaiting_movement_ = false;
    can_move_ = false;


    if (not(*movement_path_)->empty()) {
        auto dest_grid_pos = (*movement_path_)->back();
        auto dest = parent_->visual_origin();
        dest.x += Fixnum::from_integer(dest_grid_pos.x * 16);
        dest.y += Fixnum::from_integer(dest_grid_pos.y * 16 -
                                       3); // floor is two pixels thick

        if (dest_grid_pos.x < grid_position_.x) {
            sprite_.set_flip({false, false});
        } else if (dest_grid_pos.x > grid_position_.x) {
            sprite_.set_flip({true, false});
        }

        auto fpos = interpolate(
            fvec(dest), fvec(o), Float(timer_) / movement_step_duration(race_));

        sprite_.set_position(Vec2<Fixnum>{Fixnum(fpos.x), Fixnum(fpos.y)});
    }

    if (timer_ > movement_step_duration(race_)) {
        timer_ = 0;

        awaiting_movement_ = true;
        can_move_ = false;

        if (auto room = parent_->get_room(grid_position())) {
            room->update_description();
        }

        if (not(*movement_path_)->empty()) {
            const auto current_position = (*movement_path_)->back();

            time_stream::event::CharacterMoved e;
            e.id_.set(id_);
            e.previous_x_ = grid_position_.x;
            e.previous_y_ = grid_position_.y;
            e.superpinned_ = superpinned_;
            e.near_ = is_player_island(parent_);

            if (reassign_room(grid_position_, current_position)) {
                APP.time_stream().push(APP.level_timer(), e);
            }

            if (auto room = parent_->get_room(grid_position())) {
                room->update_description();
            }

            (*movement_path_)->pop_back();
        } else {
            movement_path_.reset();
        }
    }
}



void BasicCharacter::set_movement_path(Path path)
{
    time_stream::event::CharacterMovementPathAssigned e;
    e.id_.set(id_);
    e.near_ = is_player_island(parent_);
    APP.time_stream().push(APP.level_timer(), e);

    movement_path_ = std::move(path);
}



void BasicCharacter::rewind_movement_step(const RoomCoord& new_pos)
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
    timer_ = movement_step_duration(race_);
    awaiting_movement_ = false;
    can_move_ = false;
    idle_count_ = 0;
}



void BasicCharacter::heal(int amount)
{
    if (is_replicant_) {
        // Replicants cannot heal
        return;
    }

    time_stream::event::CharacterHealthChanged e;
    e.id_.set(id_);
    e.owned_by_player_ = owner_ == &APP.player();
    e.near_ = is_player_island(parent_);
    static_assert(max_health <= 255);
    e.previous_health_ = health_;
    APP.time_stream().push(APP.level_timer(), e);

    if (health_ + amount > max_health) {
        health_ = max_health;
    } else {
        health_ += amount;
    }

    if (auto room = parent_->get_room(grid_position())) {
        room->update_description();
    }
}



void BasicCharacter::apply_radiation_damage(Health amount)
{
    radiation_counter_ = 230;

    apply_damage(amount);
}



void BasicCharacter::apply_damage(Health damage)
{
    time_stream::event::CharacterHealthChanged e;
    e.id_.set(id_);
    e.owned_by_player_ = owner_ == &APP.player();
    e.near_ = is_player_island(parent_);
    e.previous_health_ = health_;
    APP.time_stream().push(APP.level_timer(), e);

    if (auto room = parent_->get_room(grid_position())) {
        room->update_description();
    }

    int current = health_;
    current -= damage;

    if (current < 0) {
        health_ = 0;
    } else {
        health_ = current;
    }
}



bool BasicCharacter::reassign_room(const RoomCoord& old_coord,
                                   const RoomCoord& new_coord)
{
    auto target_room = parent_->get_room(new_coord);
    if (not target_room) {
        return false;
    }

    if (auto room = parent_->get_room(old_coord)) {

        Optional<EntityRef<BasicCharacter>> self;

        for (auto it = room->characters().begin();
             it not_eq room->characters().end();) {

            if (it->get() == this) {
                self = std::move(*it);
                it = room->edit_characters().erase(it);
            } else {
                ++it;
            }
        }

        if (self) {
            target_room->edit_characters().push(std::move(*self));
            target_room->ready();
        }
    }

    grid_position_ = new_coord;

    if (auto room = parent_->get_room(grid_position())) {
        room->update_description();
    }

    return true;
}



bool BasicCharacter::co_op_acquire_lock()
{
    if (co_op_locked_) {
        return false;
    }

    co_op_locked_ = true;

    return true;
}



void BasicCharacter::co_op_release_lock()
{
    co_op_locked_ = false;
}



bool BasicCharacter::co_op_locked() const
{
    return co_op_locked_;
}



std::pair<BasicCharacter*, Room*> BasicCharacter::find_by_id(CharacterId id)
{
    auto found = APP.player_island().find_character_by_id(id);
    if (found.first) {
        return found;
    }

    if (APP.opponent_island()) {
        found = APP.opponent_island()->find_character_by_id(id);
        if (found.first) {
            return found;
        }
    }

    return {nullptr, nullptr};
}



const char* BasicCharacter::name() const
{
    // feature removed...

    return nullptr;
}



} // namespace skyland
