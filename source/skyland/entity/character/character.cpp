////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "character.hpp"
#include "script/lisp.hpp"
#include "script/listBuilder.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/infirmary.hpp"
#include "skyland/rooms/portal.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static Time movement_step_duration(int race)
{
    if (race == 3) {
        return milliseconds(150);
    }
    return milliseconds(300);
}



static Health repair_strength(Character::Race r)
{
    if (r == Character::Race::dog) {
        return 1;
    } else {
        return 2;
    }
}



enum CharacterSprite {
    begin = 35 * 2,
    human_fighting = begin,
    human_repairing,
    human_extinguish,
    friendly_goblin_step,
    human_step,
    human_still,
    friendly_goblin_still,
    goblin_fighting,
    goblin_repairing,
    goblin_extinguish,
    hostile_human_step,
    goblin_step,
    goblin_still,
    hostile_human_still,
    replicant_step,
    replicant_still,
};



static u16 base_frame(Character* character)
{
    if (character->owner() == &APP.player()) {
        return human_fighting;
    } else {
        return goblin_fighting;
    }
}



void Character::set_race(Race race)
{
    race_ = (int)race;

    if (race_ == (int)Race::dog) {
        for (auto& r : APP.player_island().rooms()) {
            for (auto& chr : r->edit_characters()) {
                if (chr->get_race() == Race::dog and chr->custom_texture_) {
                    custom_texture_ = chr->custom_texture_;
                }
            }
        }
        if (not custom_texture_) {
            custom_texture_ = PLATFORM.make_dynamic_texture();
        }
        if (custom_texture_) {
            (*custom_texture_)->remap(84 * 2);
        }
        set_max_health(200);
    }
}


Character::Race Character::get_race() const
{
    return (Race)race_;
}



static CharacterId character_id_generator = 1;



static CharacterId alloc_character_id()
{
    return character_id_generator++;
}



void Character::__reset_ids(int start_id)
{
    character_id_generator = start_id;
}



void Character::__rebase_ids(CharacterId id)
{
    if (character_id_generator < id) {
        character_id_generator = id;
    }
}



Character::Character(Island* parent,
                     Player* owner,
                     const RoomCoord& position,
                     bool is_replicant)
    : Entity({{}, {}}), parent_(parent), id_(alloc_character_id()), race_(0),
      icon_(0)
{
    owner_is_player_ = owner == &APP.player();

    grid_position_ = position;
    sprite_.set_texture_index(human_still);
    sprite_.set_size(Sprite::Size::w16_h16);

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

    antisocial_ = rng::choice<10>(rng::utility_state);
}



void Character::set_spr_flip(bool flipped)
{
    sprite_.set_flip({flipped, false});
}



void Character::unpin()
{
    ai_automated_ = true;
}



void Character::pin()
{
    ai_automated_ = false;
}



void Character::un_superpin()
{
    superpinned_ = false;
}



void Character::superpin(bool drop_path)
{
    if (drop_path) {
        drop_movement_path();
    }
    superpinned_ = true;
}



bool Character::is_superpinned() const
{
    return superpinned_;
}



void Character::finalize()
{
}



void Character::set_can_move()
{
    can_move_ = true;
}



void Character::transported()
{
    state_ = State::after_transport;
    anim_timer_ = 0;
    sprite_.set_mix({ColorConstant::electric_blue, 255});
    idle_count_ = 0;
    set_wants_to_chat(false);
    unpin();
}



void Character::rewind(Time delta)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    ai_automated_ = true;

    if (radiation_counter_) {
        sprite_.set_mix({});
    }
    radiation_counter_ = 0;

    if (sprite_.get_mix().color_ == ColorConstant::electric_blue) {
        sprite_.set_mix({});
    }

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
            set_spr_flip(false);
            sprite_.set_flip({false, false});
        } else if (dest_grid_pos.x > grid_position_.x) {
            set_spr_flip(true);
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



Player* Character::owner() const
{
    if (owner_is_player_) {
        return &APP.player();
    } else {
        return &APP.opponent();
    }
}



void Character::set_idle()
{
    sprite_.set_texture_index(base_frame(this) + 5);
    state_ = State::moving_or_idle;
    timer_ = 0;
    idle_count_ = 0;
    set_wants_to_chat(false);
}



bool Character::has_opponent(Room* room)
{
    for (auto& character : room->characters()) {
        if (character->owner() not_eq owner() and
            character->grid_position() == grid_position_) {
            return true;
        }
    }
    return false;
}



void Character::update(Time delta)
{
    Platform::fatal("Character::update() called... "
                    "Use other update method instead.");
}



bool Character::ai_automated() const
{
    return ai_automated_;
}



u8 Character::get_max_health() const
{
    return max_health_;
}



void Character::set_max_health(u8 val)
{
    max_health_ = val;
    health_ = clamp(health_, (Health)0, (Health)max_health_);
}



void Character::set_phase(u8 phase)
{
    sprite_.set_alpha(phase ? Sprite::Alpha::translucent
                            : Sprite::Alpha::opaque);
    sprite_.set_mix({});
}



void Character::record_stats()
{
    if (owner() not_eq &APP.player()) {
        // Don't bother with opponent stats, as they aren't displayed
        // anyway. But... maybe displaying stats for enemies might be fun?
        // e.g. if a goblin killed one of your crew, knowing which goblin it was
        // would allow your crew to exact vengence on that goblin...
        return;
    }
    time_stream::event::CharacterStatsChanged e;
    e.id_.set(id());
    e.prev_stats_ = stats_->info_;
    APP.time_stream().push(APP.level_timer(), e);
}



void Character::update(Time delta, Room* room)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    if (radiation_counter_) {
        if (radiation_counter_) {
            radiation_counter_ -= std::min((u8)4, radiation_counter_);
            sprite_.set_mix({custom_color(0xe81858), radiation_counter_});

            if (radiation_counter_ == 0) {
                sprite_.set_mix({});
            }
        }
    }

    if (parent_->phase()) {
        if (not PLATFORM.screen().fade_active() and
            state_ not_eq State::after_transport) {

            if (sprite_.get_mix().amount_ not_eq 128) {
                auto shader = APP.environment().shader();
                // Apply weather effects to the background sky tone.
                auto sky_tone = shader(
                    ShaderPalette::background, custom_color(0x63b2e0), 0, 1);
                const ColorMix fake_blend{sky_tone, 128};
                // GBA hardware blending stacks blend intensities. Fake a blend
                // effect by mixing the background sky color into the character
                // color. This is far from perfect, as the crewmemmber sprite
                // isn't actually translucent, but looks better than the
                // alternative.
                sprite_.set_mix(fake_blend);
                sprite_.set_alpha(Sprite::Alpha::opaque);
            }

        } else {
            sprite_.set_alpha(Sprite::Alpha::translucent);
            sprite_.set_mix({});
        }
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

    case State::chatting: {
        timer_ += delta;
        if (has_movement_path() or timer_ > seconds(7) + milliseconds(200)) {
            timer_ = 0;
            set_wants_to_chat(false);
            // Number of "wants to chat" cycles until the character will seek
            // out another conversation.
            antisocial_ = rng::choice<8>(rng::utility_state);
            set_idle();
        }
        anim_timer_ += delta;
        if (anim_timer_ > milliseconds(200)) {
            anim_timer_ -= milliseconds(200);
            auto index = sprite_.get_texture_index();
            if (index == base_frame(this) + 5) {
                index = 93;
            } else {
                index = base_frame(this) + 5;
            }
            sprite_.set_texture_index(index);
        }

        sprite_.set_position(o);
        break;
    }

    case State::moving_or_idle: {

        if (movement_path_) {
            if (race_ == 3) {
                can_move_ = true;
            }
            if (awaiting_movement_ and not can_move_) {
                // ... we're waiting to be told that we can move. Because movement
                // is grid-based
                sprite_.set_position(o);
            } else {
                timer_ += delta;
                movement_step(delta, room);
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
            if (delta > 0) {
                ++idle_count_;

                if (idle_count_ % 16 == 0) {
                    if (room) {
                        update_favorite_room_stat(room);
                    }
                }
            }

            if (race_ == 3) {
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
            } else {
                sprite_.set_texture_index(base_frame(this) + 5);
            }

            if (not APP.opponent_island()) {
                if (idle_count_ > 60 * 10) {
                    if (owner_is_player_) {
                        if (antisocial_) {
                            --antisocial_;
                            idle_count_ = 0;
                        } else {
                            // Uncomment to enable experimental crew social
                            // features.
                            // set_wants_to_chat(true);
                        }
                    }
                }
            } else {
                wants_to_chat_ = false;
            }

            if (wants_to_chat()) {
                // Check if adjacent crewmembers want to chat...

                auto adjacent_chr = [&](int xo, int yo) {
                    return parent()->character_at_location(
                        {(u8)(grid_position_.x + xo),
                         (u8)(grid_position_.y + yo)});
                };

                Character* chr = nullptr;

                if ((chr = adjacent_chr(-1, 0)) or (chr = adjacent_chr(1, 0)) or
                    (chr = adjacent_chr(0, -1)) or (chr = adjacent_chr(0, 1))) {

                    if (chr->wants_to_chat() and
                        not chr->has_movement_path() and
                        (chr->state() == State::moving_or_idle or
                         chr->state() == State::chatting)) {
                        state_ = State::chatting;
                        chr->state_ = State::chatting;
                        chr->timer_ = 0;
                        timer_ = 0;
                        anim_timer_ = 0;
                        chr->anim_timer_ = 0;
                    }
                }
            }

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
                            not str_eq(name, "portal") and
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
                                  health() == max_health_) and
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

            if (&parent_->owner() == owner()) {
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
                if (room->parent()->fire_present(grid_position_)) {
                    record_stats();
                    CharacterStats::inc(stats_->info_.fires_extinguished_);
                }
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
                    auto prev = room->health();
                    room->heal(repair_strength(get_race()));
                    auto healed_hp = room->health() - prev;
                    if (healed_hp) {
                        repair_wb_ = (repair_wb_ + 1) % 4;
                        if (repair_wb_ == 3) {
                            // Don't waste too much history mem on this...
                            record_stats();
                        }
                        for (int i = 0; i < healed_hp; ++i) {
                            CharacterStats::inc(stats_->info_.damage_repaired_);
                        }
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
}



void Character::draw(Platform::Screen& screen, const DrawTransform& t)
{
    auto& spr = sprite_;

    spr.set_position(
        {spr.get_position().x, spr.get_position().y + t.y_displace_});
    spr.set_priority(t.priority_);

    const auto prev_mix = spr.get_mix();
    const auto prev_alpha = spr.get_alpha();
    if (t.mix_.amount_) {
        spr.set_mix(t.mix_);
    }
    spr.set_alpha(t.alpha_);

    auto draw_regular = [&] { screen.draw(spr); };

    auto draw_custom = [&](int tidx) {
        auto old = spr.get_texture_index();
        spr.set_texture_index(tidx);
        screen.draw(spr);
        spr.set_texture_index(old);
    };

    auto draw_bumped_custom = [&](int tidx) {
        auto p = spr.get_position();
        p.y += 1.0_fixed;
        spr.set_position(p);
        draw_custom(tidx);
        p.y -= 1.0_fixed;
        spr.set_position(p);
    };

    if (is_replicant_ and get_race() not_eq Race::dog) {
        switch (spr.get_texture_index()) {
        case human_step:
            draw_custom(replicant_step);
            break;

        case human_still:
            draw_bumped_custom(replicant_still);
            break;

        default:
            draw_regular();
            break;
        }
    } else {
        switch (get_race()) {
        case Race::default_race:
            switch (spr.get_texture_index()) {
            case human_still:
                draw_bumped_custom(human_still);
                break;

            case goblin_still:
                draw_bumped_custom(goblin_still);
                break;

            default:
                draw_regular();
            }
            break;

        case Race::hostile_human:
            switch (spr.get_texture_index()) {
            case goblin_step:
                draw_custom(hostile_human_step);
                break;

            case goblin_still:
                draw_bumped_custom(hostile_human_still);
                break;

            default:
                draw_regular();
                break;
            }
            break;

        case Race::goblin:
            switch (spr.get_texture_index()) {
            case human_step:
                draw_custom(friendly_goblin_step);
                break;

            case human_still:
                draw_bumped_custom(friendly_goblin_still);
                break;

            case goblin_still:
                draw_bumped_custom(goblin_still);
                break;

            default:
                draw_regular();
                break;
            }
            break;

        case Race::dog:
            if (not custom_texture_) {
                custom_texture_ = PLATFORM.make_dynamic_texture();
            }
            if (custom_texture_) {
                int offset = 0;
                if (state_ == State::moving_or_idle and has_movement_path()) {
                    offset = 2;
                }
                switch (spr.get_texture_index()) {
                case human_still:
                    draw_bumped_custom((*custom_texture_)->mapping_index() * 4 +
                                       offset);
                    break;

                case human_step:
                    if (not offset) {
                        draw_bumped_custom(
                            (*custom_texture_)->mapping_index() * 4 + 1);
                    } else {
                        draw_custom((*custom_texture_)->mapping_index() * 4 +
                                    offset + 1);
                    }
                    break;

                default:
                    draw_regular();
                    break;
                }
            }
            break;
        }
    }

    spr.set_priority(1);
    spr.set_position(
        {spr.get_position().x, spr.get_position().y - t.y_displace_});
    spr.set_mix(prev_mix);
    spr.set_alpha(prev_alpha);
}



lisp::Value* Character::serialize()
{
    using namespace lisp;

    ListBuilder chr_info;
    chr_info.push_back(L_INT(grid_position().x));
    chr_info.push_back(L_INT(grid_position().y));
    if (health() not_eq 255) {
        chr_info.push_back(L_CONS(make_symbol("hp"), make_integer(health())));
    }
    if (is_replicant()) {
        chr_info.push_back(L_CONS(make_symbol("rplc"), make_integer(1)));
    }
    if (auto race = (int)get_race()) {
        chr_info.push_back(L_CONS(make_symbol("race"), make_integer(race)));
    }
    if (auto icon = get_icon()) {
        chr_info.push_back(L_CONS(make_symbol("icon"), make_integer(icon)));
    }
    if (auto e = stats().info_.enemies_vanquished_) {
        chr_info.push_back(L_CONS(L_SYM("kc"), L_INT(e)));
    }
    if (auto b = stats().info_.battles_fought_) {
        chr_info.push_back(L_CONS(L_SYM("bt"), L_INT(b)));
    }
    if (auto b = stats().info_.damage_repaired_.get()) {
        chr_info.push_back(L_CONS(L_SYM("dr"), L_INT(b)));
    }
    if (auto s = stats().info_.steps_taken_.get()) {
        chr_info.push_back(L_CONS(L_SYM("sc"), L_INT(s)));
    }
    if (auto s = stats().info_.fires_extinguished_) {
        chr_info.push_back(L_CONS(L_SYM("fe"), L_INT(s)));
    }
    chr_info.push_back(L_CONS(make_symbol("id"), make_integer(id())));
    return chr_info.result();
}



void Character::update_attack(Time delta)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    awaiting_movement_ = true;
    can_move_ = false;
    sprite_.set_position(o);
    timer_ += delta;
    if (timer_ > milliseconds(300)) {

        auto get_opponent = [&]() -> Character* {
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
            if (get_race() == Race::dog) {
                // Dog deals some extra damage
                chr->apply_damage(2);
            }
            if (chr->health() <= 0) {
                record_stats();
                CharacterStats::inc(stats_->info_.enemies_vanquished_);
            }
        } else {
            sprite_.set_texture_index(base_frame(this) + 5);
            state_ = State::moving_or_idle;
            timer_ = 0;
            idle_count_ = 0;
            set_wants_to_chat(false);
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
        set_wants_to_chat(false);
    }
}



CompleteCharacterStats& Character::stats()
{
    return *stats_;
}



struct WarpEffect : public Entity
{
public:
    WarpEffect(u8 tidx1,
               u8 tidx2,
               Vec2<Fixnum> source,
               Vec2<Fixnum> dest,
               Time interval)
        : Entity({}), interval_(interval), source_(source), dest_(dest)
    {
        sprite_.set_size(Sprite::Size::w8_h8);
        sprite_.set_origin({4, 4});
        sprite_.set_position(source);
        sprite_.set_tidx_8x8(tidx1, tidx2);
    }


    void update(Time delta) override
    {
        if (delta == 0) {
            return;
        }

        timer_ += delta;
        if (timer_ < interval_) {
            auto pos = interpolate_fp(
                source_, dest_, Fixnum(Float(timer_) / interval_));
            sprite_.set_position(pos);
        } else {
            kill();
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }

private:
    Time timer_ = 0;
    Time interval_;
    Vec2<Fixnum> source_;
    Vec2<Fixnum> dest_;
};



void Character::movement_step(Time delta, Room* current_room)
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(grid_position_.x * 16);
    o.y += Fixnum::from_integer(grid_position_.y * 16 - 3);

    awaiting_movement_ = false;
    can_move_ = false;

    bool warped = false;


    if (not(*movement_path_)->empty()) {
        auto dest_grid_pos = (*movement_path_)->back();
        auto dest = parent_->visual_origin();
        dest.x += Fixnum::from_integer(dest_grid_pos.x * 16);
        dest.y += Fixnum::from_integer(dest_grid_pos.y * 16 -
                                       3); // floor is two pixels thick

        if (current_room and current_room->cast<Portal>()) {
            if (auto d = parent()->get_room(dest_grid_pos)) {
                if (d not_eq current_room and d->cast<Portal>()) {
                    timer_ = movement_step_duration(race_) + 1;
                    warped = true;

                    if (auto e = APP.alloc_entity<WarpEffect>(
                            30,
                            6,
                            d->visual_center(),
                            current_room->visual_center(),
                            milliseconds(200))) {
                        APP.effects().push(std::move(e));
                    }
                }
            }
        }

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
            CharacterStats::inc(stats_->info_.steps_taken_);
            CharacterStats::inc(stats_->info_.steps_taken_);
            CharacterStats::inc(stats_->info_.steps_taken_);

            if (warped) {
                state_ = State::after_transport;
                anim_timer_ = 0;
                sprite_.set_mix({ColorConstant::electric_blue, 255});
                idle_count_ = 0;
                set_wants_to_chat(false);
                sprite_.set_texture_index(base_frame(this) + 5);
                awaiting_movement_ = false;
                can_move_ = false;
                make_transport_effect(*this);
            }

        } else {
            movement_path_.reset();
        }
    }
}



void Character::set_movement_path(Path path)
{
    time_stream::event::CharacterMovementPathAssigned e;
    e.id_.set(id_);
    e.near_ = is_player_island(parent_);
    APP.time_stream().push(APP.level_timer(), e);

    movement_path_ = std::move(path);
}



void Character::rewind_movement_step(const RoomCoord& new_pos)
{
    if (not movement_path_) {
        movement_path_.emplace(allocate_dynamic<PathBuffer>("path-buffer"));
    }

    (*movement_path_)->push_back(grid_position_);
    CharacterStats::dec(stats_->info_.steps_taken_);
    CharacterStats::dec(stats_->info_.steps_taken_);
    CharacterStats::dec(stats_->info_.steps_taken_);

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
    set_wants_to_chat(false);
}



void Character::heal(int amount)
{
    if (is_replicant_) {
        // Replicants cannot heal
        return;
    }

    time_stream::event::CharacterHealthChanged e;
    e.id_.set(id_);
    e.owned_by_player_ = owner_is_player_;
    e.near_ = is_player_island(parent_);
    static_assert(max_health <= 255);
    e.previous_health_ = health_;
    APP.time_stream().push(APP.level_timer(), e);

    if (health_ + amount > max_health_) {
        health_ = max_health_;
    } else {
        health_ += amount;
    }

    if (auto room = parent_->get_room(grid_position())) {
        room->update_description();
    }
}



void Character::apply_radiation_damage(Health amount)
{
    radiation_counter_ = 230;

    apply_damage(amount);
}



void Character::apply_damage(Health damage)
{
    time_stream::event::CharacterHealthChanged e;
    e.id_.set(id_);
    e.owned_by_player_ = owner_is_player_;
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



bool Character::reassign_room(const RoomCoord& old_coord,
                              const RoomCoord& new_coord)
{
    auto target_room = parent_->get_room(new_coord);
    if (not target_room) {
        return false;
    }

    if (auto room = parent_->get_room(old_coord)) {

        Optional<EntityRef<Character>> self;

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



bool Character::co_op_acquire_lock()
{
    if (co_op_locked_) {
        return false;
    }

    co_op_locked_ = true;

    return true;
}



void Character::co_op_release_lock()
{
    co_op_locked_ = false;
}



bool Character::co_op_locked() const
{
    return co_op_locked_;
}



std::pair<Character*, Room*> Character::find_by_id(CharacterId id)
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



const char* Character::name() const
{
    // feature removed...

    return nullptr;
}



bool Character::wants_to_chat() const
{
    return wants_to_chat_;
}



void Character::set_wants_to_chat(bool status)
{
    wants_to_chat_ = status;
    idle_count_ = 0;
}



void Character::update_favorite_room_stat(Room* room)
{
    auto current_mt = room->metaclass_index();
    if (stats_->highest_room_[0].metaclass_index_ == current_mt) {
        if (stats_->info_.favorite_room_ not_eq current_mt) {
            record_stats();
            stats_->info_.favorite_room_ = current_mt;
        }
    }
    if (stats_->current_room_.metaclass_index_ != current_mt) {
        stats_->current_room_.metaclass_index_ = current_mt;
        stats_->current_room_.counter_.set(0);
        for (auto& info : stats_->highest_room_) {
            if (info.metaclass_index_ == current_mt) {
                stats_->current_room_.counter_.set(info.counter_.get());
                break;
            }
        }
    }

    // Increment current room counter
    stats_->current_room_.counter_.set(stats_->current_room_.counter_.get() +
                                       1);

    auto current_count = stats_->current_room_.counter_.get();

    // Check if current room should be promoted to highest_room_ array
    for (int i = 0; i < 3; ++i) {
        if (stats_->highest_room_[i].metaclass_index_ == current_mt) {
            // Update existing entry
            stats_->highest_room_[i].counter_.set(current_count);
            // Bubble up if needed
            while (i > 0 && stats_->highest_room_[i].counter_.get() >
                                stats_->highest_room_[i - 1].counter_.get()) {
                std::swap(stats_->highest_room_[i],
                          stats_->highest_room_[i - 1]);
                --i;
            }
            return;
        }
    }

    // Room not in highest_room_ array, check if it should be inserted
    if (current_count > stats_->highest_room_[2].counter_.get()) {
        stats_->highest_room_[2] = stats_->current_room_;
        // Bubble up
        for (int i = 2;
             i > 0 && stats_->highest_room_[i].counter_.get() >
                          stats_->highest_room_[i - 1].counter_.get();
             --i) {
            std::swap(stats_->highest_room_[i], stats_->highest_room_[i - 1]);
        }
    }
}

} // namespace skyland
