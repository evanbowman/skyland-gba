////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "room.hpp"
#include "globals.hpp"
#include "island.hpp"
#include "platform/platform.hpp"
#include "room_metatable.hpp"
#include "scene/moveCharacterScene.hpp"
#include "script/listBuilder.hpp"
#include "skyland.hpp"
#include "skyland/entity/ghost.hpp"
#include "skyland/network.hpp"
#include "skyland/rooms/plunderedRoom.hpp"
#include "skyland/scene/multiplayerCoOpAwaitLockScene.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/tile.hpp"
#include "skyland/weather/solarStorm.hpp"
#include "timeStreamEvent.hpp"



namespace skyland
{



Room::Room(Island* parent, const char* name, const RoomCoord& position)
    : parent_(parent), dispatch_list_(nullptr), health_(1),
      x_position_(position.x), y_position_(position.y), ai_aware_(true),
      cloaked_(false), init_awareness_upon_unpause_(false),
      show_damage_delay_frames_(0), powerdown_(0)
{

    if (name == nullptr) {
        return;
    }

    shielded_ = 0;

    finalized_ = 0;
    dispatch_queued_ = 0;

    set_group(Room::Group::none);

    co_op_locked_ = false;

    auto metatable = room_metatable();

    hidden_ = false;
    description_changed_ = true;

    if (metatable.second > 255) {
        Platform::fatal("metaclass index exceeds 255! More bytes required to "
                        "store the full 16 bit metaclass index!");
    }

    for (MetaclassIndex i = 0; i < metatable.second; ++i) {
        auto& current = metatable.first[i];

        if (str_eq(name, current->name())) {
            metaclass_index_ = i;

            auto mt_size = current->size();
            if (mt_size.x > 15 or mt_size.y > 15) {
                Platform::fatal("Room size too large!");
            }
            if (mt_size.x == 0 or mt_size.y == 0) {
                Platform::fatal("Zero-sized room unsupported!");
            }

            // See comment along with definition of size_x_ and size_y_ fields.
            size_x_ = mt_size.x;
            size_y_ = mt_size.y;
            health_ = current->full_health();

            ready();
            return;
        }
    }

#if defined(__GBA__) or defined(__NDS__)
    static_assert(sizeof(Room) < 33,
                  "Not actually a hard requirement, just put"
                  "this here as a reminder to keep rooms small."
                  " Room pool entries are only 64 bytes in size."
                  " Increasing the base room size will leave fewer "
                  " bytes for derived rooms. If needed, you could "
                  " increase the room pool size in roomPool.hpp");
#endif

    Platform::fatal("room failed to bind metaclass!?");
}



static const auto injured_anim_time = milliseconds(250);



void Room::reset_injured_timer(Time value)
{
    injured_timer_ = value;
}



void Room::on_level_start()
{
}



void Room::set_injured()
{
    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            PLATFORM.set_palette(
                parent_->layer(), x_position_ + x, y_position_ + y, 13);
        }
    }

    reset_injured_timer(injured_anim_time);
}



bool Room::opponent_display_on_hover() const
{
    return false;
}



const char* Room::name() const
{
    return (*metaclass())->name();
}



RoomMeta* Room::metaclass() const
{
    return &room_metatable().first[metaclass_index_];
}



MetaclassIndex Room::metaclass_index() const
{
    return metaclass_index_;
}


Power Room::power_usage() const
{
    if (powerdown_) {
        return 0;
    }
    return (*metaclass())->consumes_power();
}


bool Room::is_decoration() const
{
    return (*metaclass())->category() == Room::Category::decoration;
}


void Room::display(Platform::Screen& screen)
{
    for (auto& c : characters()) {
        const auto& pos = c->sprite().get_position();
        if (pos.y.as_integer() < 700) {
            if (parent_->interior_visible()) {
                c->draw(screen);
            } else if (c->state() == Character::State::repair_room or
                       c->state() == Character::State::extinguish_fire) {
                ColorMix mix;
                if (parent()->layer() == Layer::map_1_ext) {
                    mix = {custom_color(0x28457b), 50};
                } else {
                    mix = {custom_color(0x28457b), 200};
                }
                Character::DrawTransform t;
                t.mix_ = mix;
                t.alpha_ = Sprite::Alpha::translucent;
                c->draw(screen, t);
            }
        }
    }
}



void Room::display_on_hover(Platform::Screen& screen, const RoomCoord& cursor)
{
    if (not parent_->interior_visible() and parent_ == &player_island()) {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y.as_integer() < 700) {
                Character::DrawTransform t;
                t.mix_ = {custom_color(0x28457b), 200};
                t.alpha_ = Sprite::Alpha::translucent;
                c->draw(screen, t);
            }
        }
    }
}



u8 Room::default_palette()
{
    if (is_powered_down()) {
        return 9;
    } else if ((*metaclass())->properties() & RoomProperties::plugin) {
        return 12;
    } else {
        return parent_->layer() == Layer::map_0_ext ? 0 : 2;
    }
}



void Room::schedule_repaint()
{
    parent()->schedule_repaint_partial();
    partial_repaint_flag_ = true;
}



class UIDamageNumber : public Entity
{
public:
    static int total_count_;

    static const int throttle_count_ = 20;


    UIDamageNumber(const Vec2<Fixnum>& position, int value) : Entity({})
    {
        sprite_.set_tidx_8x8(29, value);
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w8_h8);
    }


    void rewind(Time delta) override
    {
        kill();
    }


    void update(Time delta) override
    {
        if (delta == 0) {
            return;
        }

        timer_ += delta;
        if (timer_ > seconds(1) - milliseconds(400)) {
            kill();
        } else if (timer_ < milliseconds(300)) {
            auto pos = sprite_.get_position();
            pos.y -= Fixnum(0.25f);
            while (delta > 16667) {
                delta -= 16667;
                pos.y -= Fixnum(0.25f);
            }

            sprite_.set_position(pos);
        } else if (timer_ > milliseconds(550)) {
            sprite_.set_alpha(Sprite::Alpha::translucent);
        }
    }


    static void create(int value, Vec2<Fixnum> pos)
    {
        auto numstr = stringify(value);

        // Centered over input position
        pos.x -= Fixnum::from_integer((numstr.length() * 6) / 2);

        for (char c : numstr) {
            int value = c - '0';
            if (auto e = APP.alloc_entity<UIDamageNumber>(pos, value)) {
                APP.effects().push(std::move(e));
            }
            pos.x += 6.0_fixed;
        }
    }


private:
    Time timer_ = 0;
};



int UIDamageNumber::total_count_ = 0;



void Room::update(Time delta)
{
    dispatch_queued_ = false;

    if (not characters().empty()) {
        ready();
    }

    if ((bool)injured_timer_) {

        ready();

        if (injured_timer_ > 0) {
            const auto new_timer = injured_timer_ - delta;

            if (new_timer < milliseconds(210) and
                injured_timer_ > milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             14);
                    }
                }
            }

            if (new_timer < milliseconds(170) and
                injured_timer_ > milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             15);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ <= 0) {
                injured_timer_ = 0;

                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             default_palette());
                    }
                }

                if (is_powered_down()) {
                    schedule_repaint();
                }
            }
        }
    }

    if (accumulated_damage_) {
        if (--show_damage_delay_frames_ == 0) {
            if (UIDamageNumber::total_count_ >=
                UIDamageNumber::throttle_count_) {
                // Retry later. Too many objects onscreen.
                ++show_damage_delay_frames_;
            } else {

                bool is_offscreen =
                    (visual_center().x.as_integer() <
                     PLATFORM.screen().get_view().int_center().x + 8 -
                         (size().x * 16) / 2) or
                    (visual_center().x.as_integer() - (size().x * 16) / 2 >
                     (int)(PLATFORM.screen().get_view().int_center().x +
                           PLATFORM.screen().size().x));

                if (not is_offscreen) {
                    UIDamageNumber::create(accumulated_damage_, center());
                }

                accumulated_damage_ = 0;
            }
        }
        ready();
    }
}



void Room::rewind(Time delta)
{
    if (injured_timer_) {
        if (injured_timer_ < injured_anim_time) {
            const auto new_timer = injured_timer_ + delta;

            if (new_timer > milliseconds(10) and
                injured_timer_ < milliseconds(10)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             15);
                    }
                }
            }

            if (new_timer > milliseconds(210) and
                injured_timer_ < milliseconds(210)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             14);
                    }
                }
            }

            if (new_timer > milliseconds(170) and
                injured_timer_ < milliseconds(170)) {
                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             13);
                    }
                }
            }


            injured_timer_ = new_timer;

            if (injured_timer_ >= injured_anim_time) {
                injured_timer_ = 0;

                for (int x = 0; x < size().x; ++x) {
                    for (int y = 0; y < size().y; ++y) {
                        PLATFORM.set_palette(parent_->layer(),
                                             x_position_ + x,
                                             y_position_ + y,
                                             default_palette());
                    }
                }

                if (is_powered_down()) {
                    schedule_repaint();
                }
            }
        }
    }
}



Island* Room::other_island() const
{
    if (&APP.player_island() == parent_) {
        if (APP.opponent_island()) {
            return &*APP.opponent_island();
        }
        return nullptr;
    } else {
        return &APP.player_island();
    }
}



Vec2<Fixnum> Room::origin() const
{
    auto origin = parent_->origin();
    origin.x += Fixnum::from_integer(x_position_ * 16);
    origin.y += Fixnum::from_integer(y_position_ * 16);
    return origin;
}



Vec2<Fixnum> Room::center() const
{
    auto o = origin();
    o.x += Fixnum::from_integer((size().x * 0.5f) * 16);
    o.y += Fixnum::from_integer((size().y * 0.5f) * 16);

    return o;
}



Vec2<Fixnum> Room::visual_center() const
{
    auto o = parent_->visual_origin();
    o.x += Fixnum::from_integer(x_position_ * 16);
    o.y += Fixnum::from_integer(y_position_ * 16);
    o.x += (Fixnum::from_integer(size().x) * 0.5_fixed) * 16.0_fixed;
    o.y += (Fixnum::from_integer(size().y) * 0.5_fixed) * 16.0_fixed;

    return o;
}



bool Room::description_visible()
{
    return parent_->interior_visible();
}



Weapon* Room::cast_weapon()
{
    return nullptr;
}



bool Room::allows_powerdown()
{
    return false;
}



void Room::set_target(const RoomCoord& target, bool pinned)
{
}



void Room::set_target(const TargetQueue& q, bool pinned)
{
}



Room::TargetCount Room::target_count() const
{
    return 0;
}



void Room::set_powerdown(bool powerdown)
{
    if (powerdown == powerdown_) {
        return;
    }

    powerdown_ = powerdown;
    unset_target();
    detach_drone(false);

    on_powerchange();

    if (is_player_island(parent())) {
        time_stream::event::PlayerRoomPowerchange e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.status_ = not powerdown_;
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomPowerchange e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.status_ = not powerdown_;
        APP.time_stream().push(APP.level_timer(), e);
    }

    parent_->recalculate_power_usage();
    parent_->schedule_repaint();
}



ScenePtr Room::do_select()
{
    if (length(characters_)) {

        const bool near = is_player_island(parent());

        RoomCoord cursor_loc;

        if (near) {
            cursor_loc = globals().near_cursor_loc_;
        } else {
            cursor_loc = globals().far_cursor_loc_;
        }

        const bool is_co_op = APP.game_mode() == App::GameMode::co_op;

        if (not near and APP.opponent().is_friendly()) {
            PLATFORM.speaker().play_sound("beep_error", 2);
            return null_scene();
        }

        auto try_move = [&](auto cond) -> ScenePtr {
            for (auto& character : characters_) {
                if (character->grid_position() == cursor_loc) {

                    if (is_co_op and character->co_op_locked()) {
                        PLATFORM.speaker().play_sound("beep_error", 2);
                        // TODO: notification scene instead!
                        return null_scene();
                    }

                    if (cond(character)) {

                        const auto ch_id = character->id();

                        using Next = ModifyCharacterScene;
                        using Await = MultiplayerCoOpAwaitChrLockScene;
                        auto n = make_deferred_scene<Next>(ch_id, near);

                        if (is_co_op) {
                            // NOTE: in co-op mode, we do not allow a player to
                            // move a character with an assigned movement
                            // path. Clears up a number of edge cases.
                            if (not character->has_movement_path() and
                                character->co_op_acquire_lock()) {
                                network::packet::CoOpChrLockAcquire pkt;
                                pkt.chr_id_.set(character->id());
                                network::transmit(pkt);

                                return make_scene<Await>(n, character->id());
                            }
                        } else {
                            return n();
                        }
                    }
                }
            }
            return null_scene();
        };


        if (auto scene = try_move(
                [&](auto& chr) { return chr->owner() == &APP.player(); })) {
            return scene;
        }

        if (APP.game_mode() == App::GameMode::sandbox) {
            if (auto scene = try_move([&](auto& chr) {
                    return chr->owner() == &APP.opponent();
                })) {
                return scene;
            }
        }
    }
    return null_scene();
}



ScenePtr Room::setup()
{
    // ...
    return null_scene();
}



ScenePtr Room::select(const RoomCoord& cursor)
{
    if (is_powered_down()) {
        if (auto scn = do_select()) {
            return scn;
        }

        auto future_scene = []() { return make_scene<ReadyScene>(); };
        PLATFORM.speaker().play_sound("beep_error", 2);
        auto str = SYSTR(error_powered_off);
        return make_scene<NotificationScene>(str->c_str(), future_scene);
    }

    return select_impl(cursor);
}



ScenePtr Room::select_impl(const RoomCoord& cursor)
{
    bool chr_at_cursor = false;
    for (auto& chr : characters()) {
        if (chr->grid_position() == cursor and chr->owner() == &APP.player()) {

            chr_at_cursor = true;
        }
    }

    if (parent_->interior_visible() or chr_at_cursor) {
        if (not parent_->interior_visible()) {
            show_island_interior(parent_);
        }
        return do_select();
    }

    return null_scene();
}



lisp::Value* Room::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    if (health() not_eq max_health()) {
        builder.push_back(L_INT(health()));
    }

    return builder.result();
}



void Room::deserialize(lisp::Value* list)
{
    if (lisp::length(list) >= 4) {
        __set_health(lisp::get_list(list, 3)->integer().value_);
    }
}



void Room::plot_walkable_zones(bool matrix[16][16], Character* for_character)
{
    // By default, treat every cell in the lowest row of a room as walkable for
    // NPCs. A few rooms, like staircases, cannons, walls, etc. will need to
    // provide different implementations.
    for (int x = 0; x < size().x; ++x) {
        matrix[x_position_ + x][y_position_ + size().y - 1] = true;
    }

    // For extreme edge cases: if a character is standing in a slot that it
    // shouldn't be, designate the slot as walkable so that the character can
    // walk out of it.
    if (for_character and for_character->parent() == parent()) {
        auto p = for_character->grid_position();
        matrix[p.x][p.y] = true;
    }
}



void Room::set_shielded(bool shielded)
{
    shielded_ = shielded;
}



extern SharedVariable deflector_shield_strength;



void Room::apply_damage(Health damage, const DamageConfiguration& conf)
{
    if (shielded_ and not conf.ignore_deflector_shield_) {
        if (damage <= deflector_shield_strength) {
            return;
        }
        damage -= deflector_shield_strength;
    }

    update_description();

    if (health_ not_eq 0) {
        // NOTE: damage < health() because when we're rewinding the destruction
        // of a block, we recreate the block with full health, and then rely on
        // one of the non-small RoomDamaged messages to populate a room's actual
        // health.
        if (damage <= 255 and damage < health()) {
            if (is_player_island(parent_)) {
                time_stream::event::PlayerRoomDamagedSmall e;
                e.x_ = position().x;
                e.y_ = position().y;
                e.diff_ = damage;
                APP.time_stream().push(APP.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomDamagedSmall e;
                e.x_ = position().x;
                e.y_ = position().y;
                e.diff_ = damage;
                APP.time_stream().push(APP.level_timer(), e);
            }
        } else {
            if (is_player_island(parent_)) {
                time_stream::event::PlayerRoomDamaged e;
                e.x_ = position().x;
                e.y_ = position().y;
                e.previous_health_.set(health_);
                APP.time_stream().push(APP.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomDamaged e;
                e.x_ = position().x;
                e.y_ = position().y;
                e.previous_health_.set(health_);
                APP.time_stream().push(APP.level_timer(), e);
            }
        }
    }

    auto diff = damage;
    if (diff > health_) {
        diff -= diff - health_;
    }

    if (damage > health_) {
        health_ = 0;
    } else {
        health_ -= damage;
    }
    set_injured();
    parent_->owner().on_room_damaged(*this);

    if ((int)accumulated_damage_ + damage < 256) {
        accumulated_damage_ += damage;
        show_damage_delay_frames_ = 60;
    } else {
        // Spawn damage info immediately?
    }

    ready();
}



void Room::__unsafe__transmute(MetaclassIndex m)
{
    Island* island = parent();
    const auto pos = position();
    void* address = this;
    const auto sz = size();

    auto m_prev = metaclass_index();

    EntityList<Character> chr_list;
    characters_.move_contents(chr_list);

    if (is_player_island(island)) {
        time_stream::event::PlayerRoomTransmuted e;
        e.prev_type_ = m_prev;
        e.x_ = pos.x;
        e.y_ = pos.y;
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomTransmuted e;
        e.prev_type_ = m_prev;
        e.x_ = pos.x;
        e.y_ = pos.y;
        APP.time_stream().push(APP.level_timer(), e);
    }

    this->finalize();
    this->~Room();

    auto& mt = *load_metaclass(m);

    mt->construct(address, island, pos);

    auto new_room = (Room*)address;
    chr_list.move_contents(new_room->characters_);

    const int size_diff_y = mt->size().y - sz.y;

    island->schedule_repaint();
    if (size_diff_y) {
        island->rooms().reindex(true);

        // NOTE: because rewinding a room-mutation calls move_room, which would
        // result in the room being moved twice.
        if (APP.game_speed() not_eq GameSpeed::rewind) {

            const auto new_pos = RoomCoord{pos.x, u8(pos.y - size_diff_y)};

            parent()->move_room(pos, new_pos);

            for (auto& chr : characters()) {
                auto pos = chr->grid_position();

                time_stream::event::CharacterPositionJump e;
                e.id_.set(chr->id());
                e.previous_x_ = pos.x;
                e.previous_y_ = pos.y;
                APP.time_stream().push(APP.level_timer(), e);

                chr->drop_movement_path();
                chr->set_idle();

                chr->set_grid_position({pos.x, u8(pos.y + size_diff_y)});
            }
        }
    }
}



bool Room::non_owner_selectable() const
{
    for (auto& chr : characters()) {

        bool has_boarded_character = chr->owner() not_eq &parent()->owner();

        if (has_boarded_character) {
            return true;
        }
    }

    return false;
}



void Room::burn_damage(Health amount)
{
    DamageConfiguration conf;
    conf.ignore_deflector_shield_ = true;

    if (APP.environment().id() == weather::SolarStorm::id_) {
        amount *= 2;
    }

    auto props = (*metaclass())->properties();
    if (props & RoomProperties::highly_flammable) {
        apply_damage(amount * 4, conf);
    } else if (not(props & RoomProperties::habitable)) {
        apply_damage(amount * 2, conf);
    } else {
        apply_damage(amount, conf);
        // Hmm: revist someday, I'm not sure about this yet
        // if (prev_health > 0 and health_ == 0) {
        //     if (not cast<PlunderedRoom>()) {
        //         convert_to_plundered();
        //     }
        // }
    }
}



void Room::heal(Health amount)
{
    update_description();

    if (is_player_island(parent_)) {
        time_stream::event::PlayerRoomRepaired e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.previous_health_.set(health_);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomRepaired e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.previous_health_.set(health_);
        APP.time_stream().push(APP.level_timer(), e);
    }

    const Health new_health = health_ + amount;
    health_ = std::min((*metaclass())->full_health(), new_health);
}



void Room::convert_to_plundered()
{
    if (parent() not_eq &APP.player_island()) {
        time_stream::event::OpponentRoomPlundered e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.type_ = metaclass_index_;
        APP.time_stream().push(APP.level_timer(), e);

    } else {
        time_stream::event::PlayerRoomPlundered e;
        e.x_ = position().x;
        e.y_ = position().y;
        e.type_ = metaclass_index_;
        APP.time_stream().push(APP.level_timer(), e);
    }

    // Ok, so when a character plunders a room, we don't actually want to
    // leave the health as zero, and let the engine erase the room. Doing so
    // would kill all of the characters attached to the room. Instead, we
    // want to detach all of the room's characters, spawn a bunch of
    // "plundered room" sentinel structures, and reattach each character to
    // the plundered room.

    // NOTE: This buffer should be plenty big enough. Only two characters
    // can occupy one x,y slot in a room (not as a limitation of the code,
    // but as a requirement of the gameplay). The max room size is generally
    // four slots, so we should have eight character entities at most...
    Buffer<EntityRef<Character>, 16> chrs;

    for (auto& chr : edit_characters()) {
        chrs.push_back(std::move(chr));
    }

    edit_characters().clear();

    // We need to splice our character list into the plundered room
    // structures created in the same slot. Set our hidden flag, so that any
    // calls to Island::get_room() do not return this room structure,
    // allowing us to deal with other rooms whose coordinates overlap with
    // our own.
    set_hidden(true);

    auto plunder_metac = load_metaclass("plundered-room");

    if (not plunder_metac) {
        error("failed to load metaclass");
        return;
    }

    for (int x = 0; x < size().x; x += (*plunder_metac)->size().x) {
        int y = 0;
        if (size().y % 2 not_eq 0) {
            // NOTE: plundered-room occupies two tiles vertically. For an
            // odd-sized room height, start at 1.
            y = 1;
        }
        for (; y < size().y; y += (*plunder_metac)->size().y) {
            const RoomCoord pos = {
                u8(((u8)x_position_) + x),
                u8(((u8)y_position_) + y),
            };
            (*plunder_metac)->create(parent_, pos);
        }
    }

    // Now that we've created plundered rooms where the now-detached room
    // was, we can add the characters back to the parent island.
    for (auto& chr : chrs) {
        parent_->add_character(std::move(chr));
    }

    set_hidden(false);
    ready();
}



bool Room::target_pinned() const
{
    return false;
}



void Room::plunder(Health damage)
{
    if (size().x == 1 and size().y == 1) {
        // This block cannot be converted to a plundered room, because its
        // dimensions are smaller!
        return;
    }

    apply_damage(damage, {.ignore_deflector_shield_ = true});

    if (health_ == 0) {

        if (parent() not_eq &APP.player_island()) {
            // You get some coins for plundering a room
            PLATFORM.speaker().play_sound("coin", 2);
            APP.set_coins(APP.coins() + (*metaclass())->cost() * 0.3f);
        }

        APP.player().on_room_plundered(*this);

        convert_to_plundered();
    }
}



ATP Room::get_atp() const
{
    if (ai_aware_) {
        return (*metaclass())->atp_value();
    } else {
        return 2.0_atp;
    }
}



void Room::init_ai_awareness()
{
    bool concealed[4][4];
    memset(concealed, 0, sizeof concealed);

    ai_aware_ = true;

    for (auto& r : parent_->rooms()) {
        auto rx = position().x;
        auto ry = position().y;
        for (int x = rx; x < rx + size().x; ++x) {
            for (int y = ry; y < ry + size().y; ++y) {
                if (r->cloaks_coordinate({(u8)x, (u8)y})) {
                    concealed[x - rx][y - ry] = true;
                }
            }
        }
    }

    bool fully_concealed = true;
    for (int x = 0; x < size().x; ++x) {
        for (int y = 0; y < size().y; ++y) {
            if (not concealed[x][y]) {
                fully_concealed = false;
                break;
            }
        }
    }

    if (fully_concealed) {
        set_ai_aware(false);
        set_visually_cloaked(true);
    }

    init_awareness_upon_unpause_ = false;
}



void Room::project_deflector_shield()
{
}



void Room::set_ai_aware(bool ai_aware)
{
    if (str_eq(name(), "cloak")) {
        // A cloak block cannot be cloaked.
        ai_aware_ = true;
        return;
    }

    update_description();

    if (ai_aware_ not_eq ai_aware) {
        if (is_player_island(parent())) {
            time_stream::event::PlayerRoomAiAwareness e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            e.prev_aware_ = ai_aware_;
            APP.time_stream().push(APP.level_timer(), e);
        } else {
            time_stream::event::OpponentRoomAiAwareness e;
            e.room_x_ = position().x;
            e.room_y_ = position().y;
            e.prev_aware_ = ai_aware_;
            APP.time_stream().push(APP.level_timer(), e);
        }
    }

    ai_aware_ = ai_aware;
}



Health Room::max_health() const
{
    return (*metaclass())->full_health();
}



class Debris : public Entity
{
public:
    Debris(const Vec2<Fixnum>& position, Fixnum max_y, int tile)
        : Entity({}), max_y_(max_y)
    {
        if (tile <= 4) {
            sprite_.set_tidx_8x8(30, 3 + tile);
        } else {
            sprite_.set_tidx_8x8(34, 0);
        }
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w8_h8);
        sprite_.set_origin({4, 4});
    }


    void update(Time delta) override
    {
        // NOTE: a lot of our update logic simply multiplies speed by delta
        // time. But debris has gravity applied, so we run multiple update steps
        // in a loop.

        if (delta == 0) {
            return;
        }
        timer_ += delta;
        if (timer_ > seconds(2)) {
            kill();
        }

        delta += remainder_;
        remainder_ = 0;

        auto pos = sprite_.get_position();
        while (delta >= 16666 / 4) {
            delta -= 16666 / 4;
            speed_.y = speed_.y + gravity_;
            pos = pos + speed_;
        }
        remainder_ += delta;
        sprite_.set_position(pos);

        if (pos.y > max_y_) {
            kill();
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }


    Vec2<Fixnum> speed_;
    Fixnum gravity_;

private:
    Time timer_ = 0;
    Time remainder_ = 0;
    Fixnum max_y_;
};



class OffscreenWarning : public Entity
{
private:
    Time timer_ = 0;
    RoomCoord coord_;
    bool countdown_ = false;
    Island* island_;
    u8 gfx_;

public:
    OffscreenWarning(RoomCoord c, Island* island, u8 gfx)
        : Entity({}), coord_(c), island_(island), gfx_(gfx)
    {
        sprite_.set_size(Sprite::Size::w16_h16);
        sprite_.set_tidx_16x16(gfx, 0);

        for (auto& ent : APP.effects()) {
            if (auto other = ent->cast_offscreen_warning()) {
                if (other->coord_.y == coord_.y) {
                    kill();
                }
            }
        }
    }


    OffscreenWarning* cast_offscreen_warning() override
    {
        return this;
    }


    void update(Time delta) override
    {
        if (not APP.opponent_island()) {
            kill();
            return;
        }
        if (APP.opponent_island()->is_destroyed() or
            APP.player_island().is_destroyed()) {
            kill();
        }

        auto pos = island_->visual_origin();
        pos.x += Fixnum::from_integer(coord_.x * 16);
        pos.y += Fixnum::from_integer(coord_.y * 16);

        if (not countdown_) {
            if (pos.x.as_integer() + 8 >
                (int)(PLATFORM.screen().get_view().int_center().x +
                      PLATFORM.screen().size().x)) {
                pos.x = PLATFORM.screen().get_view().int_center().x +
                        PLATFORM.screen().size().x - (16 + 4);
                sprite_.set_flip({true, false});
                sprite_.set_tidx_16x16(gfx_, 0);
            } else if (pos.x.as_integer() <
                       PLATFORM.screen().get_view().int_center().x) {
                pos.x = PLATFORM.screen().get_view().int_center().x + 4;
                sprite_.set_flip({});
                sprite_.set_tidx_16x16(gfx_, 0);
            } else {
                sprite_.set_flip({});
                sprite_.set_tidx_16x16(gfx_, 1);
                countdown_ = true;
            }
        } else {
            timer_ += delta;
            if (timer_ > milliseconds(1450)) {
                sprite_.set_alpha(Sprite::Alpha::translucent);
            }
            if (timer_ > milliseconds(1500)) {
                kill();
            }
        }

        sprite_.set_position(pos);
    }


    void rewind(Time delta) override
    {
        kill();
    }
};



void Room::finalize()
{
    finalized_ = true;

    if (health_ == 0) {
        for (auto& c : characters_) {

            auto position = c->sprite().get_position();
            APP.on_timeout(milliseconds(500), [pos = position]() {
                if (auto e = alloc_entity<Ghost>(pos)) {
                    APP.effects().push(std::move(e));
                }
            });
        }

        const auto max_y =
            parent()->origin().y + 16.0_fixed * 16.0_fixed + 32.0_fixed;

        bool is_offscreen =
            (visual_center().x.as_integer() <
             PLATFORM.screen().get_view().int_center().x + 8 -
                 (size().x * 16) / 2) or
            (visual_center().x.as_integer() - (size().x * 16) / 2 >
             (int)(PLATFORM.screen().get_view().int_center().x +
                   PLATFORM.screen().size().x));

        const int t = debris_tile();

        if (is_offscreen) {
            u8 gfx = 56;
            if (parent() == APP.opponent_island()) {
                gfx = 57;
            }
            if (auto ent =
                    alloc_entity<OffscreenWarning>(position(), parent_, gfx)) {
                APP.effects().push(std::move(ent));
            }
        } else if (t) {
            const int count = debris_count();
            for (int i = 0; i < count; ++i) {
                if (auto e = alloc_entity<Debris>(center(), max_y, t)) {
                    int angle = rng::choice<45>(rng::utility_state);
                    if (rng::choice<2>(rng::utility_state)) {
                        angle = 360 - 45;
                    }
                    auto dir = rotate({0, 1}, angle);
                    e->speed_.x = Fixnum(dir.x);
                    e->speed_.y = Fixnum(-1 * dir.y);
                    e->speed_.y -= 0.3_fixed;
                    e->speed_ = e->speed_ * 0.5_fixed;
                    e->gravity_ = Fixnum(0.06f * 0.15f) +
                                  Fixnum::from_integer(
                                      rng::choice<3>(rng::utility_state)) *
                                      0.003_fixed;
                    APP.effects().push(std::move(e));
                }
            }
        }
    }

    if (co_op_locked_) {
        // What else can we do? Yeah, maybe not perfectly correct. But if we
        // don't release the lock from the rest of the group, the room could end
        // up deadlocked, which is worse than two players concurrently editing a
        // Room, I think. By concurrent, I mean of course across a network. We
        // don't have any sort of memory race-condition, just a networking one
        // involving state sync across consoles. If a room associated with a
        // group is locked, and subsequently destroyed, the other rooms in the
        // group will remain locked on both devices, and can no longer be
        // unlocked by any means. i.e. the rooms can't even be salvaged by the
        // player, as doing so requires acquiring the lock. Releasing the whole
        // group when a member room's destroyed clears up a large number of
        // edge-cases.
        co_op_peer_release_lock();
    }
}



const char* Room::upgrade_mt_name() const
{
    return nullptr;
}



void Room::ready()
{
    if (not dispatch_queued_) {
        parent_->dispatch_room(this);
    }
}



bool Room::attach_drone(SharedEntityRef<Drone>)
{
    warning(format("Attach drone to incompatible room %", name()).c_str());

    return false;
}



void Room::detach_drone(bool quiet)
{
    // Unimplemented.
}



Optional<SharedEntityRef<Drone>> Room::drone() const
{
    // Unimplemented.
    // TODO: raise fatal error?

    return {};
}



void Room::set_group(Group group)
{
    group_ = (u8)group;
    update_description();
}



Room::Group Room::group() const
{
    return (Group)group_;
}



Room::~Room()
{
    if (not finalized_) {
        Platform::fatal("room destroyed without invoking finalizer!");
    }

    // Because the dispatch list uses plain unchecked raw pointers, we want to
    // be extra careful. Now, we could scan an island's entire dispatch list and
    // remove ourself, but instead, tell the island to drop its dispatch list
    // and update all rooms on the next update step.
    if (parent_) {
        parent_->cancel_dispatch();
    }
}



void Room::render_scaffolding(TileId buffer[16][16])
{
    auto p = position();
    auto s = size();

    auto sal = parent()->interior_visible()
                   ? (int)InteriorTile::scaffolding_angled_l
                   : (int)Tile::scaffolding_angled_l;

    auto sar = parent()->interior_visible()
                   ? (int)InteriorTile::scaffolding_angled_r
                   : (int)Tile::scaffolding_angled_r;

    auto strut = parent()->interior_visible() ? (int)InteriorTile::strut
                                              : (int)Tile::strut;

    auto strut_t = parent()->interior_visible() ? (int)InteriorTile::strut_top
                                                : (int)Tile::strut_top;

    bool placed_strut = false;

    if (s.x == 1) {
        for (int y = p.y + s.y; y < 15; ++y) {
            if (buffer[p.x][y]) {
                break;
            }

            if (placed_strut) {
                buffer[p.x][y] = strut;
            } else {
                buffer[p.x][y] = strut_t;
                placed_strut = true;
            }
        }
        placed_strut = false;

        return;
    }

    if (buffer[p.x][p.y + s.y] not_eq 0 and buffer[p.x + 1][p.y + s.y] == 0) {

        buffer[p.x + 1][p.y + s.y] = sal;

    } else if (buffer[p.x][p.y + s.y] == 0 and
               buffer[p.x + (s.x - 1)][p.y + s.y] not_eq 0) {

        if (s.x == 2) {
            buffer[p.x + (s.x - 2)][p.y + s.y] = sar;
        } else if (buffer[p.x + (s.x - 2)][p.y + s.y] == 0) {
            buffer[p.x + (s.x - 2)][p.y + s.y] = sar;
        }

    } else if (buffer[p.x][p.y + s.y] == 0 and
               buffer[p.x + (s.x - 1)][p.y + s.y] == 0) {
        for (int x = p.x; x < p.x + s.x; ++x) {
            for (int y = p.y + s.y; y < 15; ++y) {
                if (buffer[x][y] and buffer[x][y]) {
                    break;
                }

                if (placed_strut) {
                    buffer[x][y] = strut;
                } else {
                    buffer[x][y] = strut_t;
                    placed_strut = true;
                }
            }
            placed_strut = false;
        }
    }
}



ScenePtr Room::co_op_acquire_lock(DeferredScene next)
{
    if (co_op_locked_) {
        return null_scene();
    }

    if (not co_op_peer_acquire_lock()) {
        return null_scene();
    }

    network::packet::CoOpRoomLockAcquire pkt;
    pkt.x_ = position().x;
    pkt.y_ = position().y;
    network::transmit(pkt);

    return make_scene<MultiplayerCoOpAwaitLockScene>(next, position());
}



bool Room::co_op_peer_acquire_lock()
{
    if (co_op_locked_) {
        return false;
    }

    if (group_) {
        // Part of a group, so lock the whole group!
        for (auto& room : parent()->rooms()) {
            if (room->group_ == group_) {
                if (room->co_op_locked_) {
                    return false;
                }
            }
        }

        for (auto& room : parent()->rooms()) {
            if (room->group_ == group_) {
                room->co_op_locked_ = true;
            }
        }
    }

    co_op_locked_ = true;

    return true;
}



void Room::co_op_peer_release_lock()
{
    if (group_) {
        for (auto& room : parent()->rooms()) {
            if (room->group_ == group_) {
                room->co_op_locked_ = false;
            }
        }
    }

    co_op_locked_ = false;
}



void Room::co_op_release_lock()
{
    co_op_peer_release_lock();

    network::packet::CoOpRoomLockRelease pkt;
    pkt.x_ = position().x;
    pkt.y_ = position().y;
    network::transmit(pkt);
}



bool Room::co_op_locked() const
{
    return co_op_locked_;
}



void Room::parent_layout_changed(RoomCoord moved_from, RoomCoord to)
{
}



void Room::on_salvage()
{
}



void Room::amplify(bool enabled)
{
}



} // namespace skyland
