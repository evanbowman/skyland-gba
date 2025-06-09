////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "characterStats.hpp"
#include "memory/buffer.hpp"
#include "memory/extension.hpp"
#include "script/value.hpp"
#include "skyland/characterId.hpp"
#include "skyland/entity.hpp"
#include "skyland/path.hpp"



// NOTE: Originally, I called this class BasicCharacter, because I planned on
// creating other types of derived characters. But in practice, BasicCharacter
// is the only type of character, and includes all of the different behavior
// needed for crewmember logic.



namespace skyland
{



class Island;
class Player;



class Character : public Entity
{
public:
    static constexpr const Health max_health = 255;


    Character(Island* parent,
              Player* owner,
              const RoomCoord& position,
              bool is_replicant);


    void set_spr_flip(bool flipped);


    void finalize();


    void update(Time delta, Room* room);


    void update(Time delta) override final;


    void rewind(Time delta) override;


    const RoomCoord& grid_position() const
    {
        return grid_position_;
    }


    void set_grid_position(const RoomCoord& pos)
    {
        grid_position_ = pos;
    }


    void set_movement_path(Path path);


    bool has_movement_path() const
    {
        return static_cast<bool>(movement_path_);
    }


    void drop_movement_path()
    {
        movement_path_.reset();
    }


    PathBuffer* get_movement_path()
    {
        if (movement_path_) {
            return movement_path_->obj_.get();
        }
        return nullptr;
    }



    Optional<RoomCoord> destination() const
    {
        if (has_movement_path()) {
            if (not(*movement_path_)->empty()) {
                return (**movement_path_)[0];
            }
        }
        return {};
    }


    Island* parent() const
    {
        return parent_;
    }


    void set_parent(Island* parent)
    {
        parent_ = parent;
    }


    void transported();


    Player* owner() const;


    void heal(int amount);


    void apply_damage(Health damage) override;


    void __set_health(Health amount)
    {
        health_ = amount;
    }


    u8 get_max_health() const;


    void set_max_health(u8 value);


    void set_idle();


    // The character will post an awaiting movement flag when it's either idle,
    // or when it's finished moving to the next coordinate in its path. We wait
    // for all characters on an island to be awaiting movement before allowing
    // them to move (by calling set_can_move()). This greatly simplifies the
    // game logic.
    bool is_awaiting_movement() const
    {
        return awaiting_movement_;
    }


    void set_can_move();


    void rewind_movement_step(const RoomCoord& new_pos);


    enum class State : u8 {
        moving_or_idle,
        fighting,
        plunder_room,
        repair_room,
        extinguish_fire,
        after_transport,
        chatting,
    };


    State state() const
    {
        return state_;
    }


    bool is_replicant() const
    {
        return is_replicant_;
    }


    u16 idle_count() const
    {
        return idle_count_;
    }


    CharacterId id() const
    {
        return id_;
    }


    void __assign_id(CharacterId id)
    {
        id_ = id;
    }


    enum class Race : int { default_race, goblin, hostile_human, dog };


    void set_race(Race race);


    Race get_race() const;


    static constexpr const int multiplayer_vs_client_chr_start_id = 4000;


    static void __reset_ids(int start_id = 1);


    static void __rebase_ids(CharacterId id);


    bool co_op_acquire_lock();
    void co_op_release_lock();

    bool co_op_locked() const;


    lisp::Value* serialize();


    static std::pair<Character*, Room*> find_by_id(CharacterId id);


    void mark()
    {
        mark_ = 1;
    }


    bool marked() const
    {
        return mark_;
    }


    void unmark()
    {
        mark_ = 0;
    }


    void ai_mark()
    {
        ai_mark_ = 1;
    }


    bool ai_marked() const
    {
        return ai_mark_;
    }


    void ai_unmark()
    {
        ai_mark_ = 0;
    }


    const char* name() const;


    bool has_alternate_sprite() const
    {
        return (int)get_race() or is_replicant();
    }


    struct DrawTransform
    {
        Fixnum y_displace_;
        int priority_;
        ColorMix mix_;
        Sprite::Alpha alpha_;

        DrawTransform()
            : y_displace_(0.0_fixed), priority_(1), mix_(),
              alpha_(Sprite::Alpha::opaque)
        {
        }
    };
    void draw(Platform::Screen& s, const DrawTransform& tr = DrawTransform{});


    using IconId = u8;


    IconId get_icon() const
    {
        return icon_;
    }


    void set_icon(IconId id)
    {
        icon_ = id;
    }


    // When the AI is active, a pinned character will maintain its position
    // until it finishes all current tasks available at its position, then will
    // resume automation.
    void pin();
    void unpin();

    // A superpinned character will ignore the AI completely.
    void superpin(bool drop_path = true);
    void un_superpin();


    bool ai_automated() const;
    bool is_superpinned() const;


    void apply_radiation_damage(Health amount);


    void set_phase(u8 phase);


    CompleteCharacterStats& stats();


    void record_stats();


    bool wants_to_chat() const;
    void set_wants_to_chat(bool status);


private:
    Island* parent_;
    Time timer_ = 0;
    Time anim_timer_ = 0;
    Optional<Path> movement_path_;
    mutable Optional<Platform::DynamicTexturePtr> custom_texture_;

    CharacterId id_;
    u16 idle_count_ = 0;

    RoomCoord grid_position_;
    State state_ = State::moving_or_idle;

    u8 awaiting_movement_ : 1;
    u8 can_move_ : 1;
    u8 is_replicant_ : 1;
    u8 co_op_locked_ : 1;
    u8 mark_ : 1;
    u8 ai_mark_ : 1;
    u8 ai_automated_ : 1;
    u8 superpinned_ : 1;
    u8 wants_to_chat_ : 1 = 0;
    u8 antisocial_ : 4 = 0;
    u8 race_ : 3;
    u8 owner_is_player_ : 1 = 0;
    u8 parent_near_ : 1 = 0;
    u8 repair_wb_ : 2 = 0;
    u8 unused_ : 4;

    u8 icon_;
    u8 radiation_counter_ = 0;
    u8 max_health_ = max_health;


    ExtensionField<CompleteCharacterStats> stats_;

    bool has_opponent(Room* room);

    bool reassign_room(const RoomCoord& old_coord, const RoomCoord& new_coord);


    void movement_step(Time delta, Room* current_room);

    void update_attack(Time delta);


    void update_favorite_room_stat(Room* current_room);
};



} // namespace skyland
