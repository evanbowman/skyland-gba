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


#pragma once

#include "memory/buffer.hpp"
#include "skyland/characterId.hpp"
#include "skyland/entity.hpp"
#include "skyland/path.hpp"



namespace skyland
{



class Island;
class Player;



class BasicCharacter : public Entity
{
public:
    static constexpr const Health max_health = 255;


    BasicCharacter(Island* parent,
                   Player* owner,
                   const RoomCoord& position,
                   bool is_replicant);


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


    Player* owner() const
    {
        return owner_;
    }


    void heal(int amount);


    void apply_damage(Health damage);


    void __set_health(Health amount)
    {
        health_ = amount;
    }


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


    void set_race(int race);


    int get_race() const;


    static constexpr const int multiplayer_vs_client_chr_start_id = 4000;


    static void __reset_ids(int start_id = 1);


    static void __rebase_ids(CharacterId id);


    bool co_op_acquire_lock();
    void co_op_release_lock();

    bool co_op_locked() const;


    static std::pair<BasicCharacter*, Room*> find_by_id(CharacterId id);


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
        return get_race() or is_replicant();
    }

    Sprite prepare_sprite() const;


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


private:
    Island* parent_;
    Player* owner_;
    RoomCoord grid_position_;
    Time timer_ = 0;
    Time anim_timer_ = 0;

    CharacterId id_;
    u16 idle_count_ = 0;

    u8 awaiting_movement_ : 1;
    u8 can_move_ : 1;
    u8 is_replicant_ : 1;

    u8 co_op_locked_ : 1;

    u8 mark_ : 1;
    u8 ai_mark_ : 1;

    u8 race_ : 2;

    State state_ = State::moving_or_idle;

    u8 icon_;
    u8 radiation_counter_ = 0;

    u8 ai_automated_ : 1;
    u8 superpinned_ : 1;
    u8 unused_ : 6;


    bool has_opponent(Room* room);

    Optional<Path> movement_path_;

    bool reassign_room(const RoomCoord& old_coord, const RoomCoord& new_coord);


    void movement_step(Time delta);

    void update_attack(Time delta);
};



} // namespace skyland
