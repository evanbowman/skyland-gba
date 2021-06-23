#pragma once



#include "memory/buffer.hpp"
#include "skyland/entity.hpp"
#include "skyland/path.hpp"



namespace skyland {



class Island;
class Player;



class BasicCharacter : public Entity {
public:
    BasicCharacter(Island* parent,
                   Player* owner,
                   const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    const Vec2<u8>& grid_position() const
    {
        return grid_position_;
    }


    void set_grid_position(const Vec2<u8>& pos)
    {
        grid_position_ = pos;
    }


    void set_movement_path(Path path)
    {
        movement_path_ = std::move(path);
    }


    bool has_movement_path() const
    {
        return static_cast<bool>(movement_path_);
    }


    Island* parent() const
    {
        return parent_;
    }


    Player* owner() const
    {
        return owner_;
    }


    void heal(int amount)
    {
        if (health_ + amount > 255) {
            health_ = 255;
        } else {
            health_ += amount;
        }
    }

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


    enum class State {
        check_surroundings,
        moving_or_idle,
        fighting,
    };


private:
    Island* parent_;
    Player* owner_;
    Vec2<u8> grid_position_;
    Microseconds timer_ = 0;
    bool awaiting_movement_ = true;
    bool can_move_ = false;
    State state_ = State::moving_or_idle;

    u8 health_ = 255;

    std::optional<Path> movement_path_;

    void reassign_room(const Vec2<u8>& old_coord,
                       const Vec2<u8>& new_coord);


    void movement_step(Microseconds delta);

};



} // namespace skyland
