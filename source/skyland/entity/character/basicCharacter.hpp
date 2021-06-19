#pragma once


#include "memory/buffer.hpp"
#include "skyland/entity.hpp"
#include "skyland/path.hpp"



namespace skyland {


class Island;



class BasicCharacter : public Entity {
public:
    BasicCharacter(Island* parent, const Vec2<u8>& position);


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


private:
    Island* parent_;
    Vec2<u8> grid_position_;
    Vec2<Float> real_position_;
    Microseconds movement_timer_ = 0;

    std::optional<Path> movement_path_;
};



} // namespace skyland
