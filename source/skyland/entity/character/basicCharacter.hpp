#pragma once


#include "skyland/entity.hpp"



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


    void set_outdoors(bool outdoors)
    {
        outdoors_ = outdoors;
    }


private:
    Island* parent_;
    Vec2<u8> grid_position_;
    bool outdoors_ = false;
};



}
