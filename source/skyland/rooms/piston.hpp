#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Piston : public Room
{
public:
    Piston(Island* parent, const Vec2<u8>& position, const char* name = name());


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Category category()
    {
        return Category::wall;
    }


    static u32 properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::disabled_in_tutorials;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1.f;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "piston";
    }


    static SystemString ui_name()
    {
        return SystemString::block_piston;
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const Vec2<u8>& cursor) override;


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


    void ___rewind___finished_reload(Platform&, App&) override;


    enum Direction { right, left, up, down };


    virtual bool is_sticky() const;


private:
    bool opened_ = false;
    Direction dir_ = Direction::up;
};



class StickyPiston : public Piston
{
public:
    StickyPiston(Island* parent, const Vec2<u8>& position) :
        Piston(parent, position, name())
    {
    }


    static const char* name()
    {
        return "sticky-piston";
    }


    virtual bool is_sticky() const
    {
        return true;
    }


    static SystemString ui_name()
    {
        return SystemString::block_sticky_piston;
    }


};



} // namespace skyland
