#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class CargoBay : public Room
{
public:
    CargoBay(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void display(Platform::Screen&) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;

    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "cargo-bay";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cargo_bay;
    }


    static Icon icon()
    {
        return 1320;
    }


    static Icon unsel_icon()
    {
        return 1336;
    }


    const char* cargo() const
    {
        return cargo_;
    }


    u8 cargo_count() const
    {
        return count_;
    }


    virtual lisp::Value* serialize() override;
    virtual void deserialize(lisp::Value*) override;


    bool set_cargo(const char* cargo, u8 count);


private:
    char cargo_[19];
    u8 count_;
};



} // namespace skyland
