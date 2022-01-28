#pragma once


#include "skyland/flag.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class Paint {
public:

    Paint(u8 width = 13,
          u8 height = 11,
          u8 origin_x = 3,
          u8 origin_y = 3) :
        width_(width),
        height_(height),
        origin_x_(origin_x),
        origin_y_(origin_y)
    {
    }


    virtual ~Paint()
    {
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta);


    void display(Platform& pfrm, App& app);


    void init(Platform&, App&);


    virtual void show(Platform&, App&);
    void draw_rulers(Platform&);


    virtual u8 get_pixel(App& app, u8 x, u8 y) = 0;
    virtual void set_pixel(App& app, u8 x, u8 y, u8 value) = 0;


    u8 width() const
    {
        return width_;
    }


    u8 height() const
    {
        return height_;
    }


private:
    Microseconds cursor_move_tic_ = 0;

    bool ready_ = false;

    u16 palette_[16];


    Vec2<u8> cursor_;
    u32 color_ = 0;

    u8 width_;
    u8 height_;
    u8 origin_x_;
    u8 origin_y_;

};



class FlagDesignerModule :
        public Module<FlagDesignerModule>,
        public Paint {

public:

    static const char* module_name()
    {
        return "Flag Designer";
    }


    static u16 icon()
    {
        return 952;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    static bool run_scripts()
    {
        return false;
    }


    u8 get_pixel(App& app, u8 x, u8 y) override;
    void set_pixel(App& app, u8 x, u8 y, u8 value) override;


    void show(Platform&, App&) override;


private:
    static Factory factory_;
};



} // namespace skyland
