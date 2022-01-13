#pragma once


#include "script/lisp.hpp"
#include "worldScene.hpp"



namespace skyland {


// The script API provides a function through which the script writer can
// request an input coordinate from the user. This scene implements the cursor
// selection and invokes the script callback when the user makes a selection.



class SelInputScene : public ActiveWorldScene {
public:
    SelInputScene(lisp::Value* parameters, bool near)
        : parameters_(parameters), near_(near), started_near_(near)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    lisp::Protected parameters_;
    bool near_;
    bool started_near_;

    std::optional<Text> text_;
    Vec2<u8> cached_near_cursor_;
    Vec2<u8> cached_far_cursor_;
    Microseconds cursor_anim_timer_ = 0;

    bool cursor_anim_frame_ = false;
};



} // namespace skyland
