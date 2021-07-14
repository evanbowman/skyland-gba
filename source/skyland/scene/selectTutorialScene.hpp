#pragma once


#include "graphics/overlay.hpp"
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class SelectTutorialScene : public Scene {
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    void show_options(Platform&);

    enum class State {
        fade_in,
        idle,
        fade_out,
    } state_ = State::idle;


    std::optional<lisp::Protected> tutorials_;
    Buffer<Text, 5> text_;

    int page_ = 0;
    int cursor_ = 0;

    int page_count_ = 0;

    Microseconds timer_ = 0;

    bool exit_ = false;
};



} // namespace skyland
