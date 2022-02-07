#pragma once



#include "worldScene.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



class LevelCompleteOptionsScene : public WorldScene {
public:


    LevelCompleteOptionsScene(bool fade_in = false)
    {
        if (fade_in) {
            state_ = State::fade_in;
        }
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& prev) override;


private:

    void show_cursor(Platform& pfrm);


    enum class State {
        select,
        fade_out,
        fade_resume,
        fade_in
    } state_ = State::select;

    int cursor_ = 0;

    Buffer<Text, 3> options_;
    Microseconds timer_ = 0;
};



}
