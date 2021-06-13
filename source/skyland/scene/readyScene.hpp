#pragma once


#include "worldScene.hpp"



namespace skyland {



class ReadyScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    Microseconds cursor_anim_timer_;
    u8 cursor_anim_frame_;
};



} // namespace skyland
