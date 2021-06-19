#pragma once



#include "worldScene.hpp"



namespace skyland {



class InspectP2Scene : public ActiveWorldScene {
public:
    void enter(Platform&, App&, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


public:
    Microseconds cursor_anim_timer_;
    u8 cursor_anim_frame_;
};



} // namespace skyland
