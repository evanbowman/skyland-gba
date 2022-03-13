#pragma once



#include "worldScene.hpp"



namespace skyland {



class InspectP2Scene final : public ActiveWorldScene
{
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


public:
    Microseconds cursor_anim_timer_;
    Microseconds describe_room_timer_ = seconds(1);
    std::optional<Text> room_description_;
    u8 cursor_anim_frame_;
    bool await_start_key_ = false;
};



} // namespace skyland
