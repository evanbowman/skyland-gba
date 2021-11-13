#pragma once


#include "worldScene.hpp"



namespace skyland {



class ReadyScene : public ActiveWorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    Microseconds cursor_anim_timer_;
    Microseconds describe_room_timer_ = seconds(1);
    u8 cursor_anim_frame_;
    std::optional<Text> room_description_;

    Microseconds start_key_held_timer_ = 0;
};



} // namespace skyland
