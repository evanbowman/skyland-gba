#pragma once

#include "allocator.hpp"
#include "notificationScene.hpp"



namespace skyland {



class TransportCharacterScene : public NotificationScene
{
public:
    TransportCharacterScene(Platform& pfrm, Vec2<u8> origin);


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    std::optional<DynamicMemory<bool[16][16]>> matrix_;

    Microseconds cursor_anim_timer_ = 0;
    u8 cursor_anim_frame_ = 0;

    Vec2<u8> origin_;
};



} // namespace skyland
