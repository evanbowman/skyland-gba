#pragma once



#include "worldScene.hpp"



namespace skyland
{



class NotificationScene : public ActiveWorldScene
{
public:
    NotificationScene(const StringBuffer<30>& msg, DeferredScene next_scene)
        : next_scene_(next_scene), msg_(msg)
    {
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


public:
    std::optional<Text> description_;
    DeferredScene next_scene_;
    StringBuffer<30> msg_;
};



} // namespace skyland
