#pragma once

#include "worldScene.hpp"



namespace skyland {



class ModifierKeyHintScene : public ActiveWorldScene
{
public:
    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


private:
    std::optional<Text> title_;
    Buffer<Text, 5> text_;
};



} // namespace skyland
