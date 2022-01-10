#pragma once


#include "graphics/overlay.hpp"
#include "worldScene.hpp"



namespace skyland {



class KeyComboScene : public ActiveWorldScene {
public:
    KeyComboScene(bool near) : near_(near)
    {
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    bool near_;
    StringBuffer<32> text_data_;
    std::optional<Text> text_;
};



} // namespace skyland
