#pragma once


#include "worldScene.hpp"



namespace skyland {



class Room;



class RecoverCharacterScene : public ActiveWorldScene {
public:
    RecoverCharacterScene(const Vec2<u8>& transporter_loc);


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


private:
    Vec2<u8> transporter_loc_;
};



} // namespace skyland
