#pragma once



#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class Island;



class PlayerIslandDestroyedScene : public WorldScene {
public:
    PlayerIslandDestroyedScene(Island* island) :
        island_(island)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

private:
    Microseconds timer_ = 0;
    Float sink_speed_ = 0.000011f;
    Island* island_;

    enum class AnimState {
        init,
        explosion_wait1,
        explosion_wait2,
        fade,
        wait_1,
        add_score,
        wait_2,
        fade_out,
        idle,
    } anim_state_ = AnimState::init;
};



} // namespace skyland
