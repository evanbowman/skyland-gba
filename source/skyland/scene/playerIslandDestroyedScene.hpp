#pragma once



#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class Island;



class PlayerIslandDestroyedScene : public WorldScene {
public:
    PlayerIslandDestroyedScene(Island* island) : island_(island)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


private:
    Microseconds timer_ = 0;
    Float sink_speed_ = 0.000011f;
    Island* island_;

    Buffer<Text, 5> lines_;

    void show_stats(Platform&, App&);

    Microseconds stat_timer_ = 0;

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
        fade_complete,
    } anim_state_ = AnimState::init;
};



} // namespace skyland
