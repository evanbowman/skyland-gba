#pragma once



#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"
#include "bulkAllocator.hpp"



namespace skyland {



class Island;



class PlayerIslandDestroyedScene : public WorldScene {
public:
    PlayerIslandDestroyedScene(Island* island) : island_(island)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
    void display(Platform&, App&) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


private:
    Microseconds timer_ = 0;
    Float sink_speed_ = 0.000011f;
    Island* island_;

    Buffer<Text, 5> lines_;

    struct Confetti {
        Float x_;
        Float y_;
        Float speed_;
        int angle_;
        Float gravity_;
        u8 img_;
        u8 clr_;
        u8 kf_;
        u8 anim_;
        u8 fall_slower_;
    };

    using ConfettiBuffer = Buffer<Confetti, 60>;

    std::optional<DynamicMemory<ConfettiBuffer>> confetti_;


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

    enum class ConfettiState {
        dormant,
        wait_1,
        confetti_pop_1,
        wait_2,
        confetti_pop_2,
        wait_3,
    } confetti_state_ = ConfettiState::dormant;

    Microseconds confetti_timer_ = 0;
};



} // namespace skyland
