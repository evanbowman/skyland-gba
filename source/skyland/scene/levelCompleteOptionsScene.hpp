#pragma once



#include "confetti.hpp"
#include "graphics/overlay.hpp"
#include "worldScene.hpp"



namespace skyland {



class LevelCompleteOptionsScene : public WorldScene
{
public:
    LevelCompleteOptionsScene(
        bool fade_in = false,
        std::optional<DynamicMemory<ConfettiBuffer>> confetti = {})
        : confetti_(std::move(confetti))
    {
        if (fade_in) {
            state_ = State::fade_in;
        }
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& prev) override;


    void display(Platform& pfrm, App& app) override;


private:
    void show_cursor(Platform& pfrm);


    enum class State {
        select,
        fade_out,
        fade_resume,
        fade_in
    } state_ = State::select;

    int cursor_ = 0;

    std::optional<DynamicMemory<ConfettiBuffer>> confetti_;

    Buffer<Text, 3> options_;
    Microseconds timer_ = 0;
};



} // namespace skyland
