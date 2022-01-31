#pragma once

#include "skyland/achievement.hpp"
#include "worldScene.hpp"



namespace skyland {



class AchievementNotificationScene : public WorldScene {
public:
    AchievementNotificationScene(achievements::Achievement achievement)
        : achievement_(achievement)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    enum class State {
        fade_in,
        animate_box_sweep,
        wait,
        fade_out,
    } state_ = State::fade_in;


    achievements::Achievement achievement_;

    std::optional<Text> achievement_text_;
    std::optional<Text> achievement_name_;
    std::optional<Text> item_name_;
    std::optional<Text> item_details_;

    std::optional<Text> unlocked_text_;

    Microseconds timer_ = 0;
};



} // namespace skyland
