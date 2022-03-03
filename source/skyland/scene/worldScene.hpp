#pragma once


#include "graphics/overlay.hpp"
#include "skyland/coins.hpp"
#include "skyland/gamespeed.hpp"
#include "skyland/power.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class WorldScene : public Scene
{
public:
    WorldScene() : far_camera_(false), birds_drawn_(false)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    void persist_ui();


    void far_camera();


    void near_camera();


    void force_show_coins()
    {
        last_coins_ = 0;
    }


    bool is_far_camera()
    {
        return far_camera_;
    }


    void set_gamespeed(Platform& pfrm, App& app, GameSpeed speed);


protected:
    std::optional<UIMetric> coins_;
    std::optional<UIMetric> power_;

    Microseconds camera_update_timer_ = 0;

    void reset_gamespeed(Platform& pfrm, App& app);

private:
    void set_pause_icon(Platform& pfrm, u16 icon);

    bool persistent_ui_ = false;
    Microseconds coin_hide_timer_ = 0;
    Microseconds power_hide_timer_ = 0;
    Microseconds set_gamespeed_keyheld_timer_ = 0;
    Coins last_coins_ = 0;
    Power last_power_supplied_ = 0;
    Power last_power_used_ = 0;
    bool far_camera_ : 1;
    bool birds_drawn_ : 1;

protected:
    bool power_fraction_opponent_island_ = false;
};



class ActiveWorldScene : public WorldScene
{
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
};



} // namespace skyland
