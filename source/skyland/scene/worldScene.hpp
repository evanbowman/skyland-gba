////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "graphics/overlay.hpp"
#include "skyland/coins.hpp"
#include "skyland/gamespeed.hpp"
#include "skyland/power.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class WorldScene : public Scene
{
public:
    WorldScene()
        : far_camera_(false), birds_drawn_(false), noreturn_(false),
          disable_ui_(false), disable_gamespeed_icon_(false),
          force_show_power_usage_(false)
    {
    }


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    void persist_ui();
    void unpersist_ui();


    void far_camera();


    void near_camera();


    void force_show_coins()
    {
        last_coins_ = 0;
    }


    bool is_far_camera() const
    {
        return far_camera_;
    }


    void set_gamespeed(GameSpeed speed);


    virtual bool camera_update_check_key();


    virtual bool hide_chr_icon() const;


    // Do not return any new scenes. For derived scenes that want to run the
    // world logic, but not transition through any state changes.
    void notransitions()
    {
        noreturn_ = true;
    }


    WorldScene* cast_world_scene() override
    {
        return this;
    }


    void disable_ui()
    {
        disable_ui_ = true;
    }


    void disable_gamespeed_icon()
    {
        disable_gamespeed_icon_ = true;
    }


    ScenePtr<Scene> make_dialog();


    void force_show_power_usage()
    {
        force_show_power_usage_ = true;
    }


protected:
    Optional<UIMetric> coins_;
    Optional<UIMetric> power_;

    Time camera_update_timer_ = 0;

    void reset_gamespeed();

    void set_pause_icon(u16 icon);

private:
    bool persistent_ui_ = false;
    Time coin_hide_timer_ = 0;
    Time power_hide_timer_ = 0;
    Time set_gamespeed_keyheld_timer_ = 0;
    Coins last_coins_ = 0;
    Power last_power_supplied_ = 0;
    Power last_power_used_ = 0;
    bool far_camera_ : 1;
    bool birds_drawn_ : 1;
    bool noreturn_ : 1;
    bool disable_ui_ : 1;
    bool disable_gamespeed_icon_ : 1;
    bool force_show_power_usage_ : 1;

    void multiplayer_vs_timeout_step(Time delta);

protected:
    bool power_fraction_opponent_island_ = false;
};



class ActiveWorldScene : public WorldScene
{
public:
    ScenePtr<Scene> update(Time delta) override;

private:
    ScenePtr<Scene> on_player_island_destroyed();
    ScenePtr<Scene> try_surrender();
};



} // namespace skyland
