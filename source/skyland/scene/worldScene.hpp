////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
          disable_ui_(false), disable_gamespeed_icon_(false)
    {
    }


    ScenePtr<Scene> update(Microseconds delta) override;


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


protected:
    std::optional<UIMetric> coins_;
    std::optional<UIMetric> power_;

    Microseconds camera_update_timer_ = 0;

    void reset_gamespeed();

    void set_pause_icon(u16 icon);

private:
    bool persistent_ui_ = false;
    Microseconds coin_hide_timer_ = 0;
    Microseconds power_hide_timer_ = 0;
    Microseconds set_gamespeed_keyheld_timer_ = 0;
    Coins last_coins_ = 0;
    Power last_power_supplied_ = 0;
    Power last_power_used_ = 0;
    bool far_camera_ : 1;
    bool birds_drawn_ : 1;
    bool noreturn_ : 1;
    bool disable_ui_ : 1;
    bool disable_gamespeed_icon_ : 1;

    void multiplayer_vs_timeout_step(Microseconds delta);

protected:
    bool power_fraction_opponent_island_ = false;
};



class ActiveWorldScene : public WorldScene
{
public:
    ScenePtr<Scene> update(Microseconds delta) override;

private:
    ScenePtr<Scene> on_player_island_destroyed();
    ScenePtr<Scene> try_surrender();
};



} // namespace skyland
