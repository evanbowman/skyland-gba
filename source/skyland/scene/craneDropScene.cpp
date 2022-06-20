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


#include "craneDropScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/crane.hpp"



namespace skyland
{



class CraneFadeinScene : public WorldScene
{
public:

    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        // TODO: stuff...

        WorldScene::enter(pfrm, app, prev);
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta;


        constexpr auto fade_duration = milliseconds(1400);
        constexpr auto fade_start = milliseconds(400);

        if (app.game_speed() not_eq GameSpeed::normal) {
            set_gamespeed(pfrm, app, GameSpeed::normal);
        }

        if (timer_ < fade_start) {

            // ...

        } else if (timer_ > fade_duration) {

            WorldScene::update(pfrm, app, delta);

            pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);

            pfrm.screen().schedule_fade(0.f);
            return scene_pool::alloc<ReadyScene>();

        } else {

            WorldScene::update(pfrm, app, delta);


            const auto amount = smoothstep(0.f,
                                           fade_duration - fade_start,
                                           timer_ - fade_start);

            pfrm.speaker().set_music_volume(6 + 14 * amount);

            pfrm.screen().schedule_fade(1.f - amount);
        }

        return null_scene();
    }

private:
    Microseconds timer_ = 0;
};



class FishingMinigameScene : public Scene
{
public:


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        // ...
    }


    ScenePtr<Scene> update(Platform& pfrm,
                           App& app,
                           Microseconds delta) override
    {
        return scene_pool::alloc<CraneFadeinScene>();
    }
};



ScenePtr<Scene>
CraneDropScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    if (timer_ == 0) {
        app.set_coins(pfrm, app.coins() - 2000);
    }

    timer_ += delta;

    if (app.game_speed() not_eq GameSpeed::normal) {
        set_gamespeed(pfrm, app, GameSpeed::normal);
    }

    constexpr auto fade_duration = milliseconds(1400);
    constexpr auto fade_start = milliseconds(400);

    if (timer_ < fade_start) {

        // ...

    } else if (timer_ > fade_duration) {

        if (auto room = app.player_island().get_room(crane_pos_)) {
            if (auto crane = dynamic_cast<Crane*>(room)) {
                crane->retract();
            }
        }

        return scene_pool::alloc<FishingMinigameScene>();

    } else {
        const auto amount = smoothstep(0.f,
                                       fade_duration - fade_start,
                                       timer_ - fade_start);

        pfrm.speaker().set_music_volume(20 - 14 * amount);

        pfrm.screen().schedule_fade(amount);
    }

    return null_scene();
}



} // namespace skyland
