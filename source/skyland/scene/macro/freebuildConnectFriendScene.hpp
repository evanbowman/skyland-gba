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
#include "selectorScene.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/macrocosmFreebuildSector.hpp"
#include "skyland/player/macroFreebuildTeam.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland::macro
{



class FreebuildConnectFriendScene : public Scene
{
public:
    void enter(App&, Scene& prev) override
    {
        text_.emplace(SYSTR(multi_session_connecting)->c_str(),
                      OverlayCoord{1, 3});
    }


    void exit(App&, Scene& next) override
    {
        text_.reset();
        failure_text_.reset();
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().schedule_fade(0);
    }


    ScenePtr<Scene> update(App& app, Microseconds delta) override
    {
        player(app).update(app, delta);

        switch (state_) {
        case State::show: {
            state_ = State::connect;
            break;
        }

        case State::connect: {
            PLATFORM.network_peer().listen();

            if (not PLATFORM.network_peer().is_connected()) {
                state_ = State::failure;
                failure_text_.emplace();
                failure_text_->assign(SYSTR(multi_connection_failure)->c_str(),
                                      {1, 5},
                                      {u8(calc_screen_tiles().x - 2), 10});
                break;
            }

            state_ = State::sync;
            break;
        }

        case State::failure:
            if (PLATFORM.keyboard().down_transition(Key::action_1) or
                PLATFORM.keyboard().down_transition(Key::action_2)) {
                return scene_pool::alloc<SelectorScene>();
            }
            break;

        case State::sync:
            app.swap_player<macro::FreebuildTeam>();
            if (auto s = macrocosm(app).sector().cast_freebuild_sector()) {
                s->reset();
            }
            PLATFORM.speaker().play_music(app.environment().music(), 0);
            return scene_pool::alloc<SelectorScene>();
        }

        return null_scene();
    }


private:
    std::optional<Text> text_;
    std::optional<TextView> failure_text_;

    enum class State { show, connect, sync, failure } state_ = State::show;

    Microseconds timer_ = 0;
};



} // namespace skyland::macro
