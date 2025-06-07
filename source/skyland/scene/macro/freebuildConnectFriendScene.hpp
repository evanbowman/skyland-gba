////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    void enter(Scene& prev) override
    {
        text_.emplace(SYSTR(multi_session_connecting)->c_str(),
                      OverlayCoord{1, 3});
    }


    void exit(Scene& next) override
    {
        text_.reset();
        failure_text_.reset();
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().schedule_fade(0);
    }


    ScenePtr update(Time delta) override
    {
        player().update(delta);

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
                return make_scene<SelectorScene>();
            }
            break;

        case State::sync:
            APP.swap_player<macro::FreebuildTeam>();
            if (auto s = macrocosm().sector().cast_freebuild_sector()) {
                s->reset();
            }
            PLATFORM.speaker().stream_music(APP.environment().music()->c_str(),
                                            0);
            return make_scene<SelectorScene>();
        }

        return null_scene();
    }


private:
    Optional<Text> text_;
    Optional<TextView> failure_text_;

    enum class State { show, connect, sync, failure } state_ = State::show;
};



} // namespace skyland::macro
