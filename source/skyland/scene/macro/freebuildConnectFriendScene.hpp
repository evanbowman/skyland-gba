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


    ScenePtr<Scene> update(Time delta) override
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
                return scene_pool::alloc<SelectorScene>();
            }
            break;

        case State::sync:
            APP.swap_player<macro::FreebuildTeam>();
            if (auto s = macrocosm().sector().cast_freebuild_sector()) {
                s->reset();
            }
            PLATFORM.speaker().play_music(APP.environment().music(), 0);
            return scene_pool::alloc<SelectorScene>();
        }

        return null_scene();
    }


private:
    Optional<Text> text_;
    Optional<TextView> failure_text_;

    enum class State { show, connect, sync, failure } state_ = State::show;

    Time timer_ = 0;
};



} // namespace skyland::macro
