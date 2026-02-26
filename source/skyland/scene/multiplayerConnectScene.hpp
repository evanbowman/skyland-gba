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


#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class MultiplayerConnectScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    static ScenePtr setup();


private:
    enum class HostPeerState {
        select = -1,
        host,
        peer,
        peer_select_host,
    };
    HostPeerState internet_host_peer_state_ = HostPeerState::select;
    Optional<Text> text_;

    struct HostInfo
    {
        StringBuffer<32> ip_;
        int port_;
        StringBuffer<32> username_;
    };
    using HostInfoList = Buffer<HostInfo, 8>;
    Optional<DynamicMemory<HostInfoList>> hosts_;

    bool ready_ = false;
    u8 internet_choice_sel_ = 0;
    u8 host_choice_sel_ = 0;
};



} // namespace skyland
