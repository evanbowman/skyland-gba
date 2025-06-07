////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "skyland/characterId.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland
{



class CrewStatsScene : public Scene
{
public:
    CrewStatsScene(CharacterId selected_chr);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    Optional<DeferredScene> next_;


    Float bkg_fade_amount_ = 0.5f;
    Float exit_fade_ = 0.f;
    SystemString title_ = SystemString::crewmember_stats_title;


    std::pair<u8*, u32> icons();


private:
    Vector<CharacterId> chrs_;
    int page_index_ = 0;
    int icon_sel_ = 0;
    bool exit_ = false;

    enum State : u8 {
        regular,
        icon_select,
    } state_ = State::regular;

    void show_page();
};



} // namespace skyland
