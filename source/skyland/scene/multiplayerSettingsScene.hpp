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
#include "number/random.hpp"
#include "skyland/network.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class MultiplayerSettingsScene : public Scene, public network::Listener
{
public:
    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    static Time timeout_frequency();
    static Time timeout_duration();


    ScenePtr update(Time delta) override;


    void display() override;


    void
    receive(const network::packet::GameMatchParameterUpdate& packet) override;


    void
    receive(const network::packet::GameMatchSettingsCursor& packet) override;


    void receive(const network::packet::GameMatchReady& packet) override;



    void sync_parameters();


private:
    void update_parameter(u8 line_num);


    void setup_vs_game();
    void setup_co_op_game();


    Optional<rng::LinearGenerator> co_op_rng_;


    Time parameter_sync_timer_ = seconds(1);


    enum class State {
        edit_settings,
        ready,
    } state_ = State::edit_settings;


    Optional<Text> title_;

    Optional<Text> msg_;

    u8 player_cursor_ = 0;
    u8 opponent_cursor_ = 0;

    Buffer<Text, 7> settings_text_;
    using ParamBuffer = Buffer<int, decltype(settings_text_)::capacity()>;
    static ParamBuffer vs_parameters_;

    bool opponent_ready_ = false;

    struct ParameterInfo
    {
        SystemString name_;
        int increment_;
        int lower_limit_;
        int upper_limit_;
    };

    Time key_held_timers_[4] = {0, 0, 0, 0};

    static const ParameterInfo param_info[decltype(vs_parameters_)::capacity()];
};



} // namespace skyland
