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


    static Microseconds timeout_frequency();
    static Microseconds timeout_duration();


    ScenePtr<Scene> update(Microseconds delta) override;


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


    std::optional<rng::LinearGenerator> co_op_rng_;


    Microseconds parameter_sync_timer_ = seconds(1);


    int game_mode_ = 0;


    enum class State {
        edit_settings,
        ready,
    } state_ = State::edit_settings;


    std::optional<Text> title_;

    std::optional<Text> msg_;

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

    Microseconds key_held_timers_[4] = {0, 0, 0, 0};

    static const ParameterInfo param_info[decltype(vs_parameters_)::capacity()];
};



} // namespace skyland
