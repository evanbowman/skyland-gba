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


    ScenePtr<Scene> update(Time delta) override;


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


    int game_mode_ = 0;


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
