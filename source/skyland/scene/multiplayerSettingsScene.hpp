#pragma once

#include "graphics/overlay.hpp"
#include "skyland/network.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class MultiplayerSettingsScene : public Scene, public network::Listener
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


    void
    receive(Platform& pfrm,
            App& app,
            const network::packet::GameMatchParameterUpdate& packet) override;


    void
    receive(Platform& pfrm,
            App& app,
            const network::packet::GameMatchSettingsCursor& packet) override;


    void receive(Platform& pfrm,
                 App& app,
                 const network::packet::GameMatchReady& packet) override;



    void sync_parameters(Platform& pfrm);


private:
    void update_parameter(u8 line_num);


    void setup_vs_game(Platform& pfrm, App& app);
    void setup_coop_game(Platform& pfrm, App& app);


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

    Buffer<Text, 5> settings_text_;
    using ParamBuffer = Buffer<int, decltype(settings_text_)::capacity()>;
    static ParamBuffer vs_parameters_;

    bool opponent_ready_ = false;

    struct ParameterInfo
    {
        const char* name_;
        int increment_;
        int lower_limit_;
        int upper_limit_;
    };

    Microseconds key_held_timers_[4] = {0, 0, 0, 0};

    static const ParameterInfo param_info[decltype(vs_parameters_)::capacity()];
};



} // namespace skyland
