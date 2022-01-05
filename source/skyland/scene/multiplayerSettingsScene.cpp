#include "multiplayerSettingsScene.hpp"
#include "localization.hpp"
#include "skyland/skyland.hpp"
#include "fadeInScene.hpp"



namespace skyland {



MultiplayerSettingsScene::ParamBuffer MultiplayerSettingsScene::parameters_;



const MultiplayerSettingsScene::ParameterInfo
    MultiplayerSettingsScene::param_info[decltype(parameters_)::capacity()] = {
        {"prep seconds", 5, 20, 10000},
        {"coins", 100, 1000, 10000000},
};



void MultiplayerSettingsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    const char* title = "Multiplayer Settings";

    pfrm.load_overlay_texture("overlay_challenges");

    title_.emplace(
        pfrm,
        title,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(title)), 1});


    const char* msg = "Ready? Press start!";
    msg_.emplace(pfrm,
                 msg,
                 OverlayCoord{(u8)centered_text_margins(pfrm, str_len(msg)), 18});


    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(pfrm, OverlayCoord{2, u8(4 + i * 2)});
    }

    if (not parameters_.full()) {
        for (u32 i = 0; i < parameters_.capacity(); ++i) {
            parameters_.push_back(0);
        }

        // Defaults
        parameters_[0] = 120;
        parameters_[1] = 17500;
    }


    for (u32 i = 0; i < parameters_.capacity(); ++i) {
        update_parameter(i);
    }

    pfrm.screen().fade(0.6f, ColorConstant::rich_black, {}, false, false);

    pfrm.system_call("v-parallax", (void*)false);
}



void MultiplayerSettingsScene::update_parameter(u8 line_num)
{
    if (line_num >= parameters_.capacity()) {
        return;
    }

    StringBuffer<28> temp;
    temp += param_info[line_num].name_;
    temp += " ";

    const auto int_text_len = integer_text_length(parameters_[line_num]);

    for (u32 i = temp.length(); i < 28 - int_text_len - 2; ++i) {
        if (i % 2 == 0) {
            temp.push_back('.');
        } else {
            temp.push_back(' ');
        }
    }

    temp += to_string<10>(parameters_[line_num]);

    settings_text_[line_num].assign(temp.c_str());
}



void MultiplayerSettingsScene::exit(Platform& pfrm, App& app, Scene& next)
{
    title_.reset();
    settings_text_.clear();

    pfrm.load_overlay_texture("overlay");

    pfrm.system_call("v-parallax", (void*)true);

    std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_ =
        parameters_[0];

    app.coins() = parameters_[1];
}



void MultiplayerSettingsScene::display(Platform& pfrm, App& app)
{
    Scene::display(pfrm, app);

    if (state_ not_eq State::edit_settings) {
        return;
    }

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_position({2, 31.f + player_cursor_ * 16});

    pfrm.screen().draw(spr);
}



void MultiplayerSettingsScene::receive(
    Platform& pfrm,
    App& app,
    const network::packet::GameMatchParameterUpdate& packet)
{
    parameters_[packet.parameter_id_] = packet.value_.get();
    update_parameter(packet.parameter_id_);
}



void MultiplayerSettingsScene::receive(
    Platform& pfrm,
    App& app,
    const network::packet::GameMatchSettingsCursor& packet)
{
    opponent_cursor_ = packet.cursor_line_;
}



void MultiplayerSettingsScene::receive(
    Platform& pfrm,
    App& app,
    const network::packet::GameMatchReady& packet)
{
    opponent_ready_ = true;
}



void MultiplayerSettingsScene::sync_parameters(Platform& pfrm)
{
    for (u32 i = 0; i < parameters_.size(); ++i) {
        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = i;
        p.value_.set(parameters_[i]);
        network::transmit(pfrm, p);
    }
}



ScenePtr<Scene>
MultiplayerSettingsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    if (pfrm.network_peer().is_connected()) {
        network::poll_messages(pfrm, app, *this);
    }

    if (app.player().key_pressed(pfrm, Key::up)) {
        key_held_timers_[0] += delta;
    } else {
        key_held_timers_[0] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::down)) {
        key_held_timers_[1] += delta;
    } else {
        key_held_timers_[1] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::left)) {
        key_held_timers_[2] += delta;
    } else {
        key_held_timers_[2] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::right)) {
        key_held_timers_[3] += delta;
    } else {
        key_held_timers_[3] = 0;
    }

    if (state_ == State::ready) {
        if (opponent_ready_) {
            return scene_pool::alloc<FadeInScene>();
        }
        return null_scene();
    }

    if (app.player().key_down(pfrm, Key::start)) {
        if (not pfrm.network_peer().is_host() and parameter_sync_timer_ > 0) {
            return null_scene();
        }
        state_ = State::ready;

        sync_parameters(pfrm);

        network::packet::GameMatchReady r;
        network::transmit(pfrm, r);

        const char* msg = "Waiting for other player...";
        msg_.emplace(pfrm,
                     msg,
                     OverlayCoord{(u8)centered_text_margins(pfrm, str_len(msg)), 18});


        return null_scene();
    }

    if (pfrm.network_peer().is_host()) {
        parameter_sync_timer_ -= delta;
        if (parameter_sync_timer_ < 0) {
            parameter_sync_timer_ = seconds(2);

            // Every so often, the host device broadcasts its own parameters, in
            // case anything got out of sync.
            sync_parameters(pfrm);
        }
    } else {
        if (parameter_sync_timer_ > 0) {
            // We want to get at least one sync from the host device, add a
            // smaller delta.
            parameter_sync_timer_ -= delta / 2;
        }
    }


    if (app.player().key_down(pfrm, Key::right) or
        key_held_timers_[3] > milliseconds(500)) {
        if (parameters_[player_cursor_] <
            param_info[player_cursor_].upper_limit_) {
            parameters_[player_cursor_] +=
                param_info[player_cursor_].increment_;
        }
        update_parameter(player_cursor_);
        key_held_timers_[3] -= milliseconds(80);

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = player_cursor_;
        p.value_.set(parameters_[player_cursor_]);
        network::transmit(pfrm, p);

    } else if (app.player().key_down(pfrm, Key::left) or
               key_held_timers_[2] > milliseconds(500)) {
        parameters_[player_cursor_] -= param_info[player_cursor_].increment_;
        if (parameters_[player_cursor_] <
            param_info[player_cursor_].lower_limit_) {
            parameters_[player_cursor_] =
                param_info[player_cursor_].lower_limit_;
        }
        update_parameter(player_cursor_);
        key_held_timers_[2] -= milliseconds(80);

        network::packet::GameMatchParameterUpdate p;
        p.parameter_id_ = player_cursor_;
        p.value_.set(parameters_[player_cursor_]);
        network::transmit(pfrm, p);

    } else if (app.player().key_down(pfrm, Key::down) and
               player_cursor_ < parameters_.size() - 1) {
        ++player_cursor_;

        network::packet::GameMatchSettingsCursor c;
        c.cursor_line_ = player_cursor_;
        network::transmit(pfrm, c);

    } else if (app.player().key_down(pfrm, Key::up) and player_cursor_ > 0) {
        --player_cursor_;

        network::packet::GameMatchSettingsCursor c;
        c.cursor_line_ = player_cursor_;
        network::transmit(pfrm, c);
    }

    return null_scene();
}



} // namespace skyland
