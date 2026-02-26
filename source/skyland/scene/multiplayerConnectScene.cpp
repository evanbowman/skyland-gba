////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "multiplayerConnectScene.hpp"
#include "fullscreenDialogScene.hpp"
#include "globals.hpp"
#include "linkScene.hpp"
#include "multiplayerSettingsScene.hpp"
#include "script/lisp.hpp"
#include "skyland/player/opponent/multiplayerPeer.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "version.hpp"



namespace skyland
{



void set_island_positions(Island& left_island, Island& right_island);



void MultiplayerConnectScene::enter(Scene& prev)
{
    PLATFORM.screen().schedule_fade(1.f);
    text_.emplace(OverlayCoord{1, 1});
    text_->assign(SYSTR(multi_session_connecting)->c_str());

    const char* str = PLATFORM.load_file_contents("scripts", "multi_init.lisp");
    if (str) {
        lisp::BasicCharSequence seq(str);
        lisp::dostring(seq, [](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            PLATFORM.fatal(p.data_.c_str());
        });
    } else {
        Platform::fatal("missing multi init script!");
    }

    auto& cursor_loc = globals().near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    PLATFORM.delta_clock().reset();

    show_island_exterior(&APP.player_island());
    show_island_exterior(APP.opponent_island());

    auto& g = globals();
    g.multiplayer_prep_timer_ = 0;
    g.multiplayer_prep_seconds_ = 120;
    g.multiplayer_pauses_remaining_ = 3;
    g.multiplayer_pause_owner_ = false;
}



void MultiplayerConnectScene::exit(Scene& next)
{
    text_.reset();
}



ScenePtr MultiplayerConnectScene::setup()
{
    auto buffer = allocate<DialogString>("dialog-string");

    if (PLATFORM.device_name() == "GameboyAdvance" and
        PLATFORM.model_name() == "NDS") {

        auto future_scene = [] { return make_scene<TitleScreenScene>(); };

        *buffer = SYSTR(error_link_nds)->c_str();
        return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                 future_scene);
    } else {

        auto future_scene = [] { return make_scene<LinkScene>(); };

        if (not state_bit_load(StateBit::successful_multiplayer_connect)) {
            *buffer = SYSTR(link_gba_setup)->c_str();

            if (PLATFORM.device_name() == "PC") {
                return future_scene();
            }
            return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                     future_scene);
        } else {
            return future_scene();
        }
    }
}



ScenePtr MultiplayerConnectScene::update(Time delta)
{
    if (not ready_) {
        ready_ = true;
        return null_scene();
    }

    auto future_scene = [] { return make_scene<TitleScreenScene>(); };

    switch (PLATFORM.network_peer().interface()) {
    case Platform::NetworkPeer::Interface::internet:
        for (int x = 0; x < 30; ++x) {
            for (int y = 3; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
        switch (internet_host_peer_state_) {
        case HostPeerState::select:
            Text::print(SYS_CSTR(multi_opt_host), {3, 4});
            Text::print(SYS_CSTR(multi_opt_client), {3, 6});
            PLATFORM.set_tile(
                Layer::overlay, 1, 4 + internet_choice_sel_ * 2, 475);
            if (button_down<Button::down>() or button_down<Button::up>()) {
                internet_choice_sel_ = not internet_choice_sel_;
            }

            if (button_down<Button::action_1>() or
                button_down<Button::action_2>()) {
                internet_host_peer_state_ = (HostPeerState)internet_choice_sel_;
            }
            return null_scene();
            break;

        case HostPeerState::host: {
            Text::print(SYS_CSTR(multi_broadcast_avail), {1, 4});
            Text::print(SYS_CSTR(multi_your_addr), {1, 6});
            StringBuffer<28> username;
            PLATFORM_EXTENSION(get_username, username);
            Text::print(username.c_str(), {2, 8});
            PLATFORM.network_peer().host(seconds(120));
            break;
        }

        case HostPeerState::peer: {
            hosts_ = allocate<HostInfoList>("host-info");
            Text::print(SYS_CSTR(multi_listen_avail), {1, 4});
            PLATFORM.network_peer().listen(
                seconds(2),
                [this](const char* host, int port, const char* username) {
                    (*hosts_)->push_back({host, port, username});
                });
            if (not(*hosts_)->empty()) {
                internet_host_peer_state_ = HostPeerState::peer_select_host;
            }
            return null_scene();
            break;
        }

        case HostPeerState::peer_select_host:
            Text::print(SYS_CSTR(multi_found_hosts), {1, 4});
            for (u32 i = 0; i < (*hosts_)->size(); ++i) {
                auto& host = (**hosts_)[i];
                Text::print(host.username_.c_str(), {3, (u8)(6 + i * 2)});
            }
            PLATFORM.set_tile(Layer::overlay, 1, 6 + host_choice_sel_ * 2, 475);
            if (button_down<Button::action_2>()) {
                break;
            }
            if (button_down<Button::action_1>()) {
                auto host = (**hosts_)[host_choice_sel_];
                PLATFORM.network_peer().connect(host.ip_.c_str(), host.port_);
                break;
            }
            if (button_down<Button::down>() and
                host_choice_sel_ < (*hosts_)->size() - 1) {
                ++host_choice_sel_;
            }
            if (button_down<Button::up>() and host_choice_sel_ > 0) {
                --host_choice_sel_;
            }
            return null_scene();
            break;
        }
        break;

    case Platform::NetworkPeer::Interface::serial_cable: {
        // For the gba, the link cable determines who is the host and who is the
        // peer, so there's no need to check anything.
        PLATFORM.network_peer().listen(seconds(20));
        break;
    }
    }

    if (not PLATFORM.network_peer().is_connected()) {

        globals().multiplayer_prep_seconds_ = 0;

        auto buffer = allocate<DialogString>("dialog-string");
        *buffer = SYSTR(multi_connection_failure)->c_str();
        return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                 future_scene);
    } else {

        state_bit_store(StateBit::successful_multiplayer_connect, true);

        network::packet::ProgramVersion packet;
        packet.major_.set(PROGRAM_MAJOR_VERSION);
        packet.minor_ = PROGRAM_MINOR_VERSION;
        packet.subminor_ = PROGRAM_SUBMINOR_VERSION;
        packet.revision_ = PROGRAM_VERSION_REVISION;

        network::transmit(packet);

        APP.swap_opponent<MultiplayerPeer>();
        rng::critical_state = 42;

        APP.game_mode() = App::GameMode::multiplayer;

        return make_scene<MultiplayerSettingsScene>();
    }

    return null_scene();
}



} // namespace skyland
