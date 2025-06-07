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
    auto buffer = allocate_dynamic<DialogString>("dialog-string");

    if (PLATFORM.device_name() == "GameboyAdvance" and
        PLATFORM.model_name() == "NDS") {

        auto future_scene = [] { return make_scene<TitleScreenScene>(); };

        *buffer = SYSTR(error_link_nds)->c_str();
        return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                 future_scene);
    } else if (PLATFORM.device_name() == "GameboyAdvance") {

        auto future_scene = [] { return make_scene<LinkScene>(); };

        if (not state_bit_load(StateBit::successful_multiplayer_connect)) {
            *buffer = SYSTR(link_gba_setup)->c_str();


            return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                     future_scene);
        } else {
            return future_scene();
        }

    } else {
        return make_scene<TitleScreenScene>();
    }
}



ScenePtr MultiplayerConnectScene::update(Time delta)
{
    if (not ready_) {
        ready_ = true;
        return null_scene();
    }


    auto future_scene = [] { return make_scene<TitleScreenScene>(); };


    PLATFORM.network_peer().listen();

    if (not PLATFORM.network_peer().is_connected()) {

        globals().multiplayer_prep_seconds_ = 0;

        auto buffer = allocate_dynamic<DialogString>("dialog-string");
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
