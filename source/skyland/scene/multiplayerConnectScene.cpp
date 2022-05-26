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


#include "multiplayerConnectScene.hpp"
#include "fullscreenDialogScene.hpp"
#include "globals.hpp"
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



void MultiplayerConnectScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().schedule_fade(1.f);
    text_.emplace(pfrm, OverlayCoord{1, 1});
    text_->assign(SYSTR(multi_session_connecting)->c_str());

    const char* str = pfrm.load_file_contents("scripts", "multi_init.lisp");
    if (str) {
        lisp::BasicCharSequence seq(str);
        lisp::dostring(seq, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.data_.c_str());
        });
    } else {
        Platform::fatal("missing multi init script!");
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    cursor_loc.x = 0;
    cursor_loc.y = 14;

    pfrm.delta_clock().reset();

    show_island_exterior(pfrm, app, &app.player_island());
    show_island_exterior(pfrm, app, app.opponent_island());

    std::get<SkylandGlobalData>(globals()).multiplayer_prep_timer_ = 0;
    std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_ = 120;
}



void MultiplayerConnectScene::exit(Platform&, App&, Scene& next)
{
    text_.reset();
}



ScenePtr<Scene>
MultiplayerConnectScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not ready_) {
        ready_ = true;
        return null_scene();
    }


    // TEST
    // return scene_pool::alloc<MultiplayerSettingsScene>();


    pfrm.network_peer().listen();

    if (not pfrm.network_peer().is_connected()) {

        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_ = 0;

        auto future_scene = [] {
            return scene_pool::alloc<TitleScreenScene>();
        };
        auto buffer = allocate_dynamic<DialogString>("dialog-string");
        *buffer = SYSTR(multi_connection_failure)->c_str();
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                        future_scene);
    } else {
        network::packet::ProgramVersion packet;
        packet.major_.set(PROGRAM_MAJOR_VERSION);
        packet.minor_ = PROGRAM_MINOR_VERSION;
        packet.subminor_ = PROGRAM_SUBMINOR_VERSION;
        packet.revision_ = PROGRAM_VERSION_REVISION;

        network::transmit(pfrm, packet);

        app.swap_opponent<MultiplayerPeer>();
        rng::critical_state = 42;

        app.game_mode() = App::GameMode::multiplayer;

        return scene_pool::alloc<MultiplayerSettingsScene>();
    }

    return null_scene();
}



} // namespace skyland
