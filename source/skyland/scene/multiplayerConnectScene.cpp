////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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



ScenePtr<Scene> MultiplayerConnectScene::setup()
{
    auto buffer = allocate_dynamic<DialogString>("dialog-string");

    if (PLATFORM.device_name() == "GameboyAdvance" and
        PLATFORM.model_name() == "NDS") {

        auto future_scene = [] {
            return scene_pool::alloc<TitleScreenScene>();
        };

        *buffer = SYSTR(error_link_nds)->c_str();
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                        future_scene);
    } else if (PLATFORM.device_name() == "GameboyAdvance") {

        auto future_scene = [] { return scene_pool::alloc<LinkScene>(); };

        if (not state_bit_load(StateBit::successful_multiplayer_connect)) {
            *buffer = SYSTR(link_gba_setup)->c_str();


            return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                            future_scene);
        } else {
            return future_scene();
        }

    } else {
        return scene_pool::alloc<TitleScreenScene>();
    }
}



ScenePtr<Scene> MultiplayerConnectScene::update(Microseconds delta)
{
    if (not ready_) {
        ready_ = true;
        return null_scene();
    }


    auto future_scene = [] { return scene_pool::alloc<TitleScreenScene>(); };


    PLATFORM.network_peer().listen();

    if (not PLATFORM.network_peer().is_connected()) {

        globals().multiplayer_prep_seconds_ = 0;

        auto buffer = allocate_dynamic<DialogString>("dialog-string");
        *buffer = SYSTR(multi_connection_failure)->c_str();
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
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

        return scene_pool::alloc<MultiplayerSettingsScene>();
    }

    return null_scene();
}



} // namespace skyland
