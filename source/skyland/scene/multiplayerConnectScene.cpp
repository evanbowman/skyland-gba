#include "multiplayerConnectScene.hpp"



namespace skyland {



void MultiplayerConnectScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.screen().fade(1.f);
    text_.emplace(pfrm, "session connecting...", OverlayCoord{1, 1});
}



void MultiplayerConnectScene::exit(Platform&, App&, Scene& next)
{
    text_.reset();
}



ScenePtr<Scene> MultiplayerConnectScene::update(Platform& pfrm,
                                                App&,
                                                Microseconds delta)
{
    if (not ready_) {
        ready_ = true;
        return null_scene();
    }
    pfrm.network_peer().listen();

    if (not pfrm.network_peer().is_connected()) {
        pfrm.fatal("failed to connect to multiplayer peer");
    } else {

    }

    return null_scene();
}



}
