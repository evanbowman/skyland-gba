#include "multiplayerConnectScene.hpp"



namespace skyland {



void MultiplayerConnectScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.screen().fade(1.f);
    text_.emplace(pfrm, "multiplayer session connecting", OverlayCoord{1, 1});
}



void MultiplayerConnectScene::exit(Platform&, App&, Scene& next)
{
    text_.reset();
}



ScenePtr<Scene> MultiplayerConnectScene::update(Platform& pfrm,
                                                App&,
                                                Microseconds delta)
{
    pfrm.network_peer().listen();

    if (not pfrm.network_peer().is_connected()) {
        // ...
    } else {

    }

    return null_scene();
}



}
