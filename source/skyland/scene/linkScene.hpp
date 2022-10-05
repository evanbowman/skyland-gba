#pragma once

#include "graphics/overlay.hpp"
#include "multiplayerConnectScene.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



class LinkScene : public Scene
{
public:
    std::optional<Text> t_;

    void enter(Platform& pfrm, App&, Scene&) override
    {
        pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);

        auto str = SYSTR(mt_hint);
        u8 mr = centered_text_margins(pfrm, utf8::len(str->c_str()));
        t_.emplace(pfrm, OverlayCoord{mr, 8});
        FontColors c{custom_color(0x163061), ColorConstant::silver_white};
        t_->assign(str->c_str(), c);
    }

    void exit(Platform&, App&, Scene&) override
    {
        t_.reset();
    }

    ScenePtr<Scene> update(Platform& pfrm, App&, Microseconds) override
    {
        if (key_down<Key::start>(pfrm)) {
            return scene_pool::alloc<MultiplayerConnectScene>();
        }
        if (key_down<Key::action_2>(pfrm)) {
            return scene_pool::alloc<TitleScreenScene>();
        }
        return null_scene();
    }
};



}
