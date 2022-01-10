#include "sandboxResetScene.hpp"
#include "modules/sandboxLoaderModule.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland {



static const auto sel_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};



ScenePtr<Scene>
SandboxResetScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::up)) {
        selection_ = true;
        yes_text_->assign("yes", sel_colors);
        no_text_->assign("exit");
    }

    if (app.player().key_down(pfrm, Key::down)) {
        selection_ = false;
        yes_text_->assign("yes");
        no_text_->assign("exit", sel_colors);
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (selection_) {
            return scene_pool::alloc<SandboxLoaderModule>();
        } else {
            return scene_pool::alloc<TitleScreenScene>(3);
        }
    }

    return null_scene();
}


void SandboxResetScene::enter(Platform& pfrm, App&, Scene& prev)
{
    msg_.emplace(pfrm, "Reset sandbox?", OverlayCoord{1, 1});
    yes_text_.emplace(pfrm, OverlayCoord{2, 3});
    no_text_.emplace(pfrm, "exit", OverlayCoord{2, 5});

    yes_text_->assign("yes", sel_colors);
}



void SandboxResetScene::exit(Platform&, App&, Scene& next)
{
    msg_.reset();
    yes_text_.reset();
    no_text_.reset();
}



} // namespace skyland
