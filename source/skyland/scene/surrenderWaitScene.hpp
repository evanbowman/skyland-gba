#pragma once

#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland {



class SurrenderWaitScene : public WorldScene {
public:
    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto next = WorldScene::update(pfrm, app, delta)) {
            return next;
        }

        if (timer_ < seconds(1)) {
            timer_ += delta;
            if (timer_ > seconds(1)) {
                app.invoke_script(pfrm, "/scripts/event/surrender.lisp");
            }
        } else {
            if (not app.dialog_buffer()) {
                return scene_pool::alloc<ReadyScene>();
            }
        }

        return null_scene();
    }


private:
    Microseconds timer_ = 0;
};



} // namespace skyland
