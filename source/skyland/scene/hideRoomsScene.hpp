#pragma once

#include "bulkAllocator.hpp"
#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class HideRoomsScene : public Scene
{
public:
    HideRoomsScene(DeferredScene next) : next_(next)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& prev) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


private:
    void repaint(Platform& pfrm, App& app);


    struct Data
    {
        Buffer<u16, 50> room_classes_;
    };


    std::optional<DynamicMemory<Data>> data_;


    Buffer<Text, 3> names_;
    Buffer<Text, 3> hidden_;
    int index_ = 0;


    DeferredScene next_;
    bool changed_ = false;
};



} // namespace skyland
