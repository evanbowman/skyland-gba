#pragma once

#include "number/numeric.hpp"
#include <memory>
#include "function.hpp"


class Platform;


namespace skyland {


class App;
class Scene;


template <typename T> using ScenePtr = std::unique_ptr<T, void (*)(Scene*)>;


ScenePtr<Scene> null_scene();


class Scene {
public:
    virtual ~Scene(){};


    virtual ScenePtr<Scene> update(Platform&, App&, Microseconds delta)
    {
        return null_scene();
    }


    virtual void display(Platform&, App&)
    {
    }


    virtual void enter(Platform&, App&, Scene& prev_scene){};


    virtual void exit(Platform&, App&, Scene& next_scene){};
};


ScenePtr<Scene> initial_scene();


using DeferredScene = Function<16, ScenePtr<Scene>()>;



} // namespace skyland
