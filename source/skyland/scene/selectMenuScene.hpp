#pragma once

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/island.hpp"
#include "skyland/systemString.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SelectMenuScene : public ActiveWorldScene
{
public:
    SelectMenuScene() : opts_(allocate_dynamic<Options>("sel-opts"))
    {
    }


    void enter(Platform& pfrm, App& app, Scene& scene) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


    Island* island(App& app) const;


private:
    struct Options
    {
        Buffer<Text, 10> lines_;
        Buffer<SystemString, 10> strings_;
        Buffer<Function<16, ScenePtr<Scene>(Platform&, App&)>, 10> callbacks_;
        u8 longest_line_;

        Buffer<SystemString, 10> pushed_strings_;
    };

    void redraw_line(Platform& pfrm, int line, bool highlight);

    DynamicMemory<Options> opts_;
    int sel_ = 0;
};



} // namespace skyland
