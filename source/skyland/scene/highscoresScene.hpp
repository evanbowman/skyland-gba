#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class HighscoresScene : public Module<HighscoresScene> {
public:

    HighscoresScene();


    HighscoresScene(bool show_current_score);

    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;

    ScenePtr<Scene> update(Platform&, App&, Microseconds) override;


    static const char* module_name()
    {
        return "Highscores";
    }


    static u16 icon()
    {
        return 1192;
    }


    static bool run_scripts()
    {
        return false;
    }


private:
    Buffer<Text, 8> lines_;
    bool show_current_score_;
    bool disable_writeback_;

    static Factory factory_;
};



} // namespace skyland
