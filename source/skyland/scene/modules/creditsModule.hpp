#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class CreditsModule : public Module<CreditsModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_credits;
    }


    static u16 icon()
    {
        return 1752;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    static bool stop_sound()
    {
        return false;
    }


private:
    enum class State {
        fade_in,
        fade_out_next,
        fade_out_exit,
        page_swap,
        idle,
    } state_ = State::fade_in;


    void load_page(Platform& pfrm, u32 page);


    Buffer<Text, 12> lines_;
    Microseconds timer_ = 0;

    int page_ = 0;


    static Factory factory_;
};



} // namespace skyland
