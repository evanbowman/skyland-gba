#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class SandboxLoaderModule : public Module<SandboxLoaderModule> {
public:
    static SystemString module_name()
    {
        return SystemString::module_sandbox;
    }


    static u16 icon()
    {
        return 1176;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    static bool enable_custom_scripts()
    {
        return true;
    }


private:
    void update_parameter(u8 line_num);

    u32 cursor_ = 0;

    bool unveil_ = false;

    std::optional<Text> title_;
    std::optional<Text> help_;

    struct ParameterInfo {
        const char* name_;
        int increment_;
        int lower_limit_;
        int upper_limit_;
    };

    Buffer<Text, 3> settings_text_;
    using ParamBuffer = Buffer<int, decltype(settings_text_)::capacity()>;
    static ParamBuffer parameters_;

    Microseconds long_hold_time_[2] = {0, 0};


    static const ParameterInfo param_info[decltype(parameters_)::capacity()];

    static Factory factory_;
};



} // namespace skyland
