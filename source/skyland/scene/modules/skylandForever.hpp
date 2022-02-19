#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland {



class SkylandForever : public Module<SkylandForever> {
public:
    static SystemString module_name()
    {
        return SystemString::module_skyland_forever;
    }


    static u16 icon()
    {
        return 1736;
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

    struct ParameterInfo {
        const char* name_;
        int increment_;
        int lower_limit_;
        int upper_limit_;
    };

    Buffer<Text, 1> settings_text_;
    using ParamBuffer = Buffer<int, decltype(settings_text_)::capacity()>;
    ParamBuffer parameters_;

    static const ParameterInfo param_info[decltype(parameters_)::capacity()];
    bool unveil_ = false;
    u8 cursor_ = 0;

    std::optional<Text> title_;
    std::optional<Text> help_;

    std::optional<TextView> msg_;

    static Factory factory_;
};



} // namespace skyland
