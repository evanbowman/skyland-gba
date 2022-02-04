#pragma once


#include "bulkAllocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"
#include "string.hpp"



namespace skyland {



class DlcManagerModule : public Module<DlcManagerModule> {
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    static const char* module_name()
    {
        return "DLC Manager";
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
    void show(Platform& pfrm);


    using PatchName = StringBuffer<30>;

    struct PatchInfo {
        PatchName name_;
        u8 tiles_used_ : 4;
        u8 sprites_used_ : 4;
    };

    struct PatchList {
        Buffer<PatchInfo, 32> list_;
    };

    u32 index_ = 0;

    std::optional<Text> patch_name_;

    std::optional<DynamicMemory<PatchList>> patches_;


    static Factory factory_;
};



} // namespace skyland
