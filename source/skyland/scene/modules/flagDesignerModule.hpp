#pragma once


#include "skyland/scene/module.hpp"




namespace skyland {



class FlagDesignerModule : public Module<FlagDesignerModule> {
public:
    static const char* module_name()
    {
        return "Flag Designer";
    }


    static Factory factory_;
};



}
