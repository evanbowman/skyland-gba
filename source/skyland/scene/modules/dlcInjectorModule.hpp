#pragma once


#include "skyland/scene/module.hpp"




namespace skyland {



class DlcInjectorModule : public Module<DlcInjectorModule> {
public:
    static const char* module_name()
    {
        return "DLC Loader";
    }


    static Factory factory_;
};



}
