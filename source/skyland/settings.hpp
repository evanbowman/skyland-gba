#pragma once

#include "script/lisp.hpp"



namespace skyland::settings
{



struct Settings
{
    Settings();
    Settings(const Settings&) = delete;

    StringBuffer<96> get(const char* key);

    void set(const char* key, const char* value);

    void save();


    lisp::Protected data_;
};



void load(Settings& output);



}
