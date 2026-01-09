#pragma once

#include "script/lisp.hpp"



namespace skyland::settings
{



struct Settings
{
    Settings();
    Settings(const Settings&) = delete;

    // NOTE: for backwards compatibility, you cannot assume that a setting will
    // be present in the settings file. Make sure that you handle an empty
    // string result.
    StringBuffer<96> get(const char* key);

    void set(const char* key, const char* value);

    void save();


    lisp::Protected data_;
};



void load(Settings& output);



void apply();



}
