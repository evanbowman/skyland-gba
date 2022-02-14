#pragma once

#include "persistentData.hpp"
#include "string.hpp"
#include "serial.hpp"



class Platform;



namespace skyland {


class App;


namespace save {



struct EmergencyBackup {
    PersistentData persistent_data_;
    SerialString lisp_data_;
    bool valid_ = false;

    void init(Platform& pfrm, App& app);

    void store(Platform& pfrm);
};



struct SaveData {
    HostInteger<u32> magic_;
    PersistentData data_;

    // NOTE: scripts were once attached to the end of this struct. Now, they
    // aren't, but we still maintain the script_length_ field, for backwards
    // compatibility (script length always assigned a zero value).
    HostInteger<u32> script_length_;
};



static_assert(std::is_trivially_copyable<SaveData>::value,
              "SaveData will be memcpy'd to the output destination, and "
              "therefore must be trivially copyable.");



struct GlobalSaveData {
    u32 reserved_;
    HostInteger<u32> magic_;
    GlobalPersistentData data_;
};



bool load_global_data(Platform&, GlobalPersistentData&);



void store_global_data(Platform&, const GlobalPersistentData&);



void store(Platform& pfrm, App& app, const PersistentData& d);



bool load(Platform& pfrm, App& app, PersistentData& d);



void erase(Platform& pfrm);



} // namespace save
} // namespace skyland
