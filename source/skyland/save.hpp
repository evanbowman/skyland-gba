#pragma once

#include "persistentData.hpp"



class Platform;



namespace skyland {
namespace save {


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
    HostInteger<u32> magic_;
    GlobalPersistentData data_;
};



bool load_global_data(Platform&, GlobalPersistentData&);



void store_global_data(Platform&, const GlobalPersistentData&);



void store(Platform& pfrm, const PersistentData& d);



bool load(Platform& pfrm, PersistentData& d);



void erase(Platform& pfrm);



} // namespace save
} // namespace skyland
