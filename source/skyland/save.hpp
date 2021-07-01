#pragma once

#include "persistentData.hpp"



class Platform;



namespace skyland {
namespace save {



struct SaveData {
    HostInteger<u32> magic_;
    PersistentData data_;

    // We have some persistent data used by the application (above). But, we
    // also want to serialize a bunch of data used by the lisp interpreter,
    // and just eval it later.
    HostInteger<u32> script_length_;
    // u8 script_[...]; variable-sized data to follow...
};



static_assert(std::is_trivially_copyable<SaveData>::value,
              "SaveData will be memcpy'd to the output destination, and "
              "therefore must be trivially copyable.");



void store(Platform& pfrm, const PersistentData& d);



bool load(Platform& pfrm, PersistentData& d);



void erase(Platform& pfrm);



} // namespace save
} // namespace skyland
