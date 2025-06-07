////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "containers/vector.hpp"
#include "persistentData.hpp"
#include "serial.hpp"
#include "string.hpp"



class Platform;



namespace skyland
{


class App;


namespace save
{



struct EmergencyBackup
{
    PersistentData persistent_data_;
    Optional<Vector<char>> lisp_data_;
    rng::LinearGenerator rng_state_;

    bool valid_ = false;

    // Used when restoring a backup when retrying a level, rather than restoring
    // from a crash. Restoring from a crash puts you back on the world map,
    // while retrying a level puts you at the beginning of the level.
    s8 next_world_location_ = -1;

    void init();

    void store();
};



struct SaveData
{
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



struct GlobalSaveData
{
    u32 reserved_;
    HostInteger<u32> magic_;
    GlobalPersistentData data_;
};



bool load_global_data(GlobalPersistentData&);



void store_global_data(const GlobalPersistentData&);



void store(const char* prefix_path, const PersistentData& d);



bool exists();



bool load(const char* prefix_path, PersistentData& d);



void erase();



} // namespace save
} // namespace skyland
