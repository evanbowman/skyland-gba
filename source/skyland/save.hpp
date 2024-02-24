////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



void store(const PersistentData& d);



bool exists();



bool load(PersistentData& d);



void erase();



} // namespace save
} // namespace skyland
