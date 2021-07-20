#pragma once

#include "persistentData.hpp"



class Platform;



namespace skyland {
namespace save {



bool load_global_data(Platform&, GlobalPersistentData&);



void store_global_data(Platform&, const GlobalPersistentData&);



void store(Platform& pfrm, const PersistentData& d);



bool load(Platform& pfrm, PersistentData& d);



void erase(Platform& pfrm);



} // namespace save
} // namespace skyland
