#pragma once



#include "coins.hpp"
#include "worldMap.hpp"
#include "number/endian.hpp"



namespace skyland {



struct PersistentData {
    Coins coins_ = 0; // TODO: use HostInteger<> here?
    WorldMap world_map_;
    Vec2<u8> current_map_location_ = {0, 1};
    int zone_ = 0;
};



}
