#pragma once



#include "coins.hpp"
#include "number/endian.hpp"
#include "worldMap.hpp"
#include "flag.hpp"
#include "highscores.hpp"



namespace skyland {



struct GlobalPersistentData {
    FlagPixels flag_img_;
    Highscores highscores_;
};



struct PersistentData {
    Coins coins_ = 0; // TODO: use HostInteger<> here?
    WorldMap world_map_;
    Vec2<u8> current_map_location_ = {0, 1};
    int zone_ = 0;

    HostInteger<u32> total_seconds_;
    HostInteger<u32> total_pauses_;
    HostInteger<u16> replicants_created_;
};



} // namespace skyland
