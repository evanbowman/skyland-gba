#pragma once



#include "coins.hpp"
#include "flag.hpp"
#include "highscores.hpp"
#include "number/endian.hpp"
#include "worldMap.hpp"



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
    HostInteger<s32> score_;

    enum Flags0 {
        enable_mods = (1 << 0),
    };

    u8 flags0_ = 0;
    u8 flags1_ = 0;
    u8 flags2_ = 0;
    u8 flags3_ = 0;

    u8 unused_[64];
};



} // namespace skyland
