#pragma once



#include "achievement.hpp"
#include "coins.hpp"
#include "flag.hpp"
#include "highscores.hpp"
#include "number/endian.hpp"
#include "worldGraph.hpp"



namespace skyland {



struct GlobalPersistentData {
    FlagPixels flag_img_;
    Highscores highscores_;

    enum Flags0 { developer_mode = (1 << 0), tutorial_prompt };

    u8 flags0_ = developer_mode;
    u8 flags1_ = 0;
    u8 flags2_ = 0;
    u8 flags3_ = 0;

    u64 achievement_flags_ = 0;
};



struct PersistentData {
    Coins coins_ = 0; // TODO: use HostInteger<> here?
    WorldGraph world_graph_;
    int current_world_location_ = 0;
    int zone_ = 0;

    enum class Difficulty : u8 {
        beginner,
        experienced,
        expert,
    } difficulty_ = Difficulty::experienced;

    HostInteger<u32> total_seconds_;
    HostInteger<u32> total_pauses_;
    HostInteger<u16> replicants_created_;
    HostInteger<s32> score_;
};



} // namespace skyland
