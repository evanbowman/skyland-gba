#pragma once



#include "achievement.hpp"
#include "coins.hpp"
#include "flag.hpp"
#include "highscores.hpp"
#include "number/endian.hpp"
#include "worldGraph.hpp"
#include "bitvector.hpp"



namespace skyland {



struct GlobalPersistentData
{
    FlagPixels flag_img_;
    Highscores highscores_;

    enum Flags0 {
        developer_mode = (1 << 0),
        tutorial_prompt = (1 << 1),
        configured_clock = (1 << 2),
    };

    u8 flags0_ = developer_mode;
    u8 flags1_ = 0;
    u8 flags2_ = 0;
    u8 flags3_ = 0;

    host_u64 achievement_flags_;
    host_u64 challenge_flags_;

    // Yeah, we'll break compatibility with save files if we ever have to resize
    // this bitvector. But in that case, we could simply offload the data to the
    // sram filesystem instead, and use this bitvector for something else. At
    // time of writing this comment, the game has around 44 different blocks, so
    // we're nowhere near running out.
    Bitvector<128> hidden_rooms_;

    GlobalPersistentData()
    {
        achievement_flags_.set(0);
        challenge_flags_.set(0);
    }
};



struct PersistentData
{
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
