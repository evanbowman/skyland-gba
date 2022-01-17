#pragma once

#include "number/numeric.hpp"
#include "number/random.hpp"



namespace skyland {



class WorldGraph {
public:
    static const int width = 21;
    static const int height = 12;


    static const int max_movement_distance = 4;


    WorldGraph()
    {
        generate();
    }


    void generate();


    struct Node {
        enum class Type : u8 {
            null,
            visited,
            neutral,
            hostile,
            corrupted,
            exit,
            quest,
            hub,
            hostile_hidden,
            neutral_hidden,
        } type_;

        Vec2<s8> coord_;
    };


    Node nodes_[20];
    u8 storm_depth_ = 1;
};



} // namespace skyland
