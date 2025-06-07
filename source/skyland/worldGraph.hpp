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

#include "number/numeric.hpp"
#include "number/random.hpp"



namespace skyland
{



class App;



class WorldGraph
{
public:
    static const int width = 21;
    static const int height = 12;


    static const int max_movement_distance = 4;


    WorldGraph()
    {
    }


    void generate();


    struct Node
    {
        enum class Type : u8 {
            null,
            visited,
            neutral,
            hostile,
            corrupted,
            exit,
            quest,
            shop,
            hostile_hidden,
            neutral_hidden,
            quest_marker,
        } type_;

        Vec2<s8> coord_;
    };


    static constexpr const int node_count = 20;


    using PathBuffer = Buffer<Vec2<s8>, node_count>;
    PathBuffer path(Vec2<s8> n1, Vec2<s8> n2);


    Node nodes_[node_count];
    u8 storm_depth_ = 1;
};



} // namespace skyland
