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
