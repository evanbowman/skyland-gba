////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    void generate(App&);


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


    Node nodes_[20];
    u8 storm_depth_ = 1;
};



} // namespace skyland
