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

#include <variant>

// #include "blind_jump/game.hpp"
#include "graphics/overlay.hpp"
#include "skyland/entity.hpp"
#include "skyland/roomPool.hpp"
#include "skyland/scene_pool.hpp"


// struct BlindJumpGlobalData {
//     Game::EnemyGroup::Pool_ enemy_pool_;
//     Game::EnemyGroup::NodePool_ enemy_node_pool_;

//     Game::DetailGroup::Pool_ detail_pool_;
//     Game::DetailGroup::NodePool_ detail_node_pool_;

//     Game::EffectGroup::Pool_ effect_pool_;
//     Game::EffectGroup::NodePool_ effect_node_pool_;

//     Bitmatrix<TileMap::width, TileMap::height> visited_;
// };


struct ExampleGlobalData
{
    u8 buffer[2048];
};


struct SkylandGlobalData
{
    skyland::scene_pool::_Pool scene_pool_;

    Vec2<u8> near_cursor_loc_;
    Vec2<u8> far_cursor_loc_;

    skyland::EntityNodePool<skyland::entity_pool_size> entity_node_pool_;
    skyland::EntityPools entity_pools_;
    skyland::room_pool::RoomPools room_pools_;

    std::optional<Text> multiplayer_prep_text_;
    Microseconds multiplayer_prep_timer_ = 0;
    u16 multiplayer_prep_seconds_ = 0;

    bool unhide_multiplayer_prep_ = false;

    Vec2<u8> co_op_cursor_;
    bool co_op_cursor_near_ = true;
    u8 co_op_cursor_icon_ = 15;

    u8 multiplayer_pauses_remaining_ = 0;
    bool multiplayer_pause_owner_ = false;

    SkylandGlobalData()
        : scene_pool_("scenes"), entity_node_pool_("entity-list-node")
    {
    }
};


// This data used to be static members of the EntityGroup<> class template. But
// I am trying to keep better track of global variables across the project, so I
// am declaring them all in this one structure. I have been toying with the idea
// of compiling multiple games into a single rom/executable, which makes keeping
// track of global variable memory usage important.
using Globals = std::variant< // BlindJumpGlobalData,
    SkylandGlobalData>;


Globals& globals();
