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

    Optional<Text> multiplayer_prep_text_;
    Optional<Text> multiplayer_timeout_text_;
    Microseconds multiplayer_prep_timer_ = 0;
    u16 multiplayer_prep_seconds_ = 0;

    Microseconds multiplayer_timeout_remaining_ = 0;
    Microseconds multiplayer_timeout_countdown_ = 0;
    u16 multiplayer_timeout_repaint_ = 0;

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


using Globals = SkylandGlobalData;


Globals& globals();
