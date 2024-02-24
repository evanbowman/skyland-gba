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
