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


struct ExampleGlobalData {
    u8 buffer[2048];
};


struct SkylandGlobalData {
    skyland::room_pool::_Pool room_pool_;
    skyland::scene_pool::_Pool scene_pool_;

    Vec2<u8> near_cursor_loc_;
    Vec2<u8> far_cursor_loc_;

    skyland::EntityNodePool<skyland::entity_pool_size> entity_node_pool_;
    skyland::EntityPools entity_pools_;

    std::optional<Text> multiplayer_prep_text_;
    Microseconds multiplayer_prep_timer_ = 0;
    u16 multiplayer_prep_seconds_ = 0;

    bool unhide_multiplayer_prep_ = false;
    int levels_since_music_ = 0;
};


// This data used to be static members of the EntityGroup<> class template. But
// I am trying to keep better track of global variables across the project, so I
// am declaring them all in this one structure. I have been toying with the idea
// of compiling multiple games into a single rom/executable, which makes keeping
// track of global variable memory usage important.
using Globals = std::variant< // BlindJumpGlobalData,
    SkylandGlobalData>;


Globals& globals();
