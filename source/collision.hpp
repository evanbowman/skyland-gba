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


#include "containers/list.hpp"
#include "hitbox.hpp"



class Game;
class Platform;


template <typename A, typename B, typename Pl1, typename Pl2>
void check_collisions(Platform& pf,
                      Game& game,
                      List<A, Pl1>& lhs,
                      List<B, Pl2>& rhs)
{
    for (auto& a : lhs) {
        if (a->visible()) {
            for (auto& b : rhs) {
                if (a->hitbox().overlapping(b->hitbox())) {
                    a->on_collision(pf, game, *b);
                    b->on_collision(pf, game, *a);
                }
            }
        }
    }
}


template <typename A, typename B, typename Pl1>
void check_collisions(Platform& pf, Game& game, A& lhs, List<B, Pl1>& rhs)
{
    for (auto& b : rhs) {
        if (b->visible()) {
            if (lhs.hitbox().overlapping(b->hitbox())) {
                lhs.on_collision(pf, game, *b);
                b->on_collision(pf, game, lhs);
            }
        }
    }
}
