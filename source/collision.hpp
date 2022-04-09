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
