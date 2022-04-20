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


#include "createBlockScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland::macro
{



ScenePtr<Scene>
CreateBlockScene::update(Platform& pfrm, Player& player, macro::State& state)
{
    if (auto next = MacrocosmScene::update(pfrm, player, state)) {
        return next;
    }

    if (player.key_down(pfrm, Key::action_1)) {
        auto cursor = state.data_->sector_.cursor();
        if (cursor.z < macro::terrain::Sector::z_limit - 1) {
            state.data_->sector_.set_block(cursor,
                                           macro::terrain::Type::masonry);
            ++cursor.z;
            state.data_->sector_.set_cursor(cursor);
            return scene_pool::alloc<SelectorScene>();
        }
    }

    if (player.key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<SelectorScene>();
    }

    return null_scene();
}



} // namespace skyland::macro
