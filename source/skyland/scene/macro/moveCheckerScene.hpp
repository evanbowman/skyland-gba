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

#include "macrocosmScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland::macro
{



class MoveCheckerScene : public MacrocosmScene
{
public:

    MoveCheckerScene(const Vec3<u8>& piece_loc,
                     const Buffer<Vec3<u8>, 2>& slots) :
        piece_loc_(piece_loc),
        slots_(slots)
    {
    }


    void enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev) override
    {
        refresh(state);
    }


    void exit(Platform& pfrm, macro::EngineImpl& state, Scene& next) override
    {
    }


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state) override
    {
        if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
            return scene;
        }

        const u32 prev_slot = current_slot_;

        if (player.key_down(pfrm, Key::action_2)) {
            auto& sector = state.sector();

            for (auto& slot : slots_) {
                sector.set_block(slot, terrain::Type::air);
            }

            sector.set_cursor(piece_loc_);

            return scene_pool::alloc<SelectorScene>();

        } else if (player.key_down(pfrm, Key::action_1)) {
            auto& sector = state.sector();

            const auto type = sector.get_block(piece_loc_).type();

            for (auto& slot : slots_) {
                sector.set_block(slot, terrain::Type::air);
            }
            if (not slots_.empty()) {
                sector.set_block(slots_[current_slot_], type);
            }
            sector.set_block(piece_loc_, terrain::Type::air);
            sector.set_cursor(piece_loc_);

            return scene_pool::alloc<SelectorScene>();

        } else if (player.key_down(pfrm, Key::right)) {
            ++current_slot_;
            current_slot_ %= slots_.size();
        }

        if (current_slot_ not_eq prev_slot) {
            refresh(state);
        }

        return null_scene();
    }


    void refresh(macro::EngineImpl& state)
    {
        auto& sector = state.sector();

        for (u32 i = 0; i < slots_.size(); ++i) {
            if (i == current_slot_) {
                sector.set_block(slots_[i], terrain::Type::air);
            } else {
                sector.set_block(slots_[i], terrain::Type::checker_highlight);
            }
        }

        if (not slots_.empty()) {
            sector.set_cursor(slots_[current_slot_]);
        }
    }


private:
    Vec3<u8> piece_loc_;
    Buffer<Vec3<u8>, 2> slots_;
    u32 current_slot_ = 0;
};



}
