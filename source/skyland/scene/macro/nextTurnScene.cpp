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


#include "nextTurnScene.hpp"
#include "menuOptionsScene.hpp"
#include "selectorScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



class DayTransitionScene : public MacrocosmScene
{
public:
    ScenePtr<Scene> update(Player& player, macro::EngineImpl& state)
    {
        MacrocosmScene::update(player, state);

        state.data_->p().day_night_cyc_.set(
            state.data_->p().day_night_cyc_.get() + 6);

        if (not raster::globalstate::is_night and
            abs(state.data_->p().day_night_cyc_.get() - 256) < 10) {
            return scene_pool::alloc<SelectorScene>();
        }

        return null_scene();
    }
};



ScenePtr<Scene> NextTurnScene::update(App& app, Microseconds delta)
{
    auto& m = macrocosm(app);

    if (raster::globalstate::is_night) {
        return scene_pool::alloc<SelectorScene>();
    }

    int current = m.data_->p().day_night_cyc_.get();
    m.data_->p().day_night_cyc_.set(std::max(current, day_frames - 256));

    return scene_pool::alloc<DayTransitionScene>();
}



} // namespace skyland::macro
