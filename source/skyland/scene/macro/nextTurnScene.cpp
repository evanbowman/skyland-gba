////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    ScenePtr update(Player& player, macro::EngineImpl& state)
    {
        MacrocosmScene::update(player, state);

        state.data_->p().day_night_cyc_.set(
            state.data_->p().day_night_cyc_.get() + 6);

        if (not raster::globalstate::is_night and
            abs(state.data_->p().day_night_cyc_.get() - 256) < 10) {
            return make_scene<SelectorScene>();
        }

        return null_scene();
    }
};



ScenePtr NextTurnScene::update(Time delta)
{
    auto& m = macrocosm();

    if (raster::globalstate::is_night) {
        return make_scene<SelectorScene>();
    }

    int current = m.data_->p().day_night_cyc_.get();
    m.data_->p().day_night_cyc_.set(std::max(current, day_frames - 256));

    return make_scene<DayTransitionScene>();
}



} // namespace skyland::macro
