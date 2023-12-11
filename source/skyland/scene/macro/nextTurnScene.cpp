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



ScenePtr<Scene> NextTurnScene::update(Time delta)
{
    auto& m = macrocosm();

    if (raster::globalstate::is_night) {
        return scene_pool::alloc<SelectorScene>();
    }

    int current = m.data_->p().day_night_cyc_.get();
    m.data_->p().day_night_cyc_.set(std::max(current, day_frames - 256));

    return scene_pool::alloc<DayTransitionScene>();
}



} // namespace skyland::macro
