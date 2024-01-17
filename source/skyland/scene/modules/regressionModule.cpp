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

#include "regressionModule.hpp"
#include "heap_data.hpp"
#include "script/lisp.hpp"
#include "skyland/scene/selectTutorialScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



HEAP_DATA s8 test_index = -1;



ScenePtr<Scene> RegressionModule::update(Time delta)
{
    state_bit_store(StateBit::regression, true);

    if (test_index == -1) {
        APP.invoke_script("/scripts/misc/unittest.lisp");

        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1);
        PLATFORM.screen().clear();
        Text::print("core regression passed!", {1, 1});
        Text::print("validating tutorials...", {1, 3});
        PLATFORM.screen().display();

        PLATFORM.sleep(120);

        test_index++;

        BasicCharacter::__reset_ids();

    } else {

        if (test_index > 0) {
            APP.invoke_script("/scripts/tutorials/test/common.lisp");
            APP.invoke_script(
                format("/scripts/tutorials/test/%.lisp", test_index - 1)
                    .c_str());
        }

        if (test_index == SelectTutorialScene::tutorial_count()) {
            PLATFORM.fill_overlay(0);
            PLATFORM.screen().schedule_fade(0);
            PLATFORM.screen().schedule_fade(1);
            PLATFORM.screen().clear();
            Text::print("all regression passed!", {1, 1});
            u32 mstack = 0;
            PLATFORM.system_call("stack_usage", &mstack);
            Text::print(format("max stack used %", mstack).c_str(), {1, 3});
            Text::print("press any key to reset...", {1, 5});

            while (1) {
                PLATFORM.keyboard().poll();
                PLATFORM.system_call("feed-watchdog", nullptr);

                if (PLATFORM.keyboard()
                        .down_transition<Key::action_1,
                                         Key::action_2,
                                         Key::down,
                                         Key::up,
                                         Key::left,
                                         Key::right>()) {
                    PLATFORM.system_call("restart", nullptr);
                }

                PLATFORM.screen().clear();
                PLATFORM.screen().display();
            }
        }

        auto ret = scene_pool::alloc<SelectTutorialScene>();
        ret->quick_select(test_index++);
        return ret;
    }

    return null_scene();
}



RegressionModule::Factory RegressionModule::factory_(true);



} // namespace skyland
