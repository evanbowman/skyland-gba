////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "preload.hpp"
#include "stateBit.hpp"
#include "containers/vector.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/loadLevelScene.hpp"
#include "script/lisp.hpp"



namespace skyland
{



ScriptPreloadGuard::ScriptPreloadGuard()
{
    if (script_preload_active()) {
        LOGIC_ERROR();
    }
    state_bit_store(StateBit::script_preload_active, true);
}



ScriptPreloadGuard::~ScriptPreloadGuard()
{
    state_bit_store(StateBit::script_preload_active, false);
}



bool script_preload_active()
{
    return state_bit_load(StateBit::script_preload_active);
}



struct BackgroundFadePayload
{
    Vector<u8> background_fade_amounts_;
    u32 background_fade_index_ = 0;
    int fade_alternate_ = 0;
};



static void background_fade_task(void* data)
{
    auto fade_data = (BackgroundFadePayload*)data;
    auto& fade_amounts = fade_data->background_fade_amounts_;
    auto& index = fade_data->background_fade_index_;

    Platform::Extensions::QuickfadeConfig conf;
    conf.include_sprites_ = true;

    // NOTE: this code is running in an interrupt handler, and we don't have
    // enough cpu available to fade many layers at once without stalling audio
    // interrupts. So we alternate fading one of the layers, and considering 255
    // levels of fading and the gba's restricted colorspace, this staggered
    // fading isn't perceptible.
    fade_data->fade_alternate_ += 1;
    fade_data->fade_alternate_ %= 3;
    switch (fade_data->fade_alternate_) {
    case 0:
        conf.include_tile1_ = true;
        break;

    case 1:
        conf.include_overlay_ = true;
        break;

    case 2:
        conf.include_tile0_ = true;
        break;
    }

    if (index < fade_amounts.size()) {
        PLATFORM_EXTENSION(
            quickfade, fade_amounts[index++], ColorConstant::rich_black, conf);
    }
}



ElapsedTime preload_script_during_fade(Time fade_out_duration, const char* script_path)
{
    ScriptPreloadGuard preload;

    BackgroundFadePayload fade_state;

    static const Time frame_step = Microseconds(16777);

    ElapsedTime timer = 0;

    Platform::TaskInfo old_task{};
    if (PLATFORM.has_slow_cpu()) {

        static const u8 max_fade_amount = 255;

        Platform::Extensions::QuickfadeConfig conf;
        // We don't need to fade the background upon each frame, because the
        // background layer is covered up and not visible anyway. But later,
        // when we clear tiles in other layers that cover the background, we
        // will want the background palette to have been faded, otherwise you'll
        // suddenly see a bright background showing through on what's meant to
        // be a black screen because the background layer was never included in
        // the fade effect. The simpler thing to do would be to include the
        // background in the set of layers that we're incrementally fading, but
        // that would waste cpu in a critical piece of code.
        conf.include_background_ = true;
        PLATFORM_EXTENSION(
            quickfade, max_fade_amount, ColorConstant::rich_black, conf);

        Time temp = 0;
        // NOTE: this assumes 60 fps!
        while (temp < fade_out_duration) {
            temp += frame_step;
            // NOTE: the interrupt handler that's running this fade sequence
            // cannot run heavy functions like smoothstep.
            u8 amount =
                max_fade_amount * smoothstep(0.f, fade_out_duration, temp);
            fade_state.background_fade_amounts_.push_back(amount);
        }

        old_task =
            PLATFORM.set_background_task(background_fade_task, &fade_state);
    }

    LoadLevelScene::update_weather();
    APP.invoke_script(script_path);
    lisp::gc();
    PLATFORM.set_background_task(old_task);

    timer += frame_step * fade_state.background_fade_index_;
    return timer;
}



} // namespace skyland
