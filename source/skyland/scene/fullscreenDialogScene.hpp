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

#include "skyland/dialog.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class FullscreenDialogScene : public Scene
{
public:
    FullscreenDialogScene(DialogBuffer buffer, DeferredScene next_scene)
        : buffer_(std::move(buffer)), next_scene_(next_scene)
    {
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


private:
    bool advance_text(Time delta, bool sfx);

    void clear_textbox();

    void process_command();

    u8 y_start() const;

    struct TextWriterState
    {
        const char* current_word_;
        Time timer_;
        u8 line_;
        u8 pos_;
        u8 current_word_remaining_;
    };

    enum class DisplayMode {
        animate_in,
        busy,
        key_released_check1,
        key_released_check2,
        wait,
        done,
        animate_out,
        clear,
    } display_mode_ = DisplayMode::animate_in;

    TextWriterState text_state_;
    bool img_view_ = false;
    bool halt_text_ = false;

    DialogBuffer buffer_;

    DeferredScene next_scene_;
};



} // namespace skyland
