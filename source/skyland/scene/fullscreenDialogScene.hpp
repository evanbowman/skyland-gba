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
