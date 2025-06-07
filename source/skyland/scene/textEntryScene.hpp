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

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland
{



class TextEntryScene : public Scene
{
public:
    using Receiver = Function<4 * sizeof(void*), ScenePtr(const char* text)>;


    TextEntryScene(const char* prompt,
                   Receiver receiver,
                   int required_chars,
                   int char_limit,
                   const char* default_text = "");


    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    void render_keyboard();


private:
    struct State
    {
        StringBuffer<100> prompt_;
        StringBuffer<100> buffer_;
    };

    DynamicMemory<State> state_;
    Vec2<u8> keyboard_cursor_;

    Receiver receiver_;

    Optional<Text> entry_;
    Optional<Text> prompt_text_;
    Optional<Text> submit_text_;

    int required_chars_;
    int char_limit_;
};



} // namespace skyland
