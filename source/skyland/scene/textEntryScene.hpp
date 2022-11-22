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

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland
{



class TextEntryScene : public Scene
{
public:
    using Receiver =
        Function<4 * sizeof(void*), ScenePtr<Scene>(const char* text)>;


    TextEntryScene(const char* prompt,
                   Receiver receiver,
                   int required_chars,
                   int char_limit,
                   const char* default_text = "");


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    void render_keyboard(Platform& pfrm);


private:
    struct State
    {
        StringBuffer<100> prompt_;
        StringBuffer<100> buffer_;
    };

    DynamicMemory<State> state_;
    Vec2<u8> keyboard_cursor_;

    Receiver receiver_;

    std::optional<Text> entry_;
    std::optional<Text> prompt_text_;
    std::optional<Text> submit_text_;

    int required_chars_;
    int char_limit_;
};



} // namespace skyland
