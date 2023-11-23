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
#include "script/lisp.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"


namespace skyland
{


class LispReplScene : public Scene
{
public:
    LispReplScene();

    void enter(App& app, Scene& prev) override;
    void exit(App& app, Scene& next) override;

    ScenePtr<Scene> update(App& app, Microseconds delta) override;

    using Command = StringBuffer<256>;

private:
    enum class DisplayMode {
        entry,
        show_result,
        completion_list,
    } display_mode_ = DisplayMode::show_result;

    Vec2<int> keyboard_cursor_;

    void repaint_entry(bool show_cursor = true);

    void repaint_completions();


    static constexpr const int completion_count = 16;

    struct Completions
    {
        Vector<const char*> completion_strs_;
        Buffer<Text, completion_count> completions_;
        u8 completion_cursor_ = 0;
        u8 completion_prefix_len_ = 0;
    };

    DynamicMemory<Command> command_;
    DynamicMemory<Completions> cpl_;
    Vector<Command> history_;

    std::optional<Text> keyboard_top_;
    std::optional<Text> keyboard_bottom_;
    Buffer<Text, 7> keyboard_;

    std::optional<Text> version_text_;

    Microseconds timer_ = 0;

    void reset_history_index()
    {
        history_index_ = 0;
        history_insert_pos_ = -1;
    }

    std::optional<Text> entry_;
    s16 history_insert_pos_ = -1;
    u8 history_index_ = 0;
    bool alt_ = false;
};


} // namespace skyland
