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
#include "script/lisp.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"


namespace skyland
{


class LispReplScene : public Scene
{
public:
    LispReplScene();

    void enter(Scene& prev) override;
    void exit(Scene& next) override;

    ScenePtr update(Time delta) override;

    using Command = StringBuffer<256>;

    bool gui_mode_ = false;

    void repaint(bool focused);

    bool entry_empty() const;

    bool clobbered_tiles_ = false;


    void inject_command(const char* str);


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

    Optional<Text> keyboard_top_;
    Optional<Text> keyboard_bottom_;
    Buffer<Text, 7> keyboard_;

    Optional<Text> version_text_;

    Time timer_ = 0;

    void reset_history_index()
    {
        history_index_ = 0;
        history_insert_pos_ = -1;
    }

    Time scroll_timer_ = 0;
    Optional<Text> entry_;
    s16 history_insert_pos_ = -1;
    u8 history_index_ = 0;

    u8 scroll_counter_ = 0;

    bool alt_ = false;
};


} // namespace skyland
