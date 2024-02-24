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

    ScenePtr<Scene> update(Time delta) override;

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

    Optional<Text> entry_;
    s16 history_insert_pos_ = -1;
    u8 history_index_ = 0;
    bool alt_ = false;
};


} // namespace skyland
