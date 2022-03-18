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

    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;

    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;

    using Command = StringBuffer<256>;

private:
    enum class DisplayMode {
        entry,
        show_result,
        completion_list,
    } display_mode_ = DisplayMode::entry;

    Vec2<int> keyboard_cursor_;

    void repaint_entry(Platform& pfrm, bool show_cursor = true);

    void repaint_completions(Platform& pfrm);


    static constexpr const int completion_count = 10;

    struct Completions
    {
        Buffer<const char*, completion_count> completion_strs_;
        Buffer<Text, completion_count> completions_;
        u8 completion_cursor_ = 0;
        u8 completion_prefix_len_ = 0;
    };

    DynamicMemory<Command> command_;
    DynamicMemory<Completions> cpl_;

    std::optional<Text> keyboard_top_;
    std::optional<Text> keyboard_bottom_;
    Buffer<Text, 7> keyboard_;

    std::optional<Text> version_text_;

    Microseconds timer_ = 0;

    std::optional<Text> entry_;
};


} // namespace skyland
