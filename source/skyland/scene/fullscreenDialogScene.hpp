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


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    bool advance_text(Platform& pfrm, App& app, Microseconds delta, bool sfx);

    void clear_textbox(Platform& pfrm);

    struct TextWriterState
    {
        const char* current_word_;
        Microseconds timer_;
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

    DialogBuffer buffer_;

    DeferredScene next_scene_;
};



} // namespace skyland
