#pragma once

#include "skyland/dialog.hpp"
#include "worldScene.hpp"



namespace skyland {



// Copy-pasted from the fullscreen dialog scene, but with the base class and a
// couple of other things changed. Argh this is so bad by I'm in a hurry.



class BoxedDialogScene : public WorldScene {
public:
    BoxedDialogScene(DialogBuffer buffer,
                     bool expects_answer_y_n,
                     StringBuffer<10> character_name,
                     int character_image)
        : buffer_(std::move(buffer)), expects_answer_y_n_(expects_answer_y_n),
          character_image_(character_image), character_name_(character_name)
    {
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    bool advance_text(Platform& pfrm, App& app, Microseconds delta, bool sfx);

    void clear_textbox(Platform& pfrm);

    struct TextWriterState {
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
        boolean_choice,
        clear,
    } display_mode_ = DisplayMode::animate_in;

    TextWriterState text_state_;

    DialogBuffer buffer_;

    bool expects_answer_y_n_;

    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
    std::optional<UIMetric> coins_;
    std::optional<Text> character_name_text_;
    bool choice_sel_ = true;

    u16 character_image_;

    StringBuffer<12> character_name_;
};



} // namespace skyland
