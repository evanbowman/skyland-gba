////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/dialog.hpp"
#include "worldScene.hpp"



namespace skyland
{



// Copy-pasted from the fullscreen dialog scene, but with the base class and a
// couple of other things changed. Argh this is so bad by I'm in a hurry.



class BoxedDialogScene : public WorldScene
{
public:
    BoxedDialogScene(DialogBuffer buffer, bool expects_answer_y_n)
        : buffer_(std::move(buffer)), expects_answer_y_n_(expects_answer_y_n),
          data_(allocate_dynamic<Data>("dialog-data"))
    {
        goto_tutorial_ = 0;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    void process_command(Platform& pfrm, App& app);

    bool advance_text(Platform& pfrm, App& app, Microseconds delta, bool sfx);

    void clear_textbox(Platform& pfrm);

    struct TextWriterState
    {
        const char* current_word_;
        Microseconds timer_;
        u8 line_;
        u8 pos_;
        u8 current_word_remaining_;
        u8 speed_ = 0;
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

    u8 expects_answer_y_n_ : 1;
    u8 goto_tutorial_ : 5;

    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
    std::optional<UIMetric> coins_;
    std::optional<Text> character_name_text_;
    bool choice_sel_ = true;



    struct Data
    {
        struct CharacterDescription
        {
            // Yeah, the screen is only 30 tiles wide, but remember, this buffer
            // holds utf8 text!
            StringBuffer<32> name_;
            u16 image_ = 0;
        } character_;
    };

    DynamicMemory<Data> data_;
};



} // namespace skyland
