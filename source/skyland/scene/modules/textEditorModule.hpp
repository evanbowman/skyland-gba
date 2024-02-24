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
#include "containers/vector.hpp"
#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene/module.hpp"
#include "userContext.hpp"



namespace skyland
{



class TextEditorModule : public Module<TextEditorModule>
{
public:
    TextEditorModule() = default;



    enum class FileMode { create, update, readonly };



    enum class FileSystem : u8 { sram, rom, device };



    enum class SyntaxMode : u8 {
        lisp,
        plain_text,
        ini,
        python,
    };



    TextEditorModule(UserContext&& context,
                     const char* file_path,
                     SyntaxMode syntax_mode,
                     FileMode file_mode = FileMode::update,
                     FileSystem filesystem = FileSystem::sram);


    // A special constructor, for opening the syslog.
    TextEditorModule(UserContext&& context);


    static bool run_scripts()
    {
        return false;
    }


    static SystemString module_name()
    {
        return SystemString::module_text_editor;
    }


    static u16 icon()
    {
        // TODO...
        return 952;
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr<Scene> update(Time delta) override;


    struct ParserState
    {
        bool comment = false;
        bool quotation = false;
        bool endquote = false;
        bool keyword = false;

        StringBuffer<32> parse_word_;
    };


    void handle_char(Vector<char>::Iterator data, char c, ParserState& ps);


private:
    enum class Mode {
        nav,
        edit,
        autocomplete,
    } mode_ = Mode::nav;


    void render(int start_line);
    void render_keyboard();
    void render_completions();


    Vector<char>::Iterator insert_pos();
    void insert_char(char c, Optional<Vector<char>::Iterator> insert_hint = {});
    void erase_char(Optional<Vector<char>::Iterator> erase_hint = {});


    void delete_selection();
    void save_selection(Vector<char>& output);
    void paste_selection(Vector<char>& source);


    void show_status();


    Vector<char> text_buffer_;


    void tabs_to_spaces();


    Vector<char>::Iterator current_line();
    int line_length();

    int skip_word();
    int back_word();
    int skip_paragraph();
    int back_paragraph();

    StringBuffer<32> current_word();

    void center_view();


    struct State
    {
        StringBuffer<64> file_path_;
        bool modified_;

        StringBuffer<24> current_word_;
        Buffer<StringBuffer<20>, 6> completions_;

        Optional<Vector<char>::Iterator> sel_begin_;
        Optional<Vector<char>::Iterator> sel_end_;
        Optional<Vector<char>::Iterator> sel_center_;
    };

    DynamicMemory<State> state_;


    UserContext user_context_;


    u16 start_line_ = 0;
    u16 column_offset_ = 0;
    u16 line_count_ = 0;
    u16 ideal_cursor_right_ = 0;

    u8 stashed_palette_ = 0;

    FileSystem filesystem_ = FileSystem::sram;
    SyntaxMode syntax_mode_;
    FileMode file_mode_;

    bool show_keyboard_ = false;
    bool show_completions_ = false;

    Vec2<int> cursor_;
    Vec2<int> keyboard_cursor_;
    int selected_completion_ = 0;

    Time cursor_flicker_timer_ = 0;
    bool cursor_shaded_ = false;


    Optional<Text> header_;
    Optional<Text> status_;


    static Factory factory_;
};



} // namespace skyland
