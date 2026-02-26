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
    enum class FileMode { create, update, readonly };



    enum class FileSystem : u8 { sram, rom, device };


    ScenePtr save();


    struct Glyph
    {
        u16 table_entry_;

        bool operator==(const Glyph& other) const
        {
            return table_entry_ == other.table_entry_;
        }

        bool operator not_eq(const Glyph& other) const
        {
            return table_entry_ not_eq other.table_entry_;
        }

        utf8::Codepoint cp(TextEditorModule& te) const;
    };

    void copy_selected(Vector<utf8::Codepoint>& output);
    void paste(Vector<utf8::Codepoint>& contents);


    enum class SyntaxMode : u8 {
        lisp,
        plain_text,
        ini,
        python,
    };


    static StringBuffer<32> extract_filename(const char* path);

    StringBuffer<64> file_path();


    void deselect();


    void shade_cursor();


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


    ScenePtr update(Time delta) override;


    struct ParserState
    {
        bool comment = false;
        bool quotation = false;
        bool endquote = false;
        bool keyword = false;

        StringBuffer<32> parse_word_;
    };


    void handle_char(Vector<Glyph>::Iterator data,
                     utf8::Codepoint c,
                     ParserState& ps);


    void repaint();


    u8 cursor_y_offset() const;


    int y_max() const;


    Glyph load_glyph(utf8::Codepoint c);

    Vector<utf8::Codepoint> glyph_table_;
    void init_glyph_table();


    bool gui_mode_ = false;

    enum class Mode {
        nav,
        edit,
        autocomplete,
    } mode_ = Mode::nav;


    FileSystem which_fs() const;

    s8 browser_index_ = -1;


private:
    void render(int start_line);
    void render_keyboard();
    void render_completions();


    bool export_as_ascii(Vector<char>& output);


    Vector<Glyph>::Iterator insert_pos();
    void insert_char(Glyph c,
                     Optional<Vector<Glyph>::Iterator> insert_hint = {});
    void erase_char(Optional<Vector<Glyph>::Iterator> erase_hint = {});


    void delete_selection();
    void save_selection(Vector<utf8::Codepoint>& output);
    void paste_selection(Vector<utf8::Codepoint>& source);


    bool should_smooth_scroll() const;


    void show_status();


    Vector<Glyph> text_buffer_;


    void tabs_to_spaces();


    Vector<Glyph>::Iterator current_line();
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

        Optional<Vector<Glyph>::Iterator> sel_begin_;
        Optional<Vector<Glyph>::Iterator> sel_end_;
        Optional<Vector<Glyph>::Iterator> sel_center_;
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
    bool exit_to_browser_ = false;

    Vec2<int> cursor_;
    Vec2<int> keyboard_cursor_;
    int selected_completion_ = 0;

    Time cursor_flicker_timer_ = 0;
    bool cursor_shaded_ = false;

    Glyph newline_glyph_;
    Glyph null_glyph_;

    Optional<Text> header_;
    Optional<Text> status_;
    Optional<std::pair<int, Vector<Glyph>::Iterator>> cached_start_;

    static Factory factory_;
};



} // namespace skyland
