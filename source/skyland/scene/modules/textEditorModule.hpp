#pragma once


#include "bulkAllocator.hpp"
#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene/module.hpp"
#include "vector.hpp"



namespace skyland {



class TextEditorModule : public Module<TextEditorModule> {
public:
    TextEditorModule() = default;



    enum class FileMode { create, update };



    enum class FileSystem : u8 { sram, rom };



    TextEditorModule(Platform& pfrm,
                     const char* file_path,
                     FileMode file_mode = FileMode::update,
                     FileSystem filesystem = FileSystem::sram);



    static const char* module_name()
    {
        return "Text Editor";
    }


    static u16 icon()
    {
        // TODO...
        return 952;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

private:
    enum class Mode {
        explore,
        edit,
        autocomplete,
    } mode_ = Mode::explore;


    void render(Platform& pfrm, int start_line);
    void render_keyboard(Platform& pfrm);
    void render_completions(Platform& pfrm);

    Vector<char>::Iterator insert_pos();
    void insert_char(char c);
    void erase_char();


    void show_status(Platform& pfrm);


    Vector<char> text_buffer_;


    Vector<char>::Iterator current_line();
    int line_length();

    int skip_word();
    int back_word();
    int skip_paragraph();
    int back_paragraph();

    StringBuffer<32> current_word();

    void center_view(Platform& pfrm);


    struct State {
        StringBuffer<64> file_path_;
        bool modified_;

        StringBuffer<24> current_word_;
        Buffer<StringBuffer<20>, 6> completions_;
    };

    DynamicMemory<State> state_;



    int key_held_timer_[4] = {0, 0, 0, 0};


    u16 start_line_ = 0;
    u16 column_offset_ = 0;
    u16 line_count_ = 0;
    u16 ideal_cursor_right_ = 0;

    u8 stashed_palette_ = 0;

    FileSystem filesystem_ = FileSystem::sram;

    bool show_keyboard_ = false;
    bool show_completions_ = false;

    Vec2<int> cursor_;
    Vec2<int> keyboard_cursor_;
    int selected_completion_ = 0;

    Microseconds cursor_flicker_timer_ = 0;
    bool cursor_shaded_ = false;


    std::optional<Text> header_;
    std::optional<Text> status_;


    static Factory factory_;
};



} // namespace skyland
