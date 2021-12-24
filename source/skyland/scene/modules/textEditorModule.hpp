#pragma once


#include "skyland/scene/module.hpp"
#include "memory/buffer.hpp"
#include "graphics/overlay.hpp"
#include "bulkAllocator.hpp"



namespace skyland {



class TextEditorModule : public Module<TextEditorModule> {
public:


    TextEditorModule() = default;



    enum class FileMode {
        create,
        update
    };



    enum class FileSystem : u8 {
        sram,
        rom
    };



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

    char* insert_pos();
    void insert_char(char c);
    void erase_char();


    void show_status(Platform& pfrm);


    ScratchBufferPtr text_buffer_;


    const char* current_line() const;
    int line_length() const;

    int skip_word();
    int back_word();

    StringBuffer<32> current_word();


    struct State {
        StringBuffer<64> file_path_;
        bool modified_;
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


    StringBuffer<24> current_word_;
    Buffer<StringBuffer<20>, 6> completions_;


    std::optional<Text> header_;
    std::optional<Text> status_;


    static Factory factory_;
};



}
