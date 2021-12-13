#pragma once


#include "skyland/scene/module.hpp"
#include "memory/buffer.hpp"
#include "graphics/overlay.hpp"
#include "bulkAllocator.hpp"



namespace skyland {



class TextEditorModule : public Module<TextEditorModule> {
public:


    TextEditorModule() = default;



    TextEditorModule(Platform& pfrm, const char* ram_file_path);



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

    char* insert_pos();
    void insert_char(char c);
    void erase_char();


    void show_status(Platform& pfrm);


    std::optional<ScratchBufferPtr> text_buffer_;


    const char* current_line() const;
    int line_length() const;

    int skip_word();
    int back_word();


    int start_line_ = 0;
    int column_offset_ = 0;
    int line_count_ = 0;
    int ideal_cursor_right_ = 0;

    bool show_keyboard_ = false;

    Vec2<int> cursor_;
    Vec2<int> keyboard_cursor_;

    Microseconds cursor_flicker_timer_ = 0;
    bool cursor_shaded_ = false;


    std::optional<Text> header_;
    std::optional<Text> status_;


    static Factory factory_;
};



}
