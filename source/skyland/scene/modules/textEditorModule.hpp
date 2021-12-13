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
    } mode_ = Mode::explore;


    void render(Platform& pfrm, int start_line);


    std::optional<ScratchBufferPtr> text_buffer_;


    int start_line_ = 0;
    int column_offset_ = 0;
    int line_count_ = 0;


    std::optional<Text> header_;
    std::optional<Text> status_;


    static Factory factory_;
};



}
