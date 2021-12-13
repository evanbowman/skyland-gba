#pragma once


#include "skyland/scene/module.hpp"
#include "memory/buffer.hpp"
#include "graphics/overlay.hpp"
#include "bulkAllocator.hpp"



namespace skyland {



class FileBrowserModule : public Module<FileBrowserModule> {
public:
    static const char* module_name()
    {
        return "File Browser";
    }


    static u16 icon()
    {
        return 1000;
    }


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    Buffer<Text, 15> lines_;
    std::optional<Text> info_;
    std::optional<Text> path_text_;


    StringBuffer<200> cwd() const;

    static const int max_display_files_per_folder = 62;

    using CwdNames = Buffer<StringBuffer<20>, max_display_files_per_folder>;
    std::optional<DynamicMemory<CwdNames>> cwd_names_;

    enum SelectedFilesystem {
        none,
        sram,
        rom,
    } selected_filesystem_ = SelectedFilesystem::none;

    void repaint(Platform& pfrm);

    int scroll_index_ = 0;


    static const int max_folder_name = 20;
    static const int max_path_nesting = 12;

    using PathBuffer = Buffer<StringBuffer<max_folder_name>, max_path_nesting>;
    std::optional<DynamicMemory<PathBuffer>> path_;

    static Factory factory_;
};



}
