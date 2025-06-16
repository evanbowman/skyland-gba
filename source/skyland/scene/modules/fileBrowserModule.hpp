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
#include "graphics/overlay.hpp"
#include "memory/buffer.hpp"
#include "skyland/scene/module.hpp"
#include "userContext.hpp"



namespace skyland
{



class FileBrowserModule : public Module<FileBrowserModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_file_browser;
    }


    static u16 icon()
    {
        return 1000;
    }


    static bool run_scripts()
    {
        return false;
    }


    static bool enable_custom_scripts()
    {
        // IMPORTANT!!!!!!
        // This can NEVER BE SET TO TRUE, EVER.
        //
        // If a user were allowed to run custom scripts before entering the file
        // browser, they could brick the system, if a script raised a fatal
        // error, they wouldn't be able to get into the file browser to fix
        // it. You would have to run the factory reset module and lose all of
        // your data.
        //
        // Currently, run_scripts() returns false, so this module does not
        // invoke any scripts before running anyway. But just to future-proof
        // this stuff, I want to very explicitly define the return value here to
        // false.
        return false;
    }


    FileBrowserModule() = default;
    FileBrowserModule(UserContext&& user_context,
                      const char* path,
                      bool is_rom_path = false);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    Optional<Function<8, void(const char*)>> on_select_;

    bool gui_mode_ = false;

    void repaint();

    using CwdName = StringBuffer<30>;
    using CwdNames = Vector<CwdName>;
    CwdNames& get_cwd_names()
    {
        return **cwd_names_;
    }

    StringBuffer<200> cwd() const;


    StringBuffer<200> select_entry(int opt, bool visit);


    void backout();


    void on_dir_changed();


    enum SelectedFilesystem {
        none,
        sram,
        rom,
    } selected_filesystem_ = SelectedFilesystem::none;


private:
    Buffer<Text, 15> lines_;
    Optional<Text> info_;

    int line_offset_ = 0;

    bool faded_ = false;


    UserContext user_context_;

    Optional<DynamicMemory<CwdNames>> cwd_names_;

    enum Mode {
        browse,
        options,
    } mode_ = Mode::browse;

    int scroll_index_ = 0;

    u8 opt_index_ = 0;

    void show_opts();


    static const int max_folder_name = 30;
    static const int max_path_nesting = 12;

    using PathBuffer = Buffer<StringBuffer<max_folder_name>, max_path_nesting>;
    Optional<DynamicMemory<PathBuffer>> path_;

    static Factory factory_;
};



StringBuffer<16> get_extension(const StringBuffer<200>& cwd);



} // namespace skyland
