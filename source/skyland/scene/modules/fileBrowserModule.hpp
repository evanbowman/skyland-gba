////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    void enter(App&, Scene& prev) override;
    void exit(App&, Scene& next) override;


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


private:
    Buffer<Text, 15> lines_;
    std::optional<Text> info_;

    int line_offset_ = 0;

    bool faded_ = false;


    UserContext user_context_;


    StringBuffer<200> cwd() const;

    static const int max_display_files_per_folder = 62;

    using CwdNames = Buffer<StringBuffer<20>, max_display_files_per_folder>;
    std::optional<DynamicMemory<CwdNames>> cwd_names_;

    enum SelectedFilesystem {
        none,
        sram,
        rom,
    } selected_filesystem_ = SelectedFilesystem::none;

    enum Mode {
        browse,
        options,
    } mode_ = Mode::browse;

    void repaint();

    int scroll_index_ = 0;

    u8 opt_index_ = 0;

    void show_opts();


    static const int max_folder_name = 20;
    static const int max_path_nesting = 12;

    using PathBuffer = Buffer<StringBuffer<max_folder_name>, max_path_nesting>;
    std::optional<DynamicMemory<PathBuffer>> path_;

    static Factory factory_;
};



StringBuffer<16> get_extension(const StringBuffer<200>& cwd);



} // namespace skyland
