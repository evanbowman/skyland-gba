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


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


private:
    Buffer<Text, 15> lines_;
    Optional<Text> info_;

    int line_offset_ = 0;

    bool faded_ = false;


    UserContext user_context_;


    StringBuffer<200> cwd() const;

    static const int max_display_files_per_folder = 62;

    using CwdNames = Buffer<StringBuffer<20>, max_display_files_per_folder>;
    Optional<DynamicMemory<CwdNames>> cwd_names_;

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
    Optional<DynamicMemory<PathBuffer>> path_;

    static Factory factory_;
};



StringBuffer<16> get_extension(const StringBuffer<200>& cwd);



} // namespace skyland
