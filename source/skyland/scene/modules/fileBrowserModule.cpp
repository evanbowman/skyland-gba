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


#include "fileBrowserModule.hpp"
#include "hexViewerModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/scene/createFileScene.hpp"
#include "skyland/scene/paintScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"
#include "textEditorModule.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



static const auto highlight_colors =
    FontColors{custom_color(0x000010), ColorConstant::aerospace_orange};



FileBrowserModule::FileBrowserModule(UserContext&& user_context,
                                     const char* path,
                                     bool is_rom_path)
    : user_context_(std::move(user_context))
{
    path_ = allocate_dynamic<PathBuffer>("fs-path-buffer");

    StringBuffer<max_folder_name> temp;
    u32 path_len = strlen(path);

    for (u32 i = 0; i < path_len; ++i) {
        temp.push_back(path[i]);

        if (path[i] == '/') {
            (*path_)->emplace_back(temp);
            temp.clear();
        }
    }

    if (is_rom_path) {
        selected_filesystem_ = SelectedFilesystem::rom;
    } else {
        selected_filesystem_ = SelectedFilesystem::sram;
    }
}



void FileBrowserModule::enter(Scene& prev)
{
    PLATFORM.load_overlay_texture("overlay_editor");

    cwd_names_ = allocate_dynamic<CwdNames>("fs-cwd-names");

    if (not path_) {
        path_ = allocate_dynamic<PathBuffer>("fs-path-buffer");
        (*path_)->push_back("/");
    }

    PLATFORM.screen().schedule_fade(0.95f, custom_color(0x2e3440));
    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    PLATFORM.fill_overlay(0);

    repaint();
}



void FileBrowserModule::exit(Scene& next)
{
    lines_.clear();
    info_.reset();
    PLATFORM.screen().schedule_fade(1, custom_color(0x000010));
    PLATFORM.screen().clear();
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().display();

    if (user_context_.browser_exit_scene_) {
        PLATFORM.screen().schedule_fade(0);
    }
}


StringBuffer<200> FileBrowserModule::cwd() const
{
    StringBuffer<200> cwd;
    for (u32 i = 0; i < (*path_)->size(); ++i) {
        cwd += (**path_)[i];
    }

    return cwd;
}


void FileBrowserModule::repaint()
{
    // Cover text with black during transition
    PLATFORM.screen().fade(1.f, custom_color(0x2e3440), {}, true, true);
    faded_ = true;

    // If we clear all the lines, the engine will deallocate all of the tile
    // glyphs from vram, and they'll need to be reloaded, which may result in
    // some graphical artifacts. So we want to refresh existing lines instead,
    // and near the end of this function, clean up any unused text lines.
    u32 line_count = 0;
    auto enq_line = [&](const char* text) {
        if (lines_.size() > line_count) {
            lines_[line_count++].assign(text);
        } else {
            lines_.emplace_back(text, OverlayCoord{2, (u8)(lines_.size() + 3)});
            ++line_count;
        }
    };

    // lines_.clear();
    info_.reset();

    (*cwd_names_)->clear();

    for (int y = 1; y < 20; ++y) {
        PLATFORM.set_tile(Layer::overlay, 1, y, 0);
    }


    StringBuffer<200> path;
    for (u32 i = 0; i < (*path_)->size(); ++i) {
        if (i == 1 and selected_filesystem_ == SelectedFilesystem::sram) {
            path += "flash/";
        } else if (i == 1 and selected_filesystem_ == SelectedFilesystem::rom) {
            path += "rom/";
        }
        path += (**path_)[i];
    }

    if ((*path_)->size() == 1 and
        selected_filesystem_ == SelectedFilesystem::sram) {
        path += "flash/";
    } else if ((*path_)->size() == 1 and
               selected_filesystem_ == SelectedFilesystem::rom) {
        path += "rom/";
    }

    auto cwd = this->cwd();

    auto folders = allocate_dynamic<PathBuffer>("fs-folders-buffer");



    int skip = line_offset_;

    auto walk_fs = [&](const char* path) {
        auto path_len = strlen(path);
        if (path_len < cwd.length()) {
            return;
        }

        u32 i = 0;
        for (i = 0; i < cwd.length(); ++i) {
            if (path[i] not_eq cwd[i]) {
                return;
            }
        }

        for (u32 j = i; j < path_len; ++j) {
            if (path[j] == '/') {
                StringBuffer<max_folder_name> subfolder;
                for (u32 k = i; k <= j; ++k) {
                    subfolder.push_back(path[k]);
                }

                for (auto& folder : *folders) {
                    if (folder == subfolder.c_str()) {
                        // We've already queued this folder.
                        return;
                    }
                }

                folders->emplace_back(subfolder);

                (*cwd_names_)->push_back(subfolder.c_str());

                // subfolder.pop_back(); // Do not display the trailing '/'.

                if (skip > 0) {
                    --skip;
                } else {
                    enq_line(subfolder.c_str());
                }
                return;
            }
        }

        (*cwd_names_)->push_back(path + i);

        if (skip > 0) {
            --skip;
        } else {
            enq_line(path + i);
        }
    };


    switch (selected_filesystem_) {
    case SelectedFilesystem::none:
        enq_line("flash/");
        enq_line("rom/");
        enq_line("*syslog*");
        (*cwd_names_)->push_back("flash/");
        (*cwd_names_)->push_back("rom/");
        (*cwd_names_)->push_back("*syslog*");
        break;


    case SelectedFilesystem::sram: {

        flash_filesystem::walk(walk_fs);

        auto stats = flash_filesystem::statistics();
        info_.emplace(OverlayCoord{0, 19});
        info_->append("used: ");
        info_->append(stats.bytes_used_);
        info_->append("/");
        info_->append(stats.bytes_available_ + stats.bytes_used_);
        info_->append(" bytes");
        break;
    }

    case SelectedFilesystem::rom:
        auto cwd = this->cwd();

        auto folders = allocate_dynamic<PathBuffer>("fs-folders-buffer");

        PLATFORM.walk_filesystem(walk_fs);

        break;
    }


    while (path.length() < 28) {
        path.push_back(' ');
    }

    int path_scroll = (path.length() > 28) ? path.length() - 28 : 0;
    auto str = path.c_str() + path_scroll;

    for (u32 i = 0; *str not_eq '\0'; ++i, ++str) {
        auto mapping_info = locale_texture_map()(*str);
        const u16 t = PLATFORM.map_glyph(*str, *mapping_info);

        PLATFORM.set_tile(
            i + 1,
            0,
            t,
            FontColors{custom_color(0x000010), custom_color(0xffffff)});
    }

    PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 113);

    PLATFORM.set_tile(Layer::overlay, 0, 0, 114);
    PLATFORM.set_tile(Layer::overlay, 29, 0, 115);

    while (line_count < lines_.size()) {
        lines_.pop_back();
    }

    if (lines_.size() > (u32)scroll_index_) {
        lines_[scroll_index_].assign(
            (**cwd_names_)[scroll_index_ + line_offset_].c_str(),
            highlight_colors);
    }
}



StringBuffer<16> get_extension(const StringBuffer<200>& cwd)
{
    StringBuffer<16> result;

    bool found_extension = false;

    for (auto c : cwd) {
        if (c == '.') {
            if (not result.empty()) {
                result.clear();
            }
            found_extension = true;
        }

        if (found_extension) {
            result.push_back(c);
        }
    }

    return result;
}



TextEditorModule::SyntaxMode file_edit_mode(const StringBuffer<200>& path)
{
    auto ext = get_extension(path);
    if (ext == ".lisp") {
        return TextEditorModule::SyntaxMode::lisp;
    } else if (ext == ".ini") {
        return TextEditorModule::SyntaxMode::ini;
    } else if (ext == ".py") {
        return TextEditorModule::SyntaxMode::python;
    } else {
        return TextEditorModule::SyntaxMode::plain_text;
    }
}



void FileBrowserModule::show_opts()
{
    info_->assign("file: ");

    auto highlight_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};

    if (opt_index_ == 0) {
        info_->append("create", highlight_colors);
    } else {
        info_->append("create");
    }
    info_->append(" ");


    if (opt_index_ == 1) {
        info_->append("delete", highlight_colors);
    } else {
        info_->append("delete");
    }
    info_->append(" ");


    if (opt_index_ == 2) {
        info_->append("cancel", highlight_colors);
    } else {
        info_->append("cancel");
    }
    info_->append(" ");
}



ScenePtr<Scene> FileBrowserModule::update(Microseconds delta)
{
    if (faded_) {
        faded_ = false;
        PLATFORM.screen().fade(
            0.95f,
            custom_color(0x2e3440)); // Reset the fade parameters

        // Black background behind the text.
        PLATFORM.screen().fade(1.f, custom_color(0x2e3440));
    }

    auto on_dir_changed = [&] {
        scroll_index_ = 0;
        line_offset_ = 0;
    };

    auto scroll_down = [&] {
        if (scroll_index_ == 14 and
            scroll_index_ + line_offset_ < (int)(*cwd_names_)->size() - 1) {
            PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            ++line_offset_;
            repaint();
            PLATFORM.speaker().play_sound("click_wooden", 2);
        } else if (scroll_index_ + line_offset_ <
                   (int)(*cwd_names_)->size() - 1) {
            PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            if (lines_.size() > (u32)scroll_index_) {
                lines_[scroll_index_].assign(
                    (**cwd_names_)[scroll_index_ + line_offset_].c_str());
            }
            ++scroll_index_;
            if (lines_.size() > (u32)scroll_index_) {
                lines_[scroll_index_].assign(
                    (**cwd_names_)[scroll_index_ + line_offset_].c_str(),
                    highlight_colors);
            }
            PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 113);
            PLATFORM.speaker().play_sound("click_wooden", 2);
        }
    };

    auto scroll_up = [&] {
        if (scroll_index_ == 0 and line_offset_ > 0) {
            PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            --line_offset_;
            repaint();
            PLATFORM.speaker().play_sound("click_wooden", 2);
        } else if (scroll_index_ > 0) {
            PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            if (lines_.size() > (u32)scroll_index_) {
                lines_[scroll_index_].assign(
                    (**cwd_names_)[scroll_index_ + line_offset_].c_str());
            }
            --scroll_index_;
            if (lines_.size() > (u32)scroll_index_) {
                lines_[scroll_index_].assign(
                    (**cwd_names_)[scroll_index_ + line_offset_].c_str(),
                    highlight_colors);
            }
            PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 113);
            PLATFORM.speaker().play_sound("click_wooden", 2);
        }
    };

    if (mode_ == Mode::options) {
        if (APP.player().key_down(Key::left) and opt_index_ > 0) {
            --opt_index_;
            show_opts();
        } else if (APP.player().key_down(Key::right) and opt_index_ < 2) {
            ++opt_index_;
            show_opts();
        } else if (APP.player().key_down(Key::action_2)) {
            mode_ = Mode::browse;
            repaint();
        } else if (APP.player().key_down(Key::action_1)) {
            switch (opt_index_) {
            case 0: // create
                return scene_pool::alloc<CreateFileScene>(cwd().c_str());
                break;

            case 1: // delete
                if ((**cwd_names_).size() not_eq 0) {
                    auto selected = (**cwd_names_)[scroll_index_];
                    if (selected[selected.length() - 1] == '/') {
                        // Do not allow deletion of a directory...
                    } else {
                        auto path = this->cwd();
                        path += selected;
                        flash_filesystem::unlink_file(path.c_str());
                        on_dir_changed();
                        PLATFORM.speaker().play_sound("button_wooden", 3);
                    }
                }
                break;

            case 2: // cancel
                break;
            }
            mode_ = Mode::browse;
            repaint();
        }
        return null_scene();
    }

    switch (selected_filesystem_) {
    case SelectedFilesystem::none:
        if (APP.player().key_down(Key::up)) {
            scroll_up();
        } else if (APP.player().key_down(Key::down) and scroll_index_ < 2) {
            scroll_down();
        } else if (APP.player().key_down(Key::action_1)) {
            switch (scroll_index_) {
            case 0:
                on_dir_changed();
                selected_filesystem_ = SelectedFilesystem::sram;
                repaint();
                PLATFORM.speaker().play_sound("button_wooden", 3);
                break;

            case 1:
                on_dir_changed();
                selected_filesystem_ = SelectedFilesystem::rom;
                repaint();
                PLATFORM.speaker().play_sound("button_wooden", 3);
                break;

            case 2:
                return scene_pool::alloc<TextEditorModule>(
                    std::move(user_context_));
            }
        } else if (APP.player().key_down(Key::action_2)) {
            if (user_context_.browser_exit_scene_) {
                return (*user_context_.browser_exit_scene_)();
            }
            return scene_pool::alloc<TitleScreenScene>(3);
        }
        break;

    case SelectedFilesystem::sram:
        if (APP.player().key_down(Key::action_2)) {
            on_dir_changed();
            if ((*path_)->size() == 1) {
                selected_filesystem_ = SelectedFilesystem::none;
                repaint();
            } else {
                (*path_)->pop_back();
                repaint();
            }
        } else if (APP.player().key_down(Key::up)) {
            scroll_up();
        } else if (APP.player().key_down(Key::down) and
                   scroll_index_ < (int)(*cwd_names_)->size() - 1) {
            scroll_down();
        } else if (APP.player().key_down(Key::action_1)) {
            if ((**cwd_names_).size() not_eq 0) {
                PLATFORM.speaker().play_sound("button_wooden", 3);
                auto selected = (**cwd_names_)[scroll_index_];
                if (selected[selected.length() - 1] == '/') {
                    on_dir_changed();
                    (*path_)->emplace_back(selected);
                    repaint();
                } else {
                    auto path = this->cwd();
                    path += selected;

                    if (get_extension(path) == ".dat") {
                        return scene_pool::alloc<HexViewerModule>(

                            std::move(user_context_), path.c_str(), false);
                    } else if (get_extension(path) == ".img") {
                        return scene_pool::alloc<PaintScene>(path.c_str(),
                                                             false);
                    } else {
                        return scene_pool::alloc<TextEditorModule>(

                            std::move(user_context_),
                            path.c_str(),
                            file_edit_mode(path));
                    }
                }
            }
        } else if (APP.player().key_down(Key::start) or
                   APP.player().key_down(Key::select)) {
            mode_ = Mode::options;
            opt_index_ = 0;
            show_opts();
        }
        break;

    case SelectedFilesystem::rom:
        if (APP.player().key_down(Key::action_2)) {
            on_dir_changed();
            if ((*path_)->size() == 1) {
                selected_filesystem_ = SelectedFilesystem::none;
                repaint();
            } else {
                (*path_)->pop_back();
                repaint();
            }
        } else if (APP.player().key_down(Key::up)) {
            scroll_up();
        } else if (APP.player().key_down(Key::down) and
                   scroll_index_ < (int)(*cwd_names_)->size() - 1) {
            scroll_down();
        } else if (APP.player().key_down(Key::action_1)) {
            if ((**cwd_names_).size() not_eq 0) {
                int entry = scroll_index_ + line_offset_;
                auto selected = (**cwd_names_)[entry];
                if (selected[selected.length() - 1] == '/') {
                    on_dir_changed();
                    (*path_)->emplace_back(selected);
                    repaint();
                } else {
                    auto path = this->cwd();
                    path += selected;

                    return scene_pool::alloc<TextEditorModule>(

                        std::move(user_context_),
                        path.c_str(),
                        file_edit_mode(path),
                        TextEditorModule::FileMode::update,
                        TextEditorModule::FileSystem::rom);
                }
            }
        }
        break;
    }

    return null_scene();
}



void FileBrowserModule::display()
{
}



FileBrowserModule::Factory FileBrowserModule::factory_(true);



} // namespace skyland
