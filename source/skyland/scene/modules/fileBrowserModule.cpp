////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
    FontColors{custom_color(0x007cbf), custom_color(0xffffff)};



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
    if (not gui_mode_) {
        PLATFORM.load_overlay_texture("overlay_editor");
    }

    cwd_names_ = allocate_dynamic<CwdNames>("fs-cwd-names");

    if (not path_) {
        path_ = allocate_dynamic<PathBuffer>("fs-path-buffer");
        (*path_)->push_back("/");
    }

    if (not gui_mode_) {
        PLATFORM.screen().schedule_fade(0.95f, custom_color(0x007cbf));
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.fill_overlay(0);
    }

    repaint();
}



void FileBrowserModule::exit(Scene& next)
{
    lines_.clear();
    info_.reset();
    PLATFORM.screen().schedule_fade(1, custom_color(0x007cbf));
    PLATFORM.screen().clear();
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().display();

    if (user_context_.browser_exit_scene_) {
        if (next.cast_world_scene() or next.cast_macrocosm_scene()) {
            PLATFORM.load_overlay_texture("overlay");
            PLATFORM.screen().schedule_fade(1);
            PLATFORM.screen().schedule_fade(0);
        }
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
    if (not gui_mode_) {
        PLATFORM.screen().fade(1.f, custom_color(0x007cbf), {}, true, true);
        // Cover text with black during transition
        faded_ = true;
    }

    u8 y_offset = 3;

    if (gui_mode_) {
        y_offset = 4;
        for (int x = 0; x < 30; ++x) {
            for (int y = 4; y < 17; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 82);
            }
        }
    }

    // If we clear all the lines, the engine will deallocate all of the tile
    // glyphs from vram, and they'll need to be reloaded, which may result in
    // some graphical artifacts. So we want to refresh existing lines instead,
    // and near the end of this function, clean up any unused text lines.
    u32 line_count = 0;
    auto enq_line = [&](const char* text) {
        if (lines_.size() > line_count) {
            lines_[line_count++].assign(text);
        } else {
            lines_.emplace_back(
                text, OverlayCoord{2, (u8)(lines_.size() + y_offset)});
            ++line_count;
        }
    };

    // lines_.clear();
    info_.reset();

    (*cwd_names_)->clear();

    if (not gui_mode_) {
        for (int y = 1; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 0);
        }
    }


    StringBuffer<200> path;
    for (u32 i = 0; i < (*path_)->size(); ++i) {
        if (i < user_context_.hide_path_) {
            continue;
        }
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
                    if (not gui_mode_) {
                        enq_line(subfolder.c_str());
                    }
                }
                return;
            }
        }

        (*cwd_names_)->push_back(path + i);

        if (skip > 0) {
            --skip;
        } else {
            if (not gui_mode_) {
                enq_line(path + i);
            }
        }
    };


    switch (selected_filesystem_) {
    case SelectedFilesystem::none:
        if (not gui_mode_) {
            enq_line("flash/");
            enq_line("rom/");
            enq_line("*syslog*");
        }
        (*cwd_names_)->push_back("flash/");
        (*cwd_names_)->push_back("rom/");
        (*cwd_names_)->push_back("*syslog*");
        break;


    case SelectedFilesystem::sram: {

        flash_filesystem::walk(walk_fs);

        auto stats = flash_filesystem::statistics();
        u8 y = 19;
        if (gui_mode_) {
            y = 16;
        }
        info_.emplace(OverlayCoord{0, y});
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

    if (gui_mode_) {
        return;
    }

    if (not gui_mode_) {
        while (path.length() < 28) {
            path.push_back(' ');
        }

        int path_scroll = (path.length() > 28) ? path.length() - 28 : 0;
        auto str = path.c_str() + path_scroll;

        for (u32 i = 0; *str not_eq '\0'; ++i, ++str) {
            auto mapping_info = locale_texture_map()(*str);
            const u16 t = PLATFORM.map_glyph(*str, *mapping_info);

            PLATFORM.set_tile(i + 1, 0, t, highlight_colors);
        }

        PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 113);

        PLATFORM.set_tile(Layer::overlay, 0, 0, 114);
        PLATFORM.set_tile(Layer::overlay, 29, 0, 115);
    }

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
        FontColors{custom_color(0x007cbf), custom_color(0xffffff)};

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



void FileBrowserModule::on_dir_changed()
{
    scroll_index_ = 0;
    line_offset_ = 0;
}



ScenePtr FileBrowserModule::update(Time delta)
{
    if (faded_) {
        faded_ = false;
        PLATFORM.screen().fade(
            0.95f,
            custom_color(0x007cbf)); // Reset the fade parameters

        // Black background behind the text.
        PLATFORM.screen().fade(1.f, custom_color(0x007cbf));
    }

    auto scroll_down = [&] {
        if (scroll_index_ == 14 and
            scroll_index_ + line_offset_ < (int)(*cwd_names_)->size() - 1) {
            if (not gui_mode_) {
                PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            }
            ++line_offset_;
            repaint();
            PLATFORM.speaker().play_sound("click_wooden", 2);
        } else if (scroll_index_ + line_offset_ <
                   (int)(*cwd_names_)->size() - 1) {
            if (not gui_mode_) {
                PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            }

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
            if (not gui_mode_) {
                PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 113);
            }
            PLATFORM.speaker().play_sound("click_wooden", 2);
        }
    };

    auto scroll_up = [&] {
        if (scroll_index_ == 0 and line_offset_ > 0) {
            if (not gui_mode_) {
                PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            }
            --line_offset_;
            repaint();
            PLATFORM.speaker().play_sound("click_wooden", 2);
        } else if (scroll_index_ > 0) {
            if (not gui_mode_) {
                PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 0);
            }
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
            if (not gui_mode_) {
                PLATFORM.set_tile(Layer::overlay, 1, 3 + scroll_index_, 113);
            }
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
                return make_scene<CreateFileScene>(cwd().c_str());
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
        if (not gui_mode_ and APP.player().key_down(Key::up)) {
            scroll_up();
        } else if (not gui_mode_ and APP.player().key_down(Key::down) and
                   scroll_index_ < 2) {
            scroll_down();
        } else if (not gui_mode_ and APP.player().key_down(Key::action_1)) {
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
                return make_scene<TextEditorModule>(std::move(user_context_));
            }
        } else if (not gui_mode_ and APP.player().key_down(Key::action_2)) {
            if (user_context_.browser_exit_scene_) {
                return (*user_context_.browser_exit_scene_)();
            }
            return make_scene<TitleScreenScene>(3);
        }
        break;

    case SelectedFilesystem::sram:
        if (not gui_mode_ and APP.player().key_down(Key::action_2)) {
            on_dir_changed();
            if ((*path_)->size() == 1) {
                selected_filesystem_ = SelectedFilesystem::none;
                repaint();
            } else {
                (*path_)->pop_back();
                repaint();
            }
        } else if (not gui_mode_ and APP.player().key_down(Key::up)) {
            scroll_up();
        } else if (not gui_mode_ and APP.player().key_down(Key::down) and
                   scroll_index_ < (int)(*cwd_names_)->size() - 1) {
            scroll_down();
        } else if (not gui_mode_ and APP.player().key_down(Key::action_1)) {
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

                    if (on_select_) {
                        PLATFORM.speaker().play_sound("button_wooden", 3);
                        (*on_select_)(path.c_str());
                        if (user_context_.browser_exit_scene_) {
                            return (*user_context_.browser_exit_scene_)();
                        }
                    } else if (get_extension(path) == ".dat") {
                        return make_scene<HexViewerModule>(

                            std::move(user_context_), path.c_str(), false);
                    } else if (get_extension(path) == ".img") {
                        return make_scene<PaintScene>(path.c_str(), false);
                    } else {
                        return make_scene<TextEditorModule>(

                            std::move(user_context_),
                            path.c_str(),
                            file_edit_mode(path));
                    }
                }
            }
        } else if (not gui_mode_ and (APP.player().key_down(Key::start) or
                                      APP.player().key_down(Key::select))) {
            mode_ = Mode::options;
            opt_index_ = 0;
            show_opts();
        }
        break;

    case SelectedFilesystem::rom:
        if (not gui_mode_ and APP.player().key_down(Key::action_2)) {
            if (not user_context_.allow_backtrack_) {
                if (user_context_.browser_exit_scene_) {
                    return (*user_context_.browser_exit_scene_)();
                }
            } else {
                on_dir_changed();
                if ((*path_)->size() == 1) {
                    selected_filesystem_ = SelectedFilesystem::none;
                    repaint();
                } else {
                    (*path_)->pop_back();
                    repaint();
                }
            }
        } else if (not gui_mode_ and APP.player().key_down(Key::up)) {
            scroll_up();
        } else if (not gui_mode_ and APP.player().key_down(Key::down) and
                   scroll_index_ < (int)(*cwd_names_)->size() - 1) {
            scroll_down();
        } else if (not gui_mode_ and APP.player().key_down(Key::action_1)) {
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

                    if (on_select_) {
                        PLATFORM.speaker().play_sound("button_wooden", 3);
                        (*on_select_)(path.c_str());
                        if (user_context_.browser_exit_scene_) {
                            return (*user_context_.browser_exit_scene_)();
                        }
                    }

                    if (get_extension(path) == ".exe") {

                    } else if (get_extension(path) == ".photo") {
                    }

                    return make_scene<TextEditorModule>(

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



void FileBrowserModule::backout()
{
    if (not gui_mode_) {
        PLATFORM.fatal("gui mode only!");
    }

    switch (selected_filesystem_) {
    case SelectedFilesystem::none:
        break;

    case SelectedFilesystem::sram:
        on_dir_changed();
        if ((*path_)->size() == 1) {
            selected_filesystem_ = SelectedFilesystem::none;
        } else {
            (*path_)->pop_back();
        }
        break;

    case SelectedFilesystem::rom:
        on_dir_changed();
        if ((*path_)->size() == 1) {
            selected_filesystem_ = SelectedFilesystem::none;
        } else {
            (*path_)->pop_back();
        }
        break;
    }
}



StringBuffer<200> FileBrowserModule::select_entry(int opt, bool visit)
{
    if (not gui_mode_) {
        PLATFORM.fatal("gui mode only!");
    }

    auto on_dir_changed = [&] {
        scroll_index_ = 0;
        line_offset_ = 0;
    };

    switch (selected_filesystem_) {
    case SelectedFilesystem::none: {
        switch (opt) {
        case 0:
            if (visit) {
                on_dir_changed();
                selected_filesystem_ = SelectedFilesystem::sram;
            }
            return "";

        case 1:
            if (visit) {
                on_dir_changed();
                selected_filesystem_ = SelectedFilesystem::rom;
            }
            return "";

        case 2:
            return "*syslog*";
        }
        break;
    }

    case SelectedFilesystem::sram:
        if ((**cwd_names_).size() not_eq 0) {
            auto selected = (**cwd_names_)[opt];
            if (selected[selected.length() - 1] == '/') {
                if (visit) {
                    on_dir_changed();
                    (*path_)->emplace_back(selected);
                }
                return "";
            } else {
                auto path = this->cwd();
                path += selected;
                return path;
            }
        }
        break;

    case SelectedFilesystem::rom:
        if ((**cwd_names_).size() not_eq 0) {
            int entry = opt + line_offset_;
            auto selected = (**cwd_names_)[entry];
            if (selected[selected.length() - 1] == '/') {
                if (visit) {
                    on_dir_changed();
                    (*path_)->emplace_back(selected);
                }
                return "";
            } else {
                auto path = this->cwd();
                path += selected;
                return path;
            }
        }
        break;
    }


    return "";
}



void FileBrowserModule::display()
{
}



FileBrowserModule::Factory FileBrowserModule::factory_(true);



} // namespace skyland
