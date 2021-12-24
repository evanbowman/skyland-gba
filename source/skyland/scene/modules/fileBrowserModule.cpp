#include "fileBrowserModule.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/skyland.hpp"
#include "localization.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "textEditorModule.hpp"
#include "skyland/scene/createFileScene.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



FileBrowserModule::FileBrowserModule(Platform& pfrm, const char* path)
{
    path_ = allocate_dynamic<PathBuffer>(pfrm);

    StringBuffer<max_folder_name> temp;
    u32 path_len = str_len(path);

    for (u32 i = 0; i < path_len; ++i) {
        temp.push_back(path[i]);

        if (path[i] == '/') {
            (*path_)->push_back(temp);
            temp.clear();
        }
    }

    selected_filesystem_ = SelectedFilesystem::sram;
}



void FileBrowserModule::enter(Platform& pfrm, App&, Scene& prev)
{
    cwd_names_ = allocate_dynamic<CwdNames>(pfrm);

    if (not path_) {
        path_ = allocate_dynamic<PathBuffer>(pfrm);
        (*path_)->push_back("/");
    }

    pfrm.screen().fade(0.f);

    pfrm.fill_overlay(112);

    repaint(pfrm);
}



void FileBrowserModule::exit(Platform& pfrm, App&, Scene& next)
{
    lines_.clear();
    info_.reset();
    pfrm.fill_overlay(0);
}


StringBuffer<200> FileBrowserModule::cwd() const
{
    StringBuffer<200> cwd;
    for (u32 i = 0; i < (*path_)->size(); ++i) {
        cwd += (**path_)[i];
    }

    return cwd;
}


void FileBrowserModule::repaint(Platform& pfrm)
{
    // Cover text with black during transition
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
    faded_ = true;

    lines_.clear();
    info_.reset();

    (*cwd_names_)->clear();

    for (int y = 1; y < 20; ++y) {
        pfrm.set_tile(Layer::overlay, 1, y, 112);
    }


    StringBuffer<200> path;
    for (u32 i = 0; i < (*path_)->size(); ++i) {
        if (i == 1 and selected_filesystem_ == SelectedFilesystem::sram) {
            path += "sram/";
        } else if (i == 1 and selected_filesystem_ == SelectedFilesystem::rom) {
            path += "rom/";
        }
        path += (**path_)[i];
    }

    if ((*path_)->size() == 1 and
        selected_filesystem_ == SelectedFilesystem::sram) {
        path += "sram/";
    } else if ((*path_)->size() == 1 and
               selected_filesystem_ == SelectedFilesystem::rom) {
        path += "rom/";
    }

    auto cwd = this->cwd();

    auto folders = allocate_dynamic<PathBuffer>(pfrm);


    auto walk_fs = [&](const char* path) {
        auto path_len = str_len(path);
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

                folders->push_back(subfolder);

                (*cwd_names_)->push_back(subfolder.c_str());

                subfolder.pop_back(); // Do not display the trailing '/'.

                lines_.emplace_back(pfrm,
                                    subfolder.c_str(),
                                    OverlayCoord{2, (u8)(lines_.size() + 3)});
                return;
            }
        }

        (*cwd_names_)->push_back(path + i);

        lines_.emplace_back(pfrm,
                            path + i,
                            OverlayCoord{2, (u8)(lines_.size() + 3)});
    };


    switch (selected_filesystem_) {
    case SelectedFilesystem::none:
        lines_.emplace_back(pfrm,
                            "sram",
                            OverlayCoord{2, (u8)(lines_.size() + 3)});

        lines_.emplace_back(pfrm,
                            "rom",
                            OverlayCoord{2, (u8)(lines_.size() + 3)});
        break;


    case SelectedFilesystem::sram: {

        ram_filesystem::walk(pfrm, walk_fs);

        auto stats = ram_filesystem::statistics(pfrm);
        info_.emplace(pfrm, OverlayCoord{0, 19});
        info_->append("used: ");
        info_->append(stats.blocks_used_ * ram_filesystem::block_size);
        info_->append("/");
        info_->append((stats.blocks_available_ + stats.blocks_used_)
                       * ram_filesystem::block_size);
        info_->append(" bytes");
        break;
    }

    case SelectedFilesystem::rom:
        auto cwd = this->cwd();

        auto folders = allocate_dynamic<PathBuffer>(pfrm);

        pfrm.walk_filesystem(walk_fs);

        break;
    }


    while (path.length() < 28) {
        path.push_back(' ');
    }

    for (u32 i = 0; i < path.length(); ++i) {
        auto mapping_info = locale_texture_map()(path[i]);
            const u16 t = pfrm.map_glyph(path[i], *mapping_info);

        pfrm.set_tile(i + 1, 0, t, FontColors{
            custom_color(0x000010), custom_color(0xffffff)
        });
    }

    pfrm.set_tile(Layer::overlay, 1, 3, 475);

    pfrm.set_tile(Layer::overlay, 0, 0, 401);
    pfrm.set_tile(Layer::overlay, 29, 0, 411);
}



static StringBuffer<16> get_extension(const StringBuffer<200>& cwd)
{
    StringBuffer<16> result;

    bool found_extension = false;

    for (auto c : cwd) {
        if (c == '.') {
            found_extension = true;
        }

        if (found_extension) {
            result.push_back(c);
        }
    }

    return result;
}



void FileBrowserModule::show_opts(Platform& pfrm)
{
    info_->assign("file: ");

    auto highlight_colors =
        FontColors{
            custom_color(0x000010), custom_color(0xffffff)
        };

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



ScenePtr<Scene> FileBrowserModule::update(Platform& pfrm,
                                          App& app,
                                          Microseconds delta)
{
    if (faded_) {
        faded_ = false;
        pfrm.screen().fade(0.f); // Reset the fade parameters

        // Black background behind the text.
        pfrm.screen().fade(1.f);
    }

    auto scroll_down = [&] {
        pfrm.set_tile(Layer::overlay, 1, 3 + scroll_index_, 112);
        ++scroll_index_;
        pfrm.set_tile(Layer::overlay, 1, 3 + scroll_index_, 475);
    };

    auto scroll_up = [&] {
        pfrm.set_tile(Layer::overlay, 1, 3 + scroll_index_, 112);
        --scroll_index_;
        pfrm.set_tile(Layer::overlay, 1, 3 + scroll_index_, 475);
    };

    if (mode_ == Mode::options) {
        if (app.player().key_down(pfrm, Key::left) and opt_index_ > 0) {
            --opt_index_;
            show_opts(pfrm);
        } else if (app.player().key_down(pfrm, Key::right) and opt_index_ < 2) {
            ++opt_index_;
            show_opts(pfrm);
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            mode_ = Mode::browse;
            repaint(pfrm);
        } else if (app.player().key_down(pfrm, Key::action_1)) {
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
                        ram_filesystem::unlink_file(pfrm, path.c_str());
                        scroll_index_ = 0;
                    }
                }
                break;

            case 2: // cancel
                break;
            }
            mode_ = Mode::browse;
            repaint(pfrm);
        }
        return null_scene();
    }

    switch (selected_filesystem_) {
    case SelectedFilesystem::none:
        if (app.player().key_down(pfrm, Key::up) and scroll_index_ > 0) {
            scroll_up();
        } else if (app.player().key_down(pfrm, Key::down) and scroll_index_ == 0) {
            scroll_down();
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            switch (scroll_index_) {
            case 0:
                scroll_index_ = 0;
                selected_filesystem_ = SelectedFilesystem::sram;
                repaint(pfrm);
                break;

            case 1:
                scroll_index_ = 0;
                selected_filesystem_ = SelectedFilesystem::rom;
                repaint(pfrm);
                break;
            }
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<TitleScreenScene>();
        }
        break;

    case SelectedFilesystem::sram:
        if (app.player().key_down(pfrm, Key::action_2)) {
            scroll_index_ = 0;
            if ((*path_)->size() == 1) {
                selected_filesystem_ = SelectedFilesystem::none;
                repaint(pfrm);
            } else {
                (*path_)->pop_back();
                repaint(pfrm);
            }
        } else if (app.player().key_down(pfrm, Key::up) and
                   scroll_index_ > 0) {
            scroll_up();
        } else if (app.player().key_down(pfrm, Key::down) and
                   scroll_index_ < (int)(*cwd_names_)->size() - 1) {
            scroll_down();
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            if ((**cwd_names_).size() not_eq 0) {
                auto selected = (**cwd_names_)[scroll_index_];
                if (selected[selected.length() - 1] == '/') {
                    scroll_index_ = 0;
                    (*path_)->push_back(selected);
                    repaint(pfrm);
                } else {
                    auto path = this->cwd();
                    path += selected;

                    const auto ext = get_extension(path);

                    if (ext == ".lisp") {
                        return scene_pool::alloc<TextEditorModule>(pfrm, path.c_str());
                    }
                }
            }
        } else if (app.player().key_down(pfrm, Key::start) or
                   app.player().key_down(pfrm, Key::select)) {
            mode_ = Mode::options;
            opt_index_ = 0;
            show_opts(pfrm);
        }
        break;

    case SelectedFilesystem::rom:
        if (app.player().key_down(pfrm, Key::action_2)) {
            scroll_index_ = 0;
            if ((*path_)->size() == 1) {
                selected_filesystem_ = SelectedFilesystem::none;
                repaint(pfrm);
            } else {
                (*path_)->pop_back();
                repaint(pfrm);
            }
        } else if (app.player().key_down(pfrm, Key::up) and
                   scroll_index_ > 0) {
            scroll_up();
        } else if (app.player().key_down(pfrm, Key::down) and
                   scroll_index_ < (int)(*cwd_names_)->size() - 1) {
            scroll_down();
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            if ((**cwd_names_).size() not_eq 0) {
                auto selected = (**cwd_names_)[scroll_index_];
                if (selected[selected.length() - 1] == '/') {
                    scroll_index_ = 0;
                    (*path_)->push_back(selected);
                    repaint(pfrm);
                } else {
                    auto path = this->cwd();
                    path += selected;
                    // TODO...
                }
            }
        }
        break;
    }

    return null_scene();
}



void FileBrowserModule::display(Platform& pfrm, App&)
{
}



FileBrowserModule::Factory FileBrowserModule::factory_;



}
