#include "fileBrowserModule.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/skyland.hpp"
#include "localization.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "textEditorModule.hpp"



namespace skyland {



void FileBrowserModule::enter(Platform& pfrm, App&, Scene& prev)
{
    path_ = allocate_dynamic<PathBuffer>(pfrm);
    cwd_names_ = allocate_dynamic<CwdNames>(pfrm);

    (*path_)->push_back("/");

    pfrm.screen().fade(0.f);

    pfrm.fill_overlay(112);

    repaint(pfrm);
}



void FileBrowserModule::exit(Platform&, App&, Scene& next)
{
    lines_.clear();
    info_.reset();
    path_text_.reset();
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
    lines_.clear();
    info_.reset();

    (*cwd_names_)->clear();

    path_text_.reset();

    pfrm.fill_overlay(112);

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

    path_text_.emplace(pfrm, OverlayCoord{1, 0});
    path_text_->append(path.c_str(), FontColors{
            custom_color(0x000010), custom_color(0xffffff)
        });

    pfrm.set_tile(Layer::overlay, 1, 3, 475);

    pfrm.set_tile(Layer::overlay, 0, 0, 401);
    pfrm.set_tile(Layer::overlay, 29, 0, 411);
}


ScenePtr<Scene> FileBrowserModule::update(Platform& pfrm,
                                          App& app,
                                          Microseconds delta)
{
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

                    return scene_pool::alloc<TextEditorModule>(pfrm, path.c_str());

                    // ram_filesystem::unlink_file(pfrm, path.c_str());
                    // scroll_index_ = 0;
                    // repaint(pfrm);
                }
            }
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



void FileBrowserModule::display(Platform&, App&)
{

}



FileBrowserModule::Factory FileBrowserModule::factory_;



}
