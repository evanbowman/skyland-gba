////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "dlcManagerModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void DlcManagerModule::enter(Scene& prev)
{
    patches_ = allocate_dynamic<PatchList>("dlc-patch-list");

    auto on_match = [&](const char* const path) {
        const char* p = path;
        PatchName patch_name;
        while (*p not_eq '\0' and *p not_eq '/') {
            patch_name.push_back(*(p++));
        }

        for (auto& patch : (*patches_)->list_) {
            if (patch_name == patch.name_.c_str()) {
                return;
            }
        }

        StringBuffer<flash_filesystem::max_path> patch_info_path;
        patch_info_path += "/dlc/";
        patch_info_path += patch_name;
        patch_info_path += "/meta.lisp";

        u8 tile_deps = 0;
        u8 spr_deps = 0;

        auto v = APP.invoke_ram_script(patch_info_path.c_str());
        if (v->type() == lisp::Value::Type::cons) {
            lisp::l_foreach(v, [&](lisp::Value* kvp) {
                if (kvp->type() not_eq lisp::Value::Type::cons) {
                    return;
                }

                auto car = kvp->cons().car();
                auto cdr = kvp->cons().cdr();
                if (car->type() not_eq lisp::Value::Type::symbol or
                    cdr->type() not_eq lisp::Value::Type::integer) {
                    return;
                }

                if (str_eq(car->symbol().name(), "tile")) {
                    tile_deps = cdr->integer().value_;
                } else if (str_eq(car->symbol().name(), "spr")) {
                    spr_deps = cdr->integer().value_;
                }
            });
        } else {
            Platform::fatal("dlc meta.lisp returns incorrect datatype");
        }


        (*patches_)->list_.emplace_back(
            PatchInfo{patch_name, tile_deps, spr_deps});
    };

    flash_filesystem::walk_directory("/dlc/", on_match);

    PLATFORM.screen().fade(0.95);
    PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);

    show();
}



void DlcManagerModule::exit(Scene& next)
{
    patch_name_.reset();
    tiles_text_.reset();
    sprites_text_.reset();

    total_tiles_.reset();
    total_sprites_.reset();

    total_tiles_label_.reset();
    total_sprites_label_.reset();
}



void DlcManagerModule::show()
{
    static const auto heading_colors =
        Text::OptColors{{ColorConstant::silver_white, custom_color(0x193a77)}};


    static const auto alt_colors =
        Text::OptColors{{ColorConstant::silver_white, custom_color(0x8988b8)}};


    static const auto text_colors =
        Text::OptColors{{custom_color(0x193a77), ColorConstant::silver_white}};

    static const auto text_colors_alt =
        Text::OptColors{{custom_color(0x8988b8), ColorConstant::silver_white}};



    patch_name_.emplace(OverlayCoord{1, 1});

    StringBuffer<30> heading;

    patch_name_->append("package : < ", heading_colors);

    patch_name_->append((*patches_)->list_[index_].name_.c_str(),
                        heading_colors);

    patch_name_->append(" >", heading_colors);


    tiles_text_.emplace(OverlayCoord{1, 4});
    tiles_text_->append("tiles: ", text_colors);
    for (int i = 0; i < (*patches_)->list_[index_].tiles_used_; ++i) {
        tiles_text_->append(" ", text_colors);
        tiles_text_->append(" ", heading_colors);
    }

    sprites_text_.emplace(OverlayCoord{1, 6});
    sprites_text_->append("sprites: ", text_colors);
    for (int i = 0; i < (*patches_)->list_[index_].sprites_used_; ++i) {
        sprites_text_->append(" ", text_colors);
        sprites_text_->append(" ", heading_colors);
    }


    u8 total_tiles_used = 0;
    u8 total_sprites_used = 0;
    for (auto& info : (*patches_)->list_) {
        total_tiles_used += info.tiles_used_;
        total_sprites_used += info.sprites_used_;
    }

    total_tiles_label_.emplace(OverlayCoord{1, 14});
    total_tiles_label_->assign("total tiles", text_colors_alt);
    total_tiles_.emplace(OverlayCoord{1, 15});

    for (int i = 0; i < total_tiles_used; ++i) {
        total_tiles_->append(" ", heading_colors);
        total_tiles_->append(" ", text_colors);
    }

    for (int i = total_tiles_used; i < 15; ++i) {
        total_tiles_->append(" ", alt_colors);
        total_tiles_->append(" ", text_colors);
    }

    total_sprites_label_.emplace(OverlayCoord{1, 17});
    total_sprites_label_->assign("total sprites", text_colors_alt);
    total_sprites_.emplace(OverlayCoord{1, 18});

    for (int i = 0; i < total_sprites_used; ++i) {
        total_sprites_->append(" ", heading_colors);
        total_sprites_->append(" ", text_colors);
    }

    for (int i = total_sprites_used; i < 15; ++i) {
        total_sprites_->append(" ", alt_colors);
        total_sprites_->append(" ", text_colors);
    }
}



ScenePtr DlcManagerModule::update(Time delta)
{
    APP.player().update(delta);

    if ((*patches_)->list_.empty()) {
        auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
        *buffer = SYSTR(no_dlc_prompt)->c_str();
        return make_scene<FullscreenDialogScene>(
            std::move(buffer), [] { return make_scene<TitleScreenScene>(3); });
    }

    if (APP.player().key_down(Key::right) and
        index_ < (*patches_)->list_.size() - 1) {
        ++index_;

        erase_text_.reset();

        show();

    } else if (APP.player().key_down(Key::left) and index_ > 0) {
        --index_;

        erase_text_.reset();

        show();
    }

    if (APP.player().key_down(Key::action_1)) {
        if (not erase_text_) {
            erase_text_.emplace(OverlayCoord{7, 10});
            erase_text_->assign(
                SYSTR(dlc_erase_hint)->c_str(),
                Text::OptColors{{ColorConstant::silver_white,
                                 ColorConstant::spanish_crimson}});
        }
    }

    if (APP.player().key_held(Key::action_1, seconds(2))) {
        StringBuffer<flash_filesystem::max_path> folder("/dlc/");
        folder += (*patches_)->list_[index_].name_.c_str();

        auto destroy_file = [&](const char* const path) {
            StringBuffer<flash_filesystem::max_path> path_str(path);
            // FIXME: bug??!! path pointer seems to be completely wiped out
            // after this line. I've investigated this over and over again, and
            // I just can't make any sense of it. Seeing as I'm passing a lambda
            // to another lambda to a function template to another function
            // template, I do wonder if there may be an obscure compiler bug, as
            // I've been using this StringBuffer<> class for years, and I've
            // never seen any issues like this. I'm always inclined to blame
            // myself whenever the code breaks, as compiler bugs are so rare,
            // but I'm just stumped on this one.
            StringBuffer<flash_filesystem::max_path> full_path;
            full_path += folder;
            full_path += path_str;
            flash_filesystem::unlink_file(full_path.c_str());
        };

        flash_filesystem::walk_directory(folder.c_str(), destroy_file);

        APP.player().key_held_reset(Key::action_1, seconds(2));

        return make_scene<DlcManagerModule>();
    }

    if (APP.player().key_down(Key::action_2)) {
        return make_scene<TitleScreenScene>(3);
    }

    return null_scene();
}



DlcManagerModule::Factory DlcManagerModule::factory_(true);



} // namespace skyland
