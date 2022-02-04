#include "dlcManagerModule.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



void DlcManagerModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    patches_ = allocate_dynamic<PatchList>(pfrm);

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

        StringBuffer<ram_filesystem::max_path> patch_info_path;
        patch_info_path += "/dlc/";
        patch_info_path += patch_name;
        patch_info_path += "/meta.lisp";

        u8 tile_deps = 0;
        u8 spr_deps = 0;

        auto v = app.invoke_ram_script(pfrm, patch_info_path.c_str());
        if (v->type() == lisp::Value::Type::cons) {
            lisp::foreach (v, [&](lisp::Value* kvp) {
                if (kvp->type() not_eq lisp::Value::Type::cons) {
                    return;
                }

                auto car = kvp->cons().car();
                auto cdr = kvp->cons().cdr();
                if (car->type() not_eq lisp::Value::Type::symbol or
                    cdr->type() not_eq lisp::Value::Type::integer) {
                    return;
                }

                if (str_eq(car->symbol().name_, "tile")) {
                    tile_deps = cdr->integer().value_;
                } else if (str_eq(car->symbol().name_, "spr")) {
                    spr_deps = cdr->integer().value_;
                }
            });
        } else {
            Platform::fatal("dlc meta.lisp returns incorrect datatype");
        }


        (*patches_)->list_.emplace_back(
            PatchInfo{patch_name, tile_deps, spr_deps});
    };

    ram_filesystem::walk_directory(pfrm, "/dlc/", on_match);

    pfrm.screen().fade(0.95);
    pfrm.screen().fade(1.f);

    show(pfrm);
}



void DlcManagerModule::exit(Platform& pfrm, App& app, Scene& next)
{
    patch_name_.reset();
}



void DlcManagerModule::show(Platform& pfrm)
{
    patch_name_.emplace(
        pfrm, (*patches_)->list_[index_].name_.c_str(), OverlayCoord{1, 1});

    patch_name_->append((*patches_)->list_[index_].tiles_used_);
    patch_name_->append(" ");
    patch_name_->append((*patches_)->list_[index_].sprites_used_);
}



ScenePtr<Scene>
DlcManagerModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if ((*patches_)->list_.empty()) {
        auto buffer = allocate_dynamic<DialogString>(pfrm);
        *buffer = "You don't have any DLC installed!";
        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer), [] {
            return scene_pool::alloc<TitleScreenScene>(3);
        });
    }

    if (app.player().key_down(pfrm, Key::right) and
        index_ < (*patches_)->list_.size() - 1) {
        ++index_;

        show(pfrm);

    } else if (app.player().key_down(pfrm, Key::left) and index_ > 0) {
        --index_;

        show(pfrm);
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    return null_scene();
}



DlcManagerModule::Factory DlcManagerModule::factory_;



} // namespace skyland
