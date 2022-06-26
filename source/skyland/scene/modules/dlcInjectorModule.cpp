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


#include "dlcInjectorModule.hpp"
#include "containers/vector.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/dlc.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



template <typename T>
std::optional<T> read_dlc_struct(Vector<char>& data, Vector<char>::Iterator& it)
{
    alignas(T) u8 buffer[sizeof(T)];

    for (u32 i = 0; i < sizeof(T); ++i) {
        if (it == data.end()) {
            return {};
        }

        buffer[i] = *it;
        ++it;
    }

    return *reinterpret_cast<T*>(buffer);
}



static void store_dlc(Platform& pfrm, Vector<char>& data)
{
    auto it = data.begin();

    auto hdr = read_dlc_struct<dlc::archive::Header>(data, it);
    if (not hdr) {
        Platform::fatal("dlc archive missing header!");
    }

    for (int i = 0; i < hdr->file_count_.get(); ++i) {
        auto desc = read_dlc_struct<dlc::archive::FileDescription>(data, it);
        if (not desc) {
            Platform::fatal("dlc archive missing file description!");
        }

        StringBuffer<64> file_name;
        for (int i = 0; i < desc->name_length_; ++i) {
            if (it == data.end()) {
                Platform::fatal("dlc archive file name cropped!");
            }
            file_name.push_back(*it);
            ++it;
        }

        Vector<char> file_contents;
        for (int i = 0; i < desc->file_size_.get(); ++i) {
            if (it == data.end()) {
                Platform::fatal("dlc archive file contents cropped!");
            }
            file_contents.push_back(*it);
            ++it;
        }

        flash_filesystem::store_file_data_binary(
            pfrm, file_name.c_str(), file_contents);
    }

    pfrm.system_call("sram-flash-writeback", nullptr);
}



ScenePtr<Scene>
DlcInjectorModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not begin_load_) {
        auto future_scene = [] {
            return scene_pool::alloc<DlcInjectorModule>(true);
        };

        auto buffer = allocate_dynamic<DialogString>("dialog-buffer");

        if (pfrm.device_name() == "GameboyAdvance") {
            *buffer = SYSTR(misc_dlc_message)->c_str();
        } else {
            Platform::fatal(stringify(pfrm.device_name().length()).c_str());
            return scene_pool::alloc<TitleScreenScene>(3);
        }

        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                        future_scene);
    }

    Module::update(pfrm, app, delta);

    Vector<char> result;

    pfrm.system_call("dlc-download", &result);

    if (result.size() not_eq 0) {
        store_dlc(pfrm, result);
    }

    return scene_pool::alloc<TitleScreenScene>(3);
}



DlcInjectorModule::Factory DlcInjectorModule::factory_(true);



} // namespace skyland
