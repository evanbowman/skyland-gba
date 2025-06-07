////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
Optional<T> read_dlc_struct(Vector<char>& data, Vector<char>::Iterator& it)
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



static void store_dlc(Vector<char>& data)
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

        flash_filesystem::store_file_data_binary(file_name.c_str(),
                                                 file_contents);
    }
}



ScenePtr DlcInjectorModule::update(Time delta)
{
    if (not begin_load_) {
        auto future_scene = [] { return make_scene<DlcInjectorModule>(true); };

        auto buffer = allocate_dynamic<DialogString>("dialog-buffer");

        if (PLATFORM.device_name() == "GameboyAdvance") {
            *buffer = SYSTR(misc_dlc_message)->c_str();
        } else {
            Platform::fatal(stringify(PLATFORM.device_name().length()).c_str());
            return make_scene<TitleScreenScene>(3);
        }

        return make_scene<FullscreenDialogScene>(std::move(buffer),
                                                 future_scene);
    }

    Module::update(delta);

    Vector<char> result;
    PLATFORM_EXTENSION(dlc_download, result);

    if (result.size() not_eq 0) {
        store_dlc(result);
    }

    return make_scene<TitleScreenScene>(3);
}



DlcInjectorModule::Factory DlcInjectorModule::factory_(true);



} // namespace skyland
