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



ScenePtr<Scene> DlcInjectorModule::update(Time delta)
{
    if (not begin_load_) {
        auto future_scene = [] {
            return scene_pool::alloc<DlcInjectorModule>(true);
        };

        auto buffer = allocate_dynamic<DialogString>("dialog-buffer");

        if (PLATFORM.device_name() == "GameboyAdvance") {
            *buffer = SYSTR(misc_dlc_message)->c_str();
        } else {
            Platform::fatal(stringify(PLATFORM.device_name().length()).c_str());
            return scene_pool::alloc<TitleScreenScene>(3);
        }

        return scene_pool::alloc<FullscreenDialogScene>(std::move(buffer),
                                                        future_scene);
    }

    Module::update(delta);

    Vector<char> result;

    PLATFORM.system_call("dlc-download", &result);

    if (result.size() not_eq 0) {
        store_dlc(result);
    }

    return scene_pool::alloc<TitleScreenScene>(3);
}



DlcInjectorModule::Factory DlcInjectorModule::factory_(true);



} // namespace skyland
