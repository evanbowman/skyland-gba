#include "dlcInjectorModule.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "vector.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/dlc.hpp"



namespace skyland {



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

        Vector<char> file_contents(pfrm);
        for (int i = 0; i < desc->file_size_.get(); ++i) {
            if (it == data.end()) {
                Platform::fatal("dlc archive file contents cropped!");
            }
            file_contents.push_back(*it);
            ++it;
        }
        file_contents.push_back('\0');

        ram_filesystem::store_file_data(pfrm, file_name.c_str(), file_contents);
    }
}



ScenePtr<Scene>
DlcInjectorModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    Module::update(pfrm, app, delta);

    Vector<char> result(pfrm);

    pfrm.system_call("dlc-download", &result);

    if (result.size() not_eq 0) {
        store_dlc(pfrm, result);
    }

    return scene_pool::alloc<TitleScreenScene>(3);
}



// NOTE: uncomment when we actually implement this module.
DlcInjectorModule::Factory DlcInjectorModule::factory_;


} // namespace skyland
