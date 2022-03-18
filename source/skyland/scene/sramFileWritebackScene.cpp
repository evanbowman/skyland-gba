#include "sramFileWritebackScene.hpp"
#include "modules/fileBrowserModule.hpp"
#include "platform/ram_filesystem.hpp"



namespace skyland
{



SramFileWritebackScene::SramFileWritebackScene(const char* path,
                                               Vector<char>&& text_buffer,
                                               UserContext&& user_context)
    : path_(path), text_buffer_(std::move(text_buffer)),
      user_context_(std::move(user_context))
{
}



ScenePtr<Scene>
SramFileWritebackScene::update(Platform& pfrm, App&, Microseconds delta)
{
    ram_filesystem::store_file_data(pfrm, path_.c_str(), text_buffer_);

    return scene_pool::alloc<FileBrowserModule>(
        std::move(user_context_), path_.c_str(), true);
}



void SramFileWritebackScene::enter(Platform&, App&, Scene& prev)
{
}



void SramFileWritebackScene::exit(Platform&, App&, Scene& next)
{
}



} // namespace skyland
