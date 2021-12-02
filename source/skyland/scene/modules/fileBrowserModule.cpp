#include "fileBrowserModule.hpp"
#include "platform/ram_filesystem.hpp"



namespace skyland {



void FileBrowserModule::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.screen().fade(0.f);

    ram_filesystem::walk(pfrm, [&](const char* path) {
        lines_.emplace_back(pfrm,
                            path,
                            OverlayCoord{1, (u8)lines_.size()});
    });


    auto stats = ram_filesystem::statistics(pfrm);
    info_.emplace(pfrm, OverlayCoord{0, 19});
    info_->append("used blocks: ");
    info_->append(stats.blocks_used_);
    info_->append("/");
    info_->append(stats.blocks_available_);
}



void FileBrowserModule::exit(Platform&, App&, Scene& next)
{

}



ScenePtr<Scene> FileBrowserModule::update(Platform&, App&, Microseconds delta)
{
    return null_scene();
}



void FileBrowserModule::display(Platform&, App&)
{

}



FileBrowserModule::Factory FileBrowserModule::factory_;



}
