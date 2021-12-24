#pragma once

#include "skyland/scene.hpp"
#include "string.hpp"
#include "graphics/overlay.hpp"



// The user edited a ROM file. Obviously, we cannot write the modified file back
// to ROM, but we should notify the user that we are about to save a file to the
// SRAM filesystem instead, and give him/her the option to cancel.



namespace skyland {



class SramFileWritebackScene : public Scene {
public:
    SramFileWritebackScene(const char* path,
                           ScratchBufferPtr text_buffer);


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


private:
    StringBuffer<64> path_;
    ScratchBufferPtr text_buffer_;

    std::optional<TextView> menu_text_;
};



}
