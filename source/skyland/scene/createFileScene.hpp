#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland {



class CreateFileScene : public Scene {
public:
    CreateFileScene(const char* ram_file_path);


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    void render_keyboard(Platform& pfrm);


private:
    StringBuffer<64> file_path_;
    Vec2<int> keyboard_cursor_;

    std::optional<Text> title_text_;
    std::optional<Text> entry_;

    StringBuffer<28> path_;
};



} // namespace skyland
