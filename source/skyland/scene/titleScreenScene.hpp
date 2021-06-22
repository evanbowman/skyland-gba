#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class TitleScreenScene : public Scene {
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    bool wait_ = true;
    std::optional<Text> text_;
    Microseconds timer_ = 0;
    Microseconds hover_timer_ = 0;
    Microseconds selector_timer_ = 0;
    bool selector_shaded_ = false;

    void window_image_hack(Platform&);

    enum class State {
        fade_in,
        wait,
        fade_out,
        scroll_right,
        scroll_left,
        wait_2,
    } state_ = State::fade_in;

    int menu_selection_ = 0;

    void put_menu_text(Platform&);
    void redraw_margins(Platform&);

    int menu_selection_start_ = 0;
    int menu_selection_stop_ = 0;

    int x_scroll_ = 0;

    Microseconds island_mov_timer_ = 0;
    int island_offset_ = 0;
    Microseconds birb_timer_ = seconds(7);
};



} // namespace skyland
