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

    struct Pong {
        struct Pad {
            Float speed_;
            Float pos_;
        };

        Pad pad1_ = {0.2f, 0};
        Pad pad2_ = {0.2f, 10};

        Vec2<Float> ball_ = {3, 3};
        Vec2<Float> ball_speed_ = {0.35f, 0.35f};

        void update();
        void display(Platform&, int x_scroll);
    } pong_;


    void window_image_hack(Platform&, u16 empty_tile);

    enum class State {
        fade_in,
        wait,
        fade_out,
        scroll_right,
        scroll_left,
        scroll_multiplayer,
        scroll_to_center,
        wait_2,
        scroll_to_end,
        scroll_from_end,
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
