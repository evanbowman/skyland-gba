#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class WorldMap;



class WorldMapScene : public Scene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev_scene) override;


    void exit(Platform&, App&, Scene& next_scene) override;


    void display(Platform&, App&) override;


private:
    void update_tree(Platform&, WorldMap& map);


    void show_map(Platform&, WorldMap& map);


    void show_move_arrows(Platform&, App&);


    enum class State {
        fade_in,
        deselected,
        explore_paths,
        move,
        wait,
        save_selected,
        save_button_depressed,
        save_button_released_wait,
        // flag_selected,
        // flag_button_depressed,
        // flag_button_released_wait,
        fade_out,
        fade_out_saved,
        print_saved_text,
        show_saved_text,
    } state_ = State::fade_in;

    bool move_arrow_sel_[3] = {false, true, false};

    Vec2<u8> cursor_ = {0, 1};
    u8 cursor_keyframe_ = 0;
    Microseconds cursor_anim_timer_ = 0;
    Microseconds timer_ = 0;
    ColorMix cmix_;

    std::optional<Text> heading_;
    std::optional<Text> key_[3];
    std::optional<MediumIcon> save_icon_;
    std::optional<MediumIcon> flag_icon_;
};



} // namespace skyland
