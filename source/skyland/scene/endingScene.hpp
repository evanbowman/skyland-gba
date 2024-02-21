#pragma once

#include "skyland/scene.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class EndingScene : public Scene
{
public:
    std::optional<DeferredScene> next_;
    Time timer_ = 0;
    int state_ = 0;
    const char* msg_str_ = nullptr;
    StringBuffer<48> msg_data_;
    int msg_pos_ = 0;
    bool flip_ = false;


    ScenePtr<Scene> update(Time delta) override
    {

        switch (state_) {
        case 0:
            msg_data_ = SYS_CSTR(the_end);
            msg_str_ = msg_data_.c_str();

            PLATFORM.screen().set_shader(passthrough_shader);
            PLATFORM.set_scroll(Layer::map_1_ext, 0, 0);
            PLATFORM.load_tile1_texture("/scripts/misc/img/ending.img.bin");
            PLATFORM.load_overlay_texture("overlay_ending");
            __draw_image(0, 3, 3, 24, 10, Layer::map_1);
            PLATFORM.screen().set_view(View{});
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 16; ++y) {
                    PLATFORM.set_tile(Layer::map_0_ext, x, y, 0);
                }
            }

            PLATFORM.screen().schedule_fade(
                1, ColorConstant::rich_black, true, true);

            PLATFORM.screen().clear();
            PLATFORM.screen().display();

            PLATFORM.fill_overlay(0);

            for (int x = 0; x < 30; ++x) {
                for (int y = 0; y < 20; ++y) {
                    if (y < 3 or y > 12 or x < 3 or x > 26) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 83);
                    }
                }
            }

            state_ = 1;
            break;

        case 1: {
            constexpr auto fade_duration = milliseconds(900);
            timer_ += delta;
            if (timer_ > fade_duration) {
                PLATFORM.screen().schedule_fade(0);
                state_ = 2;
                timer_ = milliseconds(750);
            } else {
                const auto amount =
                    1.f - smoothstep(0.f, fade_duration, timer_);
                PLATFORM.screen().schedule_fade(
                    amount, ColorConstant::rich_black, true, true);
            }

            break;
        }

        case 2: {
            timer_ += delta;
            auto delay = seconds(1);
            if (*msg_str_ == '.') {
                delay = milliseconds(500);
            } else if (*msg_str_ == ' ' or *msg_str_ == '\0') {
                delay = milliseconds(80);
            }
            if (timer_ > delay) {
                timer_ = 0;
                if (*msg_str_ == '\0') {
                    state_ = 3;
                    timer_ = 0;
                } else {
                    timer_ = 0;
                    char tmp[2];
                    tmp[0] = *msg_str_;
                    tmp[1] = '\0';
                    Text::print(tmp, {u8(9 + msg_pos_++), 15});
                    if (*msg_str_ not_eq ' ') {
                        PLATFORM.speaker().play_sound("msg", 5);
                    }
                    ++msg_str_;
                }
            }
            break;
        }

        case 3:
            timer_ += delta;
            if (timer_ > milliseconds(500)) {
                timer_ = 0;

                int x = 9 + msg_pos_;
                if (flip_) {
                    PLATFORM.set_tile(Layer::overlay, x, 15, 85);
                } else {
                    PLATFORM.set_tile(Layer::overlay, x, 15, 84);
                }

                flip_ = not flip_;
            }
            if (PLATFORM.keyboard().pressed<Key::action_1>() or
                PLATFORM.keyboard().pressed<Key::action_2>()) {
                state_ = 4;
                PLATFORM.set_tile(Layer::overlay, 9 + msg_pos_, 15, 83);
                timer_ = 0;
            }
            break;

        case 4: {
            constexpr auto fade_duration = milliseconds(600);
            timer_ += delta;
            if (timer_ > fade_duration) {
                state_ = 5;
            } else {
                const auto amount = smoothstep(0.f, fade_duration, timer_);
                PLATFORM.screen().schedule_fade(
                    amount, custom_color(0xf9f8f0), true, true);
            }

            break;
        }

        case 5: {
            if (next_) {
                PLATFORM.screen().schedule_fade(
                    1, custom_color(0xf9f8f0), true, true);
                PLATFORM.fill_overlay(0);
                PLATFORM.screen().clear();
                PLATFORM.screen().display();
                return (*next_)();
            } else {
                LOGIC_ERROR();
            }
            break;
        }
        }

        return null_scene();
    }
};



} // namespace skyland
