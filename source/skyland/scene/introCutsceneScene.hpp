////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "introCreditsScene.hpp"
#include "number/random.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class IntroCutsceneDoneScene : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.speaker().stop_music();
        PLATFORM.speaker().set_music_volume(
            Platform::Speaker::music_volume_max);
        PLATFORM.screen().schedule_fade(1.f);
    }


    ScenePtr update(Time delta)
    {
        PLATFORM.fill_overlay(0);
        return make_scene<IntroCreditsScene>();
    }
};



class IntroCutsceneSceneText3 : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.load_overlay_texture("overlay_challenges");

        PLATFORM.load_tile1_texture("intro_storm_king_flattened");

        __draw_image(1, 0, 3, 30, 14, Layer::map_1);

        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
            PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
        }
    }


    void exit(Scene& next) override
    {
        text_.reset();
    }


    ScenePtr update(Time delta) override
    {
        timer_ += delta;

        if (timer_ < milliseconds(7000)) {
            auto fade_amount =
                smoothstep(milliseconds(3000), milliseconds(5000), timer_);
            PLATFORM.screen().schedule_fade(1.f - fade_amount / 2);
        } else if (timer_ <= milliseconds(8000)) {

            auto fade_amount =
                smoothstep(milliseconds(7000), milliseconds(7900), timer_);
            PLATFORM.screen().schedule_fade(0.5f - fade_amount * 0.8f);

        } else if (timer_ > milliseconds(8000)) {
            auto fade_amount =
                smoothstep(milliseconds(8000), milliseconds(9000), timer_);
            PLATFORM.screen().schedule_fade(0.2f + 0.8f * fade_amount);
        }



        if (timer_ > seconds(1) and not text_) {
            text_.emplace();
            text_->assign(
                SYSTR(intro_sequence_message_3)->c_str(), {1, 6}, {28, 8});
            // PLATFORM.screen().schedule_fade(1.f, ColorConstant::rich_black);
        }

        if (timer_ > milliseconds(7000)) {
            text_.reset();
            PLATFORM.speaker().stop_music();
        } else {
            PLATFORM.speaker().set_music_volume(
                20 - 19 * (Float(timer_) / milliseconds(7000)));
        }

        if (timer_ > milliseconds(11000)) {
            PLATFORM.speaker().set_music_volume(
                Platform::Speaker::music_volume_max);
            return make_scene<IntroCutsceneDoneScene>();
        } else {
            if (PLATFORM.keyboard().down_transition<Key::action_2>() or
                PLATFORM.keyboard().down_transition<Key::action_1>()) {
                return make_scene<IntroCutsceneDoneScene>();
            }
        }

        return null_scene();
    }

    Optional<TextView> text_;
    Time timer_;
    bool fade_out_ = false;
};



class IntroCutsceneLaunch : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.load_tile1_texture("intro_island_launch_flattened");

        __draw_image(1, 0, 3, 30, 14, Layer::map_1);

        APP.camera()->shake(14);
    }



    void exit(Scene& next) override
    {
        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.fill_overlay(0);
    }



    void display() override
    {
        for (auto& p : snow_particles_) {
            Sprite spr;
            spr.set_size(Sprite::Size::w16_h32);
            spr.set_texture_index(13);
            spr.set_position({p.x, p.y});
            PLATFORM.screen().draw(spr);
        }


        auto amount =
            24 * smoothstep(milliseconds(-500), milliseconds(12000), timer_);

        Sprite sprite;
        sprite.set_texture_index(8);
        sprite.set_position({114, 35 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(9);
        sprite.set_position({146, 35 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(10);
        sprite.set_position({178, 35 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(11);
        sprite.set_position({114, 67 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(12);
        sprite.set_position({146, 67 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(13);
        sprite.set_position({178, 67 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(14);
        sprite.set_position({114, 99 - amount});
        PLATFORM.screen().draw(sprite);

        sprite.set_texture_index(15);
        sprite.set_position({146, 99 - amount});
        PLATFORM.screen().draw(sprite);


        // amount =
        //     48 * smoothstep(milliseconds(-500), milliseconds(12000), timer_);

        // sprite.set_texture_index(17);
        // sprite.set_position({200, 55 - amount});

        // if (timer_ > milliseconds(4000)) {
        //     sprite.set_alpha(Sprite::Alpha::translucent);
        // } else {
        //     sprite.set_mix({custom_color(0x9cb5c6),
        //                     (u8)(28 + 100 * smoothstep(milliseconds(-500),
        //                                                milliseconds(4000),
        //                                                timer_))});
        // }

        // PLATFORM.screen().draw(sprite);
    }



    ScenePtr update(Time delta) override
    {
        timer_ += delta;

        if (PLATFORM.keyboard().down_transition<Key::action_2>() or
            PLATFORM.keyboard().down_transition<Key::action_1>()) {
            return make_scene<IntroCutsceneDoneScene>();
        }

        auto fade_amount =
            smoothstep(milliseconds(4000), milliseconds(6000), timer_);
        PLATFORM.screen().schedule_fade(fade_amount);


        for (auto it = snow_particles_.begin();
             it not_eq snow_particles_.end();) {
            if (it->x > PLATFORM.screen().size().x or
                it->y > PLATFORM.screen().size().y) {
                it = snow_particles_.erase(it);
            } else {
                it->x += 0.0002f * delta;
                it->y += 0.0002f * delta;
                ++it;
            }
        }

        if (snow_particles_.size() < 16) {
            if (rng::choice<2>(rng::utility_state)) {
                Float x =
                    rng::choice(PLATFORM.screen().size().x, rng::utility_state);
                Float y = 0;
                snow_particles_.push_back({x, y});
            } else {
                Float x = 0;
                Float y =
                    rng::choice(PLATFORM.screen().size().y, rng::utility_state);
                snow_particles_.push_back({x, y});
            }
        }



        if (timer_ > milliseconds(6000)) {
            return make_scene<IntroCutsceneSceneText3>();
        }

        return null_scene();
    }


private:
    Buffer<Vec2<Float>, 16> snow_particles_;
    Time timer_ = 0;
};



class IntroCutsceneSceneText2 : public Scene
{
public:
    void enter(Scene& prev) override
    {
        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
            PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
        }
    }


    void exit(Scene& next) override
    {
        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 0);
        }
        text_.reset();
    }


    ScenePtr update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition<Key::action_2>() or
            PLATFORM.keyboard().down_transition<Key::action_1>()) {
            return make_scene<IntroCutsceneDoneScene>();
        }

        timer_ += delta;

        if (timer_ > seconds(2) and not text_) {
            text_.emplace();
            text_->assign(
                SYSTR(intro_sequence_message_2)->c_str(), {1, 6}, {28, 8});
            PLATFORM.screen().schedule_fade(1.f, ColorConstant::rich_black);
        }

        if (timer_ > milliseconds(7300)) {
            return make_scene<IntroCutsceneLaunch>();
        }

        return null_scene();
    }

    Optional<TextView> text_;
    Time timer_;
};



class IntroCutsceneScene : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.load_tile1_texture("intro_crops_snow_flattened");

        PLATFORM.load_sprite_texture("spritesheet_intro");

        __draw_image(1, 0, 3, 30, 14, Layer::map_1);

        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
            PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
        }
    }


    void exit(Scene& prev) override
    {
        // PLATFORM.fill_overlay(0);
        PLATFORM.set_scroll(Layer::map_1, 0, 0);
    }


    ScenePtr update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition<Key::action_2>() or
            PLATFORM.keyboard().down_transition<Key::action_1>()) {
            return make_scene<IntroCutsceneDoneScene>();
        }

        timer_ += delta;

        if (fade_out_) {

            auto amount = smoothstep(seconds(1), milliseconds(5000), timer_);
            PLATFORM.screen().schedule_fade(amount,
                                            ColorConstant::silver_white);

            if (timer_ > milliseconds(5000)) {
                return make_scene<IntroCutsceneSceneText2>();
            }
        } else {
            auto amount = smoothstep(0.f, milliseconds(1200), timer_);
            PLATFORM.set_scroll(Layer::map_1, 0, amount * 5);

            auto fade_amount = smoothstep(0.f, milliseconds(1200), timer_);
            PLATFORM.screen().schedule_fade(1.f - fade_amount);

            if (timer_ > milliseconds(4000)) {
                fade_out_ = true;
                timer_ = 0;
                PLATFORM.load_tile1_texture("intro_crops_snow_far_flattened");
            }
        }

        for (auto it = snow_particles_.begin();
             it not_eq snow_particles_.end();) {
            if (it->x > PLATFORM.screen().size().x or
                it->y > PLATFORM.screen().size().y) {
                it = snow_particles_.erase(it);
            } else {
                it->x += 0.0002f * delta;
                it->y += 0.0002f * delta;
                ++it;
            }
        }

        if (snow_particles_.size() < 16) {
            if (rng::choice<2>(rng::utility_state)) {
                Float x =
                    rng::choice(PLATFORM.screen().size().x, rng::utility_state);
                Float y = 0;
                snow_particles_.push_back({x, y});
            } else {
                Float x = 0;
                Float y =
                    rng::choice(PLATFORM.screen().size().y, rng::utility_state);
                snow_particles_.push_back({x, y});
            }
        }

        return null_scene();
    }


    void display() override
    {
        for (auto& p : snow_particles_) {
            Sprite spr;
            spr.set_size(Sprite::Size::w16_h32);
            spr.set_texture_index(14);
            spr.set_position({p.x, p.y});
            PLATFORM.screen().draw(spr);
        }
    }


private:
    Buffer<Vec2<Float>, 16> snow_particles_;
    Time timer_ = 0;
    bool fade_out_ = false;
};



class IntroCutsceneExplosion2 : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.load_tile1_texture("intro_explosion_2_flattened");

        __draw_image(1, 0, 3, 30, 14, Layer::map_1);

        APP.camera()->shake(14);
    }



    void exit(Scene& next) override
    {
        PLATFORM.screen().schedule_fade(1.f);
        PLATFORM.fill_overlay(0);
    }



    ScenePtr update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition<Key::action_2>() or
            PLATFORM.keyboard().down_transition<Key::action_1>()) {
            return make_scene<IntroCutsceneDoneScene>();
        }

        timer_ += delta;

        auto fade_amount = smoothstep(0.f, milliseconds(2000), timer_);
        PLATFORM.screen().schedule_fade(1.f - fade_amount,
                                        custom_color(0xf7f2ab));


        if (timer_ > milliseconds(4000)) {
            return make_scene<IntroCutsceneScene>();
        }

        return null_scene();
    }


private:
    Time timer_ = 0;
};



class IntroCutsceneExplosion1 : public Scene
{
public:
    void enter(Scene& prev) override
    {
        PLATFORM.load_tile1_texture("intro_explosion_1_flattened");

        PLATFORM.load_sprite_texture("spritesheet_intro");

        __draw_image(1, 0, 3, 30, 14, Layer::map_1);

        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
            PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
        }
    }



    void exit(Scene& next) override
    {
        PLATFORM.screen().schedule_fade(1.f);
        // PLATFORM.fill_overlay(0);
    }



    void display()
    {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);

        auto amount = smoothstep(-300, milliseconds(2800), timer_);

        spr.set_position({194 - 10 * amount, 108 * amount});
        spr.set_texture_index(15);

        PLATFORM.screen().draw(spr);
    }



    ScenePtr update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition<Key::action_2>() or
            PLATFORM.keyboard().down_transition<Key::action_1>()) {
            return make_scene<IntroCutsceneDoneScene>();
        }

        timer_ += delta;

        auto fade_amount = smoothstep(0.f, milliseconds(1700), timer_);
        PLATFORM.screen().schedule_fade(1.f - fade_amount,
                                        custom_color(0xf7f2ab));


        if (timer_ > milliseconds(2300)) {
            return make_scene<IntroCutsceneExplosion2>();
        }

        return null_scene();
    }


private:
    Time timer_ = 0;
};



class IntroCutsceneSceneText1 : public Scene
{
public:
    void enter(Scene& prev) override
    {
        text_.emplace();
        text_->assign(
            SYSTR(intro_sequence_message_1)->c_str(), {1, 6}, {28, 8});
        PLATFORM.screen().schedule_fade(1.f);
    }


    ScenePtr update(Time delta) override
    {
        if (PLATFORM.keyboard().down_transition<Key::action_2>() or
            PLATFORM.keyboard().down_transition<Key::action_1>()) {
            return make_scene<IntroCutsceneDoneScene>();
        }

        timer_ += delta;

        if (timer_ > milliseconds(5500)) {
            text_.reset();
        }

        if (timer_ > milliseconds(6500)) {
            return make_scene<IntroCutsceneExplosion1>();
        }

        return null_scene();
    }

    Optional<TextView> text_;
    Time timer_;
};



class IntroCutsceneSceneBegin : public Scene
{
public:
    void enter(Scene& prev) override
    {
        // PLATFORM.speaker().play_music("unaccompanied_wind", 0);


        const auto screen_tiles = calc_screen_tiles();
        for (int i = 0; i < screen_tiles.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 0, 112);
            PLATFORM.set_tile(Layer::overlay, i, 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
        }

        PLATFORM.speaker().stream_music("isle_of_the_dead", 0);
    }


    ScenePtr update(Time delta) override
    {
        timer_ += delta;

        // if (timer_ > milliseconds(200)) {
        //     PLATFORM.screen().schedule_fade(1.f, custom_color(0xf7f2ab));
        // }

        if (timer_ > milliseconds(800)) {
            return make_scene<IntroCutsceneSceneText1>();
        }
        return null_scene();
    }

    Time timer_ = 0;
};



} // namespace skyland
