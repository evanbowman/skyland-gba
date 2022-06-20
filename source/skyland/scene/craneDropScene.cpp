////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "craneDropScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "readyScene.hpp"
#include "skyland/rooms/crane.hpp"



namespace skyland
{



void init_clouds(Platform& pfrm);



class CraneFadeinScene : public WorldScene
{
public:

    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        pfrm.load_sprite_texture("spritesheet");


        show_island_exterior(pfrm, app, &app.player_island());

        if (app.opponent_island()) {
            app.opponent_island()->repaint(pfrm, app);
        }

        init_clouds(pfrm);

        WorldScene::enter(pfrm, app, prev);
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta;


        constexpr auto fade_duration = milliseconds(1400);
        constexpr auto fade_start = milliseconds(400);

        if (app.game_speed() not_eq GameSpeed::normal) {
            set_gamespeed(pfrm, app, GameSpeed::normal);
        }

        if (timer_ < fade_start) {

            // ...

        } else if (timer_ > fade_duration) {

            WorldScene::update(pfrm, app, delta);

            pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);

            pfrm.screen().schedule_fade(0.f);
            return scene_pool::alloc<ReadyScene>();

        } else {

            WorldScene::update(pfrm, app, delta);


            const auto amount = smoothstep(0.f,
                                           fade_duration - fade_start,
                                           timer_ - fade_start);

            pfrm.speaker().set_music_volume(6 + 14 * amount);

            pfrm.screen().schedule_fade(1.f - amount);
        }

        return null_scene();
    }

private:
    Microseconds timer_ = 0;
};



class FishingMinigameScene : public Scene
{
public:


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        // ...
    }


    ScenePtr<Scene> update(Platform& pfrm,
                           App& app,
                           Microseconds delta) override
    {
        pfrm.screen().schedule_fade(1.f);
        return scene_pool::alloc<CraneFadeinScene>();
    }
};




static void draw_anchor(Platform& pfrm, const Vec2<Fixnum>& offset)
{
    Sprite spr;

    spr.set_size(Sprite::Size::w32_h32);
    spr.set_texture_index(6);
    spr.set_position({offset.x, offset.y});
    spr.set_origin({16, 16});

    pfrm.screen().draw(spr);

    auto pos = offset;
    while (pos.y > -16) {
        spr.set_texture_index(22);
        spr.set_size(Sprite::Size::w16_h32);
        spr.set_position({offset.x + 8, pos.y - 28});
        pfrm.screen().draw(spr);

        pos.y -= 30;
    }
}



class CraneDropCinematicScene : public Scene
{
public:

    CraneDropCinematicScene() :
        data_(allocate_dynamic<Data>("crane-drop-data"))
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        pfrm.load_sprite_texture("spritesheet_fishing");
        pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                pfrm.set_tile(Layer::map_1_ext, x, y, 0);
                pfrm.set_tile(Layer::map_0_ext, x, y, 0);
            }
        }

        for (int x = 0; x < 32; ++x) {
            for (int y = 0; y < 32; ++y) {
                pfrm.set_tile(Layer::background, x, y, 4);
            }
        }

        pfrm.screen().set_view({});

        rng::utility_state = 1;
    }


    ScenePtr<Scene> update(Platform& pfrm,
                           App& app,
                           Microseconds delta) override
    {
        if (fadein_timer_ < milliseconds(1000)) {
            fadein_timer_ += delta;
            const auto amount = smoothstep(0.f,
                                           milliseconds(1000),
                                           fadein_timer_);

            pfrm.screen().schedule_fade(1.f - amount, ColorConstant::silver_white);
        }

        timer_ += delta;
        if (timer_ > seconds(7)) {
            return scene_pool::alloc<FishingMinigameScene>();
        }

        if (timer_ < seconds(4)) {
            cloud_timer_ += delta;
            if (cloud_timer_ > cloud_respawn_) {
                cloud_respawn_ += milliseconds(10);
                cloud_timer_ = 0;
                Vec2<Fixnum> pos;
                pos.x = -80 + rng::choice<240 + 40>(rng::utility_state);
                pos.y = 180;
                data_->clouds_.push_back({pos, (u8)rng::choice<2>(rng::utility_state)});
            }
        }

        anchor_offset_ += Fixnum(0.00001f) * delta;


        for (auto& cloud : data_->clouds_) {
            switch (cloud.graphics_) {
            case 0:
                cloud.position_.y -= Fixnum(0.00015f) * delta;
                break;

            case 1:
                cloud.position_.y -= Fixnum(0.00024f) * delta;
                break;
            }
        }

        for (auto it = data_->clouds_.begin(); it not_eq data_->clouds_.end();) {
            if (it->position_.y < -24) {
                it = data_->clouds_.erase(it);
            } else {
                ++it;
            }
        }

        return null_scene();
    }


    void display(Platform& pfrm, App& app) override
    {
        draw_anchor(pfrm, {120, anchor_offset_});

        Sprite spr;

        for (auto& cloud : data_->clouds_) {
            switch (cloud.graphics_) {
            case 0:
                spr.set_texture_index(10);
                spr.set_position(cloud.position_);
                pfrm.screen().draw(spr);
                break;

            case 1: {
                auto pos = cloud.position_;
                spr.set_texture_index(7);
                spr.set_position(pos);
                pfrm.screen().draw(spr);

                pos.x += 32;
                spr.set_texture_index(8);
                spr.set_position(pos);
                pfrm.screen().draw(spr);

                pos.x += 32;
                spr.set_texture_index(9);
                spr.set_position(pos);
                pfrm.screen().draw(spr);
                break;
            }
            }
        }
    }


private:
    Microseconds fadein_timer_ = 0;
    Microseconds timer_ = 0;
    Microseconds cloud_respawn_ = milliseconds(40);
    Fixnum anchor_offset_ = 20;

    struct CloudInfo {
        Vec2<Fixnum> position_;
        u8 graphics_;
    };

    struct Data {
        Buffer<CloudInfo, 18> clouds_;
    };
    DynamicMemory<Data> data_;

    Microseconds cloud_timer_;
};



ScenePtr<Scene>
CraneDropScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    if (timer_ == 0) {
        app.set_coins(pfrm, app.coins() - 2000);
    }

    timer_ += delta;

    if (app.game_speed() not_eq GameSpeed::normal) {
        set_gamespeed(pfrm, app, GameSpeed::normal);
    }

    constexpr auto fade_duration = milliseconds(1400);
    constexpr auto fade_start = milliseconds(400);

    if (timer_ < fade_start) {

        // ...

    } else if (timer_ > fade_duration) {

        if (auto room = app.player_island().get_room(crane_pos_)) {
            if (auto crane = dynamic_cast<Crane*>(room)) {
                crane->retract();
            }
        }

        return scene_pool::alloc<CraneDropCinematicScene>();

    } else {
        const auto amount = smoothstep(0.f,
                                       fade_duration - fade_start,
                                       timer_ - fade_start);

        pfrm.speaker().set_music_volume(20 - 14 * amount);

        pfrm.screen().schedule_fade(amount);
    }

    return null_scene();
}



} // namespace skyland
