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
#include "readyScene.hpp"
#include "skyland/rooms/crane.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



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


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
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

            pfrm.speaker().set_music_volume(
                Platform::Speaker::music_volume_max);

            pfrm.screen().schedule_fade(0.f);
            return scene_pool::alloc<ReadyScene>();

        } else {

            WorldScene::update(pfrm, app, delta);


            const auto amount = smoothstep(
                0.f, fade_duration - fade_start, timer_ - fade_start);

            pfrm.speaker().set_music_volume(6 + 14 * amount);

            pfrm.screen().schedule_fade(1.f - amount);
        }

        return null_scene();
    }

private:
    Microseconds timer_ = 0;
};



static void
draw_crane(Platform& pfrm, const Vec2<Fixnum>& offset, u16 image = 6)
{
    Sprite spr;

    spr.set_size(Sprite::Size::w32_h32);
    spr.set_texture_index(image);
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



class FishingMinigameScene : public Scene
{
public:
    FishingMinigameScene(const Vec2<Fixnum>& crane_pos)
        : crane_pos_(crane_pos),
          level_(allocate_dynamic<Level>("fishing-level-data"))
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        Level::Fish fish;
        fish.type_ = 0;
        fish.x_.set(Fixnum(30.f).data());
        fish.y_.set(100);
        level_->fish_.push_back(fish);

        fish.y_.set(300);
        fish.x_.set(Fixnum(50.f).data());
        level_->fish_.push_back(fish);

        fish.y_.set(500);
        fish.x_.set(Fixnum(70.f).data());
        level_->fish_.push_back(fish);

        fish.y_.set(580);
        fish.x_.set(Fixnum(120.f).data());
        level_->fish_.push_back(fish);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (app.player().key_down(pfrm, Key::action_2)) {
            pfrm.screen().schedule_fade(1.f);
            return scene_pool::alloc<CraneFadeinScene>();
        }

        if (got_fish_) {
            if (crane_pos_.y < 120) {
                crane_pos_.y += Fixnum(0.00003f) * delta;
                depth_ -= Fixnum(0.00006f) * delta;
            } else {
                depth_ -= Fixnum(0.00009f) * delta;
            }
            if (depth_ < -32) {
                pfrm.screen().fade(1.f);
                return scene_pool::alloc<CraneFadeinScene>();
            }
        } else {
            if (crane_pos_.y > 30) {
                crane_pos_.y -= Fixnum(0.00003f) * delta;
            }
            depth_ += Fixnum(0.00005f) * delta;
        }


        if (app.player().key_pressed(pfrm, Key::right)) {
            crane_pos_.x += Fixnum(0.00007f) * delta;
        }

        if (app.player().key_pressed(pfrm, Key::left)) {
            crane_pos_.x -= Fixnum(0.00007f) * delta;
        }

        HitBox crane_hb;
        crane_hb.position_ = &crane_pos_;
        crane_hb.dimension_.size_ = {24, 24};
        crane_hb.dimension_.origin_ = {16, 16};

        bool seen_fish = false;

        for (auto it = level_->fish_.begin(); it not_eq level_->fish_.end();) {
            auto& fish = *it;

            auto screen_y = fish.y_.get() - depth_;
            if (screen_y < -24 or screen_y > 180) {
                if (seen_fish) {
                    break;
                } else {
                    ++it;
                    continue;
                }
            }
            seen_fish = true;

            Vec2<Fixnum> fish_pos;
            fish_pos.y = (fish.y_.get() - depth_);
            fish_pos.x = Fixnum::create(fish.x_.get());

            HitBox fish_hb;
            fish_hb.position_ = &fish_pos;
            fish_hb.dimension_.size_ = {24, 10};
            fish_hb.dimension_.origin_ = {16, 16};

            if (fish_hb.overlapping(crane_hb)) {
                got_fish_ = true;

                Level::CaughtFish c;
                c.type_ = fish.type_;
                c.crane_x_offset_ =
                    rng::sample<4>((fish_pos.x - crane_pos_.x).as_integer(),
                                   rng::utility_state);
                c.crane_y_offset_ =
                    rng::sample<4>((fish_pos.y - crane_pos_.y).as_integer(),
                                   rng::utility_state);
                level_->caught_fish_.push_back(c);

                it = level_->fish_.erase(it);
            } else {
                ++it;
            }
        }

        return null_scene();
    }


    void display(Platform& pfrm, App& app) override
    {
        Sprite spr;

        for (auto& fish : level_->fish_) {
            auto y = fish.y_.get() - depth_;
            if (y < -24 or y > 180) {
                continue;
            }

            auto x = Fixnum::create(fish.x_.get());

            spr.set_origin({16, 16});
            spr.set_texture_index(12);
            spr.set_position({x, y});
            pfrm.screen().draw(spr);
        }

        spr.set_mix({ColorConstant::electric_blue, 100});
        for (auto& fish : level_->caught_fish_) {
            spr.set_texture_index(12);
            spr.set_origin({16, 16});
            spr.set_position({crane_pos_.x + fish.crane_x_offset_ / 2,
                              crane_pos_.y + fish.crane_y_offset_ / 2});
            pfrm.screen().draw(spr);
        }

        draw_crane(pfrm, crane_pos_, got_fish_ ? 13 : 6);
    }

private:
    Vec2<Fixnum> crane_pos_;
    Fixnum depth_;
    bool got_fish_ = false;

    struct Level
    {
        struct Fish
        {
            u8 type_;
            HostInteger<u16> y_;
            HostInteger<u64> x_;
        };

        Buffer<Fish, 100> fish_;

        struct CaughtFish
        {
            u8 type_;
            s8 crane_x_offset_;
            s8 crane_y_offset_;
        };

        Buffer<CaughtFish, 10> caught_fish_;
    };

    DynamicMemory<Level> level_;
};



class CraneDropCinematicScene : public Scene
{
public:
    CraneDropCinematicScene() : data_(allocate_dynamic<Data>("crane-drop-data"))
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        pfrm.load_sprite_texture("spritesheet_fishing");
        pfrm.load_tile0_texture("tilesheet_fishing");
        pfrm.screen().schedule_fade(1.f, ColorConstant::silver_white);

        pfrm.set_scroll(Layer::map_0_ext, 0, -160);

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                pfrm.set_tile(Layer::map_1_ext, x, y, 0);
                pfrm.set_tile(Layer::map_0_ext, x, y, 0);
            }
        }

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 6; ++y) {
                if (y == 0) {
                    pfrm.set_tile(Layer::map_0_ext, x, y, 1);
                } else {
                    pfrm.set_tile(Layer::map_0_ext, x, y, 2);
                }
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


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (app.player().key_pressed(pfrm, Key::right)) {
            crane_x_ += Fixnum(0.00003f) * delta;
        }

        if (app.player().key_pressed(pfrm, Key::left)) {
            crane_x_ -= Fixnum(0.00003f) * delta;
        }

        if (timer_ < seconds(7)) {
            if (fadein_timer_ < milliseconds(1000)) {
                fadein_timer_ += delta;
                const auto amount =
                    smoothstep(0.f, milliseconds(1000), fadein_timer_);

                pfrm.screen().schedule_fade(1.f - amount,
                                            ColorConstant::silver_white);
            }

            timer_ += delta;

            if (timer_ > seconds(7)) {
                for (int x = 0; x < 16; ++x) {
                    for (int y = 1; y < 10; ++y) {
                        pfrm.set_tile(Layer::map_0_ext, x, y, 2);
                    }
                }

                pfrm.sleep(4);
            }


            if (timer_ > milliseconds(6500)) {
                auto amt =
                    Float(timer_ - milliseconds(6500)) / milliseconds(500);
                int offset = -160;
                offset += 75 * amt;
                pfrm.set_scroll(Layer::map_0_ext, 0, offset);
            }

            if (timer_ < seconds(4)) {
                cloud_timer_ += delta;
                if (cloud_timer_ > cloud_respawn_) {
                    cloud_respawn_ += milliseconds(10);
                    cloud_timer_ = 0;
                    Vec2<Fixnum> pos;
                    pos.x = -80 + rng::choice<240 + 40>(rng::utility_state);
                    pos.y = 180;
                    data_->clouds_.push_back(
                        {pos, (u8)rng::choice<2>(rng::utility_state)});
                }
            }

            crane_offset_ += Fixnum(0.00001f) * delta;


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

            for (auto it = data_->clouds_.begin();
                 it not_eq data_->clouds_.end();) {
                if (it->position_.y < -24) {
                    it = data_->clouds_.erase(it);
                } else {
                    ++it;
                }
            }
        } else {
            crane_offset_ -= Fixnum(0.000015f) * delta;

            if (timer_ < seconds(9) and timer_ + delta > seconds(9)) {
                for (int x = 0; x < 16; ++x) {
                    for (int y = 10; y < 14; ++y) {
                        pfrm.set_tile(Layer::map_0_ext, x, y, 2);
                    }
                }
            }

            timer_ += delta;
            auto amt = Float(timer_ - seconds(7)) / milliseconds(2500);

            int offset = -160 + 75;
            offset += 105 * amt;
            pfrm.set_scroll(Layer::map_0_ext, 0, offset);

            if (timer_ > milliseconds(9500)) {
                return scene_pool::alloc<FishingMinigameScene>(
                    Vec2<Fixnum>{crane_x_, crane_offset_});
            }
        }


        return null_scene();
    }


    void display(Platform& pfrm, App& app) override
    {
        draw_crane(pfrm, {crane_x_, crane_offset_});

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
    Fixnum crane_offset_ = 20;
    Fixnum crane_x_ = 120;

    struct CloudInfo
    {
        Vec2<Fixnum> position_;
        u8 graphics_;
    };

    struct Data
    {
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
        const auto amount =
            smoothstep(0.f, fade_duration - fade_start, timer_ - fade_start);

        pfrm.speaker().set_music_volume(20 - 14 * amount);

        pfrm.screen().schedule_fade(amount);
    }

    return null_scene();
}



} // namespace skyland
