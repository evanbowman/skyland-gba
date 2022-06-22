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



class CraneMinigameScene : public Scene
{
public:

    using ChunkData = u8[7][7];

    static constexpr const ChunkData init_chunk = {
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {1, 0, 0, 0, 1, 0, 1},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 1, 0, 0, 0, 1, 0},
        {0, 0, 0, 0, 0, 0, 0},
    };

    static constexpr const ChunkData sector_empty = {
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
    };

    static constexpr const ChunkData sector_0 = {
        {0, 0, 0, 0, 0, 0, 0},
        {1, 0, 0, 0, 1, 0, 0},
        {0, 1, 0, 0, 0, 0, 1},
        {0, 0, 1, 0, 0, 1, 0},
        {0, 0, 0, 0, 1, 0, 0},
        {0, 1, 0, 0, 0, 1, 0},
        {0, 0, 0, 0, 0, 0, 0},
    };

    static constexpr const ChunkData sector_1 = {
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 1, 0, 1, 1, 1, 0},
        {1, 1, 0, 1, 0, 1, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {1, 1, 1, 0, 0, 1, 1},
        {0, 0, 1, 1, 0, 0, 0},
    };

    static constexpr const ChunkData sector_2 = {
        {1, 0, 0, 0, 0, 0, 0},
        {0, 1, 0, 0, 1, 0, 0},
        {0, 0, 1, 0, 0, 1, 0},
        {0, 0, 0, 1, 0, 0, 1},
        {0, 0, 0, 0, 1, 0, 1},
        {0, 0, 0, 1, 0, 0, 0},
        {0, 0, 0, 1, 0, 1, 1},
    };

    static constexpr const ChunkData sector_3 = {
        {0, 0, 0, 0, 0, 0, 0},
        {1, 0, 1, 0, 1, 0, 1},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 1, 0, 1, 0, 1, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {1, 0, 1, 0, 1, 0, 1},
        {0, 0, 0, 0, 0, 0, 0},
    };

    static constexpr const ChunkData sector_4 = {
        {0, 0, 0, 0, 0, 0, 0},
        {1, 1, 1, 1, 1, 0, 1},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {1, 1, 0, 1, 1, 1, 1},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
    };




    CraneMinigameScene(const RoomCoord& crane_loc,
                       const Vec2<Fixnum>& crane_pos)
        : crane_loc_(crane_loc),
          crane_pos_(crane_pos),
          level_(allocate_dynamic<Level>("fishing-level-data"))
    {
    }


    static constexpr const Float spacing = 35.f;


    void load_chunk(const u8 chunk[7][7], Fixnum y_offset)
    {
        Level::Object object;
        object.type_ = 0;

        for (int x = 0; x < 7; ++x) {
            for (int y = 0; y < 7; ++y) {
                if (chunk[y][x]) {
                    object.x_.set(Fixnum(15 + x * spacing).data());
                    object.y_.set((y_offset + y * spacing).as_integer());
                    level_->objects_.push_back(object);
                }
            }
        }
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        rng::LinearGenerator seed = app.crane_game_rng();

        std::array<const ChunkData*, 5> choices = {&sector_0,
                                                   &sector_1,
                                                   &sector_2,
                                                   &sector_3,
                                                   &sector_4};

        Buffer<const ChunkData*, choices.size() * 2> pattern;
        for (auto& c : choices) {
            pattern.push_back(c);
            pattern.push_back(c);
        }
        rng::shuffle(pattern, seed);

        Buffer<const ChunkData*, 5> result;
        for (u32 i = 0; i < result.capacity(); ++i) {
            result.push_back(pattern.back());
            pattern.pop_back();
        }


        load_chunk(init_chunk, 100.f);


        int offset = 1;
        for (auto& c : result) {
            load_chunk(*c, 100.f + spacing * ((offset++) * 7));
        }

        x_speed_ = 0;
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (app.player().key_pressed(pfrm, Key::down)) {
            descent_speed_ += Fixnum(0.0000045f);
        }

        if (app.player().key_pressed(pfrm, Key::up)) {
            descent_speed_ -= Fixnum(0.0000045f);
        }


        descent_speed_ = clamp(descent_speed_,
                               Fixnum(0.000055f),
                               Fixnum(0.000065f));

        if (got_treasure_ or got_bomb_) {
            if (crane_pos_.y < 120) {
                crane_pos_.y += Fixnum(0.00003f) * delta;
                depth_ -= Fixnum(0.00006f) * delta;
            } else {
                depth_ -= descent_speed_ * delta;
            }

            bomb_timer_ += delta;

            if ((got_bomb_ and bomb_timer_ > milliseconds(750))
                or depth_ < -32) {

                pfrm.screen().schedule_fade(1.f);
                pfrm.screen().clear();
                pfrm.screen().display();

                if (got_bomb_) {
                    if (auto room = app.player_island().get_room(crane_loc_)) {
                        if (auto crane = dynamic_cast<Crane*>(room)) {
                            crane->set_item(0);
                        }
                    }
                }

                exit_ = true;
                return scene_pool::alloc<CraneFadeinScene>();
            }
        } else {
            if (crane_pos_.y > 30) {
                crane_pos_.y -= descent_speed_ * delta;
            }
            depth_ += descent_speed_ * delta;
        }


        x_speed_ = clamp(x_speed_, Fixnum(-0.0001f), Fixnum(0.0001f));

        if (x_speed_ > 0) {
            x_speed_ -= Fixnum(0.0000007f);
        } else if (x_speed_ < 0) {
            x_speed_ += Fixnum(0.0000007f);
        }


        if (app.player().key_pressed(pfrm, Key::right)) {
            x_speed_ += Fixnum(0.0000045f);
        }

        if (app.player().key_pressed(pfrm, Key::left)) {
            x_speed_ -= Fixnum(0.0000045f);
        }

        crane_pos_.x += x_speed_ * delta;


        HitBox crane_hb;
        crane_hb.position_ = &crane_pos_;
        crane_hb.dimension_.size_ = {24, 24};
        crane_hb.dimension_.origin_ = {16, 16};

        bool seen_object = false;

        for (auto it = level_->objects_.begin(); it not_eq level_->objects_.end();) {
            auto& object = *it;

            auto screen_y = object.y_.get() - depth_;
            if (screen_y < -24 or screen_y > 180) {
                if (seen_object) {
                    break;
                } else {
                    ++it;
                    continue;
                }
            }
            seen_object = true;

            Vec2<Fixnum> object_pos;
            object_pos.y = (object.y_.get() - depth_);
            object_pos.x = Fixnum::create(object.x_.get());

            HitBox object_hb;
            object_hb.position_ = &object_pos;
            object_hb.dimension_.size_ = {24, 14};
            object_hb.dimension_.origin_ = {16, 16};

            if (object_hb.overlapping(crane_hb)) {

                if (object.type_ == 0) {
                    got_bomb_ = true;
                } else {
                    got_treasure_ = true;
                }

                Level::CaughtObject c;
                c.type_ = object.type_;
                c.crane_x_offset_ =
                    rng::sample<4>((object_pos.x - crane_pos_.x).as_integer(),
                                   rng::utility_state);
                c.crane_y_offset_ =
                    rng::sample<4>((object_pos.y - crane_pos_.y).as_integer(),
                                   rng::utility_state);
                level_->caught_objects_.push_back(c);

                it = level_->objects_.erase(it);
            } else {
                ++it;
            }
        }

        draw_crane(pfrm, crane_pos_, got_treasure_ ? 13 : 6);
        return null_scene();
    }


    void display(Platform& pfrm, App& app) override
    {
        if (exit_) {
            return;
        }
        Sprite spr;

        for (auto& object : level_->objects_) {
            auto y = object.y_.get() - depth_;
            if (y < -24 or y > 180) {
                continue;
            }

            auto x = Fixnum::create(object.x_.get());

            spr.set_origin({16, 16});
            spr.set_texture_index(12);
            spr.set_position({x, y});
            pfrm.screen().draw(spr);
        }

        spr.set_mix({ColorConstant::electric_blue, 100});
        for (auto& object : level_->caught_objects_) {
            spr.set_texture_index(12);
            spr.set_origin({16, 16});
            spr.set_position({crane_pos_.x + object.crane_x_offset_ / 2,
                              crane_pos_.y + object.crane_y_offset_ / 2});
            pfrm.screen().draw(spr);
        }
    }

private:
    RoomCoord crane_loc_;
    Vec2<Fixnum> crane_pos_;
    Fixnum x_speed_;
    Fixnum depth_;
    Fixnum descent_speed_ = Fixnum(0.00006f);
    bool got_treasure_ = false;
    bool got_bomb_ = false;
    Microseconds bomb_timer_ = 0;
    bool exit_ = false;

    struct Level
    {
        struct Object
        {
            u8 type_;
            HostInteger<u16> y_;
            HostInteger<u64> x_;
        };

        Buffer<Object, 100> objects_;

        struct CaughtObject
        {
            u8 type_;
            s8 crane_x_offset_;
            s8 crane_y_offset_;
        };

        Buffer<CaughtObject, 10> caught_objects_;
    };

    DynamicMemory<Level> level_;
};



class CraneDropCinematicScene : public Scene
{
public:
    CraneDropCinematicScene(const RoomCoord& crane_pos) :
        data_(allocate_dynamic<Data>("crane-drop-data")),
        crane_pos_(crane_pos)
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
        x_speed_ = clamp(x_speed_, Fixnum(-0.00004f), Fixnum(0.00004f));

        if (x_speed_ > 0) {
            x_speed_ -= Fixnum(0.000001f);
        } else if (x_speed_ < 0) {
            x_speed_ += Fixnum(0.000001f);
        }


        if (app.player().key_pressed(pfrm, Key::right)) {
            x_speed_ += Fixnum(0.000003f);
        }

        if (app.player().key_pressed(pfrm, Key::left)) {
            x_speed_ -= Fixnum(0.000003f);
        }

        crane_x_ += x_speed_ * delta;


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
                return scene_pool::alloc<CraneMinigameScene>(crane_pos_,
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
    Fixnum x_speed_ = 0;

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
    RoomCoord crane_pos_;

    Microseconds cloud_timer_;
};



ScenePtr<Scene>
CraneDropScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

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

        return scene_pool::alloc<CraneDropCinematicScene>(crane_pos_);

    } else {
        const auto amount =
            smoothstep(0.f, fade_duration - fade_start, timer_ - fade_start);

        pfrm.speaker().set_music_volume(20 - 14 * amount);

        pfrm.screen().schedule_fade(amount);
    }

    return null_scene();
}



} // namespace skyland
