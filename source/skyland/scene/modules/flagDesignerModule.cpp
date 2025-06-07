////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "flagDesignerModule.hpp"
#include "platform/platform.hpp"
#include "skyland/configure_island.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void load_flag(u16 tile);



class SurfaceFlagsScene : public Scene
{
private:
    int page_ = 0;
    Vec2<int> cursor_;
    Time cursor_timer_ = seconds(1);
    bool cursor_flip_ = false;

public:
    void print_icon(const OverlayCoord& coord, u16 tile)
    {
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, tile);
        PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y, tile + 1);
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y + 1, tile + 2);
        PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y + 1, tile + 3);
    }


    void enter(Scene&) override
    {
        PLATFORM.load_overlay_texture("flags");
        Text::print("historical flags:",
                    OverlayCoord{1, 1},
                    {{custom_color(0xcdc3eb), ColorConstant::rich_black}});

        int tile = 88 + 8;

        for (int y = 0; y < 5; ++y) {
            for (int x = 0; x < 9; ++x) {
                print_icon({(u8)(2 + x * 3), (u8)(4 + y * 3)}, tile);
                tile += 4;
            }
        }

        load_page();
    }



    void exit(Scene&) override
    {
        PLATFORM.load_overlay_texture("overlay");
    }



    void load_page()
    {
        static const int page_count = 6;
        int margin = (calc_screen_tiles().x - page_count * 2) / 2;
        for (int i = 0; i < page_count; ++i) {
            if (i == page_ + 1) {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 19, 85);
            } else {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 19, 84);
            }
        }

        PLATFORM.load_overlay_chunk(88 + 8, 268 + 180 * page_, 180);
    }



    ScenePtr update(Time delta) override;



    bool editing_ingame_;
};



class FlagTemplateScene : public Scene
{
public:
    void print_icon(const OverlayCoord& coord, u16 tile)
    {
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y, tile);
        PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y, tile + 1);
        PLATFORM.set_tile(Layer::overlay, coord.x, coord.y + 1, tile + 2);
        PLATFORM.set_tile(Layer::overlay, coord.x + 1, coord.y + 1, tile + 3);
    }


    void enter(Scene& prev) override
    {
        PLATFORM.load_overlay_texture("flags");

        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1);

        Text::OptColors colors = {
            {custom_color(0xcdc3eb), ColorConstant::rich_black}};

        Text::print(SYS_CSTR(choose_flag), {1, 1}, colors);
        Text::print(SYS_CSTR(flag_default), {4, 4}, colors);
        Text::print(SYS_CSTR(flag_alt1), {4, 6}, colors);
        Text::print(SYS_CSTR(flag_alt2), {4, 8}, colors);
        Text::print(SYS_CSTR(flag_alt3), {4, 10}, colors);
        Text::print(SYS_CSTR(flag_alt4), {4, 12}, colors);
        Text::print(SYS_CSTR(flag_alt5), {4, 14}, colors);
        Text::print(SYS_CSTR(flag_alt6), {4, 16}, colors);

        for (int y = 0; y < 7; ++y) {
            print_icon({1, u8(4 + y * 2)}, 88 + 8 + 4 * y);
        }

        PLATFORM.set_tile(Layer::overlay, 3, 4, 86);

        static const int page_count = 6;
        int margin = (calc_screen_tiles().x - page_count * 2) / 2;
        for (int i = 0; i < page_count; ++i) {
            if (i == 0) {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 19, 85);
            } else {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 19, 84);
            }
        }
    }



    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.load_overlay_texture("overlay");
    }


    ScenePtr update(Time delta) override
    {
        player().update(delta);

        auto test_key = [&](Key k) {
            return player().test_key(k, milliseconds(500), milliseconds(100));
        };

        if (player().key_down(Key::action_2) or
            player().key_down(Key::select)) {
            auto next = make_scene<FlagDesignerModule>();
            next->editing_ingame_ = editing_ingame_;
            return next;
        }

        if (player().key_down(Key::right)) {
            PLATFORM.speaker().play_sound("click_wooden", 2);
            auto next = make_scene<SurfaceFlagsScene>();
            next->editing_ingame_ = editing_ingame_;
            return next;
        }


        if (test_key(Key::down)) {
            if (sel_ < 6) {
                ++sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 3, 4 + y, 0);
            }
            PLATFORM.set_tile(Layer::overlay, 3, 4 + sel_ * 2, 86);
        }
        if (test_key(Key::up)) {
            if (sel_ > 0) {
                --sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 3, 4 + y, 0);
            }
            PLATFORM.set_tile(Layer::overlay, 3, 4 + sel_ * 2, 86);
        }

        if (player().key_down(Key::action_1)) {
            switch (sel_) {
            case 0:
                load_default_flag();
                break;

            case 1:
                load_flag(381);
                break;

            case 2:
                load_flag(379);
                break;

            case 6:
                load_flag(376);
                break;

            case 3:
                load_flag(378);
                break;

            case 5:
                load_flag(377);
                break;

            case 4:
                load_flag(380);
                break;
            }
            auto next = make_scene<FlagDesignerModule>();
            next->editing_ingame_ = editing_ingame_;
            next->changed_ = true;
            return next;
        }


        return null_scene();
    }

    bool editing_ingame_ = false;

private:
    int sel_ = 0;
};



ScenePtr SurfaceFlagsScene::update(Time delta)
{
    player().update(delta);

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

    auto clear_cursor = [&] {
        PLATFORM.set_tile(
            Layer::overlay, 2 + cursor_.x * 3 - 1, 4 + cursor_.y * 3 - 1, 0);

        PLATFORM.set_tile(Layer::overlay,
                          (2 + cursor_.x * 3 - 1) + 3,
                          (4 + cursor_.y * 3 - 1) + 0,
                          0);

        PLATFORM.set_tile(Layer::overlay,
                          (2 + cursor_.x * 3 - 1) + 0,
                          (4 + cursor_.y * 3 - 1) + 3,
                          0);

        PLATFORM.set_tile(Layer::overlay,
                          (2 + cursor_.x * 3 - 1) + 3,
                          (4 + cursor_.y * 3 - 1) + 3,
                          0);
        cursor_timer_ = milliseconds(200);
    };

    if (player().key_down(Key::action_1)) {
        u16 tile = 268 + 180 * page_ + cursor_.x * 4 + cursor_.y * 4 * 9;

        auto data = PLATFORM.extract_tile(Layer::overlay, tile);
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 8; ++y) {
                APP.custom_flag_image_.pixels[x][y] =
                    data.data_[x][y + 1] & 0x0f;
            }
        }

        ++tile;
        data = PLATFORM.extract_tile(Layer::overlay, tile);
        for (int x = 0; x < 5; ++x) {
            for (int y = 0; y < 8; ++y) {
                APP.custom_flag_image_.pixels[8 + x][y] =
                    data.data_[x][y + 1] & 0x0f;
            }
        }

        ++tile;
        data = PLATFORM.extract_tile(Layer::overlay, tile);
        for (int x = 0; x < 8; ++x) {
            for (int y = 0; y < 4; ++y) {
                APP.custom_flag_image_.pixels[x][7 + y] =
                    data.data_[x][y] & 0x0f;
            }
        }

        ++tile;
        data = PLATFORM.extract_tile(Layer::overlay, tile);
        for (int x = 0; x < 5; ++x) {
            for (int y = 0; y < 4; ++y) {
                APP.custom_flag_image_.pixels[8 + x][7 + y] =
                    data.data_[x][y] & 0x0f;
            }
        }


        auto next = make_scene<FlagDesignerModule>();
        next->editing_ingame_ = editing_ingame_;
        next->changed_ = true;
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        return next;
    }

    if (player().key_down(Key::action_2) or player().key_down(Key::select)) {
        auto next = make_scene<FlagDesignerModule>();
        next->editing_ingame_ = editing_ingame_;
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        return next;
    }

    if (test_key(Key::down)) {
        if (cursor_.y < 4) {
            clear_cursor();
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            ++cursor_.y;
        }
    }

    if (test_key(Key::up)) {
        if (cursor_.y > 0) {
            clear_cursor();
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            --cursor_.y;
        }
    }


    if (test_key(Key::right)) {
        if (cursor_.x < 8) {
            clear_cursor();
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            ++cursor_.x;
        } else {
            if (page_ < 4) {
                ++page_;
                clear_cursor();
                cursor_.x = 0;
                load_page();
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
    }


    if (test_key(Key::left)) {
        if (cursor_.x > 0) {
            clear_cursor();
            PLATFORM.speaker().play_sound("cursor_tick", 0);
            --cursor_.x;
        } else {
            if (page_ == 0 and player().key_down(Key::left)) {
                PLATFORM.fill_overlay(0);
                PLATFORM.screen().clear();
                PLATFORM.screen().display();
                auto next = make_scene<FlagTemplateScene>();
                next->editing_ingame_ = editing_ingame_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
                return next;
            } else if (page_ > 0) {
                --page_;
                clear_cursor();
                cursor_.x = 8;
                load_page();
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
    }

    cursor_timer_ += delta;

    if (cursor_timer_ > milliseconds(200)) {
        PLATFORM.set_tile(Layer::overlay,
                          2 + cursor_.x * 3 - 1,
                          4 + cursor_.y * 3 - 1,
                          88 + (cursor_flip_ ? 0 : 4));

        PLATFORM.set_tile(Layer::overlay,
                          (2 + cursor_.x * 3 - 1) + 3,
                          (4 + cursor_.y * 3 - 1) + 0,
                          89 + (cursor_flip_ ? 0 : 4));

        PLATFORM.set_tile(Layer::overlay,
                          (2 + cursor_.x * 3 - 1) + 0,
                          (4 + cursor_.y * 3 - 1) + 3,
                          90 + (cursor_flip_ ? 0 : 4));

        PLATFORM.set_tile(Layer::overlay,
                          (2 + cursor_.x * 3 - 1) + 3,
                          (4 + cursor_.y * 3 - 1) + 3,
                          91 + (cursor_flip_ ? 0 : 4));

        cursor_timer_ = 0;
        cursor_flip_ = not cursor_flip_;
    }


    return null_scene();
}



void FlagDesignerModule::enter(Scene& prev)
{
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(1.f);

    APP.player_island().show_flag(true);


    if (editing_ingame_) {
        if (APP.player_island().interior_visible()) {
            PLATFORM.load_tile0_texture("tilesheet");
        }
        if (APP.player_island().flag_pos()) {
            auto flag_y = APP.player_island().flag_pos()->y;
            target_y_ = clamp(48 - (16 - flag_y) * 16, -80, 60);
        }
    } else {
        PLATFORM.load_tile0_texture("tilesheet");
    }

    Paint::init();

    if (editing_ingame_) {
        if (not prev.cast_world_scene() or
            APP.player_island().interior_visible()) {
            show_island_exterior(&APP.player_island());
        }

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
            }
        }

        View v;
        Float vx = -140 + -32;
        vx += APP.player_island().flag_pos()->x * 16;
        v.set_center({vx, Float(view_shift_)});
        PLATFORM.screen().set_view(v);

    } else {
        APP.player_island().init_terrain(4);
        configure_island_from_codestring(APP.player_island(),
                                         "'((power-core 1 13))");

        APP.player_island().render_exterior();
        APP.player_island().set_position(
            {Fixnum::from_integer(152), Fixnum::from_integer(370)});

        GenericBird::spawn(APP.player_island(),
                           rng::choice<3>(rng::utility_state));
    }

    show();

    PLATFORM.screen().schedule_fade(0);
}



void FlagDesignerModule::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);

    if (editing_ingame_) {
        if (APP.opponent_island()) {
            show_island_exterior(APP.opponent_island());
        }
    } else {
        PLATFORM_EXTENSION(force_vsync);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.screen().fade(1.f);
    }
}



void FlagDesignerModule::show()
{
    Paint::show();

    vram_write_flag(APP.custom_flag_image_, Layer::map_0_ext);
}



ScenePtr FlagDesignerModule::update(Time delta)
{
    APP.player().update(delta);

    if (tool_ == Tool::preset and APP.player().key_down(Key::action_1)) {
        auto next = make_scene<FlagTemplateScene>();
        next->editing_ingame_ = editing_ingame_;
        return next;
    }

    if (tool_ == Tool::exit and APP.player().key_down(Key::action_1)) {
        if (changed_) {
            APP.custom_flag_image_.save();
        }
        if (editing_ingame_) {
            return make_scene<ReadyScene>();
        } else {
            return make_scene<TitleScreenScene>(3);
        }
    }

    if (editing_ingame_ and view_shift_ not_eq target_y_) {
        int amount = 2;
        if (abs(view_shift_ - target_y_) > 40) {
            amount = 4;
        }
        if (abs(view_shift_ - target_y_) < 10) {
            amount = 1;
        }
        if (view_shift_ < target_y_) {
            view_shift_ += amount;
        } else {
            view_shift_ -= amount;
        }

        View v;
        Float vx = -140 + -32;
        vx += APP.player_island().flag_pos()->x * 16;
        v.set_center({vx, Float(view_shift_)});
        PLATFORM.screen().set_view(v);
    }

    APP.player_island().update_simple(delta);
    for (auto& r : APP.player_island().rooms()) {
        for (auto& c : r->characters()) {
            c->update(0, r.get());
        }
    }

    update_entities(delta, APP.effects());

    return Paint::update(delta);
}



void FlagDesignerModule::display()
{
    return Paint::display();
}



u8 FlagDesignerModule::get_pixel(u8 x, u8 y)
{
    if (x >= width() or y >= height()) {
        return 111;
    }
    return APP.custom_flag_image_.pixels[x][y];
}



void FlagDesignerModule::set_pixel(u8 x, u8 y, u8 value)
{
    if (x >= width() or y >= height()) {
        return;
    }
    APP.custom_flag_image_.pixels[x][y] = value;
    changed_ = true;
}



FlagDesignerModule::Factory FlagDesignerModule::factory_;



} // namespace skyland
