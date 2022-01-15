#include "titleScreenScene.hpp"
#include "loadModuleScene.hpp"
#include "module.hpp"
#include "modules/fileBrowserModule.hpp"
#include "multiplayerConnectScene.hpp"
#include "newgameScene.hpp"
#include "script/lisp.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/birbs/smolBirb.hpp"
#include "skyland/keyCallbackProcessor.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "zoneImageScene.hpp"



namespace skyland {



void init_clouds(Platform& pfrm);



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer)
{
    u16 tile = start_tile;

    for (u16 y = start_y; y < start_y + height; ++y) {
        for (u16 x = start_x; x < start_x + width; ++x) {
            pfrm.set_raw_tile(layer, x, y, tile++);
        }
    }
}



TitleScreenScene::TitleScreenScene(int start_page)
{
    switch (start_page) {
    case 1:
        return;

    case 2:
        state_ = State::resume_challenges;
        break;

    case 3:
        state_ = State::resume_end;
        break;
    }
}



void TitleScreenScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().fade(1.f);
    const int offset = 64;

    app.swap_player<PlayerP1>();

    init_clouds(pfrm);
    pfrm.system_call("v-parallax", (void*)false);

    key_callback_processor.clear();

    auto view = pfrm.screen().get_view();
    auto c = view.get_center();
    c.x = 0;
    c.y += offset;
    view.set_center(c);
    pfrm.screen().set_view(view);

    pfrm.load_overlay_texture("overlay");
    pfrm.load_tile1_texture("skyland_title_1_flattened");
    pfrm.load_tile0_texture("skyland_title_0_flattened");
    pfrm.load_sprite_texture("spritesheet_title_screen");

    if (not pfrm.speaker().is_music_playing("shadows")) {
        pfrm.speaker().play_music("shadows", true);
    }

    pfrm.fill_overlay(0);

    redraw_margins(pfrm);

    for (int i = 0; i < 32; ++i) {
        for (int j = 0; j < 32; ++j) {
            pfrm.set_raw_tile(Layer::map_0, i, j, 0);
            pfrm.set_raw_tile(Layer::map_1, i, j, 0);
        }
    }

    __draw_image(pfrm, 1, 0, 3, 30, 14, Layer::map_1);
    __draw_image(pfrm, 1, 0, 3, 30, 14, Layer::map_0);

    window_image_hack(pfrm, 2);

    pfrm.set_scroll(Layer::map_1_ext, 0, -offset + 8);

    pfrm.set_overlay_origin(0, 4);

    app.delete_backup();
}


void TitleScreenScene::window_image_hack(Platform& pfrm, u16 empty_tile)
{
    // We needed to cram the textures for the scrolling background image into
    // the texture for another background layer (in this case, into the empty
    // space inside the window image). But, we draw the layer as an image, and
    // we need to mask out all of the bits that we don't want to be visible.

    // for (int i = 3; i < 12; ++i) {
    //     pfrm.set_tile(Layer::map_1_ext, i, 4, empty_tile);
    //     pfrm.set_tile(Layer::map_1_ext, i, 5, empty_tile);
    // }
    // pfrm.set_tile(Layer::map_1_ext, 3, 6, empty_tile);

    for (int i = 3; i < 12; ++i) {
        pfrm.set_tile(Layer::map_0_ext, i, 4, empty_tile);
        pfrm.set_tile(Layer::map_0_ext, i, 5, empty_tile);
    }
    pfrm.set_tile(Layer::map_0_ext, 3, 6, empty_tile);
}



void TitleScreenScene::redraw_margins(Platform& pfrm)
{
    const auto screen_tiles = calc_screen_tiles(pfrm);
    for (int i = 0; i < screen_tiles.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, 0, 112);
        pfrm.set_tile(Layer::overlay, i, 1, 112);
        pfrm.set_tile(Layer::overlay, i, 2, 116);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 4, 256);
    }
}



void TitleScreenScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.set_overlay_origin(0, 0);

    init_clouds(pfrm);

    text_.reset();

    pfrm.system_call("v-parallax", (void*)true);

    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");
    pfrm.load_sprite_texture("spritesheet");


    vram_write_flag(pfrm, app.gp_.flag_img_);


    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_0_ext, x, y, 0);
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    pfrm.fill_overlay(0);

    pfrm.screen().set_view(View{});

    pfrm.set_scroll(Layer::map_1_ext, 0, 8);
    pfrm.set_scroll(Layer::map_0_ext, 0, 0);

    app.birbs().clear();
}



static const char* menu_text[4]{
    "adventure",
    "challenge",
    "multiplayer",
    "extras",
};



void TitleScreenScene::put_module_text(Platform& pfrm)
{
    text_.reset();
    redraw_margins(pfrm);
    show_module_icons(pfrm, 0); // TODO: page

    const auto st = calc_screen_tiles(pfrm);
    StringBuffer<32> buffer;
    if (module_cursor_) {
        const auto index = module_cursor_->x + module_cursor_->y * 3;
        if (auto factory = detail::_Module::Factory::get(index)) {
            buffer += factory->name();
        } else {
            return;
        }
    } else {
        return;
    }

    const auto len = utf8::len(buffer.c_str());

    auto margin = centered_text_margins(pfrm, buffer.length());
    text_.emplace(pfrm,
                  buffer.c_str(),
                  OverlayCoord{u8(st.x - (len + margin)), u8(st.y - 2)});
}



void TitleScreenScene::put_menu_text(Platform& pfrm)
{
    text_.reset();
    redraw_margins(pfrm);

    const auto st = calc_screen_tiles(pfrm);
    StringBuffer<32> buffer("SKYLAND:   ");
    const auto prefix_len = buffer.length();
    buffer += menu_text[menu_selection_];

    const auto len = utf8::len(buffer.c_str());

    auto margin = centered_text_margins(pfrm, buffer.length());
    text_.emplace(pfrm,
                  buffer.c_str(),
                  OverlayCoord{u8(st.x - (len + margin + 1)), u8(st.y - 2)});

    menu_selection_start_ = margin + 1 + prefix_len + (len % 2 ? 1 : 0);
    menu_selection_stop_ = margin + 1 + buffer.length() + (len % 2 ? 1 : 0);

    pfrm.set_tile(Layer::overlay, menu_selection_start_ - 4, st.y - 2, 375);
    pfrm.set_tile(Layer::overlay, menu_selection_stop_ - 1, st.y - 2, 376);
}



void TitleScreenScene::run_init_scripts(Platform& pfrm,
                                        App& app,
                                        bool allow_mods)
{
    // For some stuff, like the tutorial viewer, or multiplayer games,
    // everything would get messed up if we allowed users to run modded scripts.

    const bool use_rom_fs = (allow_mods == false);

    app.invoke_script(pfrm, "/scripts/config/rooms.lisp", use_rom_fs);
    app.invoke_script(pfrm, "/scripts/config/damage.lisp", use_rom_fs);
    app.invoke_script(pfrm, "/scripts/config/timing.lisp", use_rom_fs);

    if (allow_mods) {
        app.invoke_ram_script(pfrm, "/mods/init.lisp");
    }
}



ScenePtr<Scene>
TitleScreenScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    rng::get(rng::critical_state);

    hover_timer_ += delta;

    pong_.update();

    {
        ambient_movement_ =
            4 * float(sine(1 * 3.14f * 0.0005f * hover_timer_ + 180)) /
            std::numeric_limits<s16>::max();

        const int offset = 60 + ambient_movement_;

        auto view = pfrm.screen().get_view();
        auto c = view.get_center();
        c.y = offset;
        view.set_center(c);
        pfrm.screen().set_view(view);

        if (x_scroll_ < 0) {
            pfrm.set_scroll(Layer::map_1_ext, x_scroll_ - 272, -offset + 8);
            pfrm.set_scroll(Layer::map_0_ext, x_scroll_, -offset + 8);
        } else if (x_scroll_ > 240) {
            pfrm.set_scroll(Layer::map_1_ext, x_scroll_ - 240, -offset + 8);
            pfrm.set_scroll(Layer::map_0_ext, x_scroll_ + 32, -offset + 8);
        } else {
            pfrm.set_scroll(Layer::map_1_ext, x_scroll_ - 240, -offset + 8);
            pfrm.set_scroll(Layer::map_0_ext, x_scroll_, -offset + 8);
        }
    }


    if (menu_selection_ == 1) {
        island_mov_timer_ += delta;
        island_offset_ = 2 *
                         float(sine(2 * 3.14f * 0.0005f * hover_timer_ + 180)) /
                         std::numeric_limits<s16>::max();
    } else if (menu_selection_ == 0) {
        birb_timer_ -= delta;
        if (birb_timer_ <= 0) {
            birb_timer_ =
                seconds(12) + seconds(rng::choice<4>(rng::critical_state));
            if (rng::choice<6>(rng::critical_state)) {
                birb_timer_ =
                    milliseconds(1300) -
                    milliseconds(rng::choice<700>(rng::critical_state));
            }
            auto pos = Vec2<Float>{1, 130};
            auto speed =
                (5 + rng::choice<4>(rng::critical_state)) / Float(100000);
            pos.y += rng::choice<20>(rng::critical_state);
            if (auto e = alloc_entity<SmolBirb>(pos, speed)) {
                app.birbs().push(std::move(e));
            }
        }
    }

    for (auto& birb : app.birbs()) {
        if (birb->sprite().get_position().x > (197 + 16) - x_scroll_ / 2) {
            birb->kill();
        }
    }
    update_entities(pfrm, app, delta, app.birbs());


    switch (state_) {
    case State::resume_challenges:
        state_ = State::fade_in;
        menu_selection_ = 1;
        x_scroll_ = 240;
        break;

    case State::resume_end:
        state_ = State::fade_in;
        menu_selection_ = 3;
        pfrm.load_tile0_texture("skyland_title_3_flattened");
        window_image_hack(pfrm, 130);
        x_scroll_ = 480;
        break;

    case State::fade_in: {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            pfrm.screen().fade(0.f);
            state_ = State::wait;
            put_menu_text(pfrm);
            timer_ = 0;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::wait:
        selector_timer_ += delta;
        if (selector_timer_ > milliseconds(100)) {
            selector_timer_ = 0;
            selector_shaded_ = not selector_shaded_;

            const auto st = calc_screen_tiles(pfrm);
            if (selector_shaded_) {
                pfrm.set_tile(
                    Layer::overlay, menu_selection_start_ - 4, st.y - 2, 373);
                pfrm.set_tile(
                    Layer::overlay, menu_selection_stop_ - 1, st.y - 2, 374);
            } else {
                pfrm.set_tile(
                    Layer::overlay, menu_selection_start_ - 4, st.y - 2, 375);
                pfrm.set_tile(
                    Layer::overlay, menu_selection_stop_ - 1, st.y - 2, 376);
            }
        }

        if (pfrm.keyboard().pressed<Key::action_1>()) {
            state_ = State::fade_out;
            if (menu_selection_ == 3) {
                state_ = State::fade_modules_1;
            } else {
                pfrm.speaker().play_music("unaccompanied_wind", 0);
            }
        }

        if (app.player().key_down(pfrm, Key::right) or
            app.player().key_down(pfrm, Key::down)) {
            if (menu_selection_ == 0) {
                menu_selection_ = 1;
                put_menu_text(pfrm);
                // pfrm.speaker().play_sound("scroll", 1);
                state_ = State::scroll_right;
                timer_ = 0;
            } else if (menu_selection_ == 2) {
                menu_selection_ = 0;
                put_menu_text(pfrm);
                state_ = State::scroll_to_center;
                timer_ = 0;
            } else if (menu_selection_ == 1) {
                menu_selection_ = 3;
                put_menu_text(pfrm);
                state_ = State::scroll_to_end;
                timer_ = 0;
                pfrm.sleep(1);
                pfrm.load_tile0_texture("skyland_title_3_flattened");
                window_image_hack(pfrm, 130);
            }
        }
        if (app.player().key_down(pfrm, Key::left) or
            app.player().key_down(pfrm, Key::up)) {
            if (menu_selection_ == 1) {
                menu_selection_ = 0;
                put_menu_text(pfrm);
                // pfrm.speaker().play_sound("scroll", 1);
                state_ = State::scroll_left;
                timer_ = 0;
            } else if (menu_selection_ == 0) {
                menu_selection_ = 2;
                put_menu_text(pfrm);
                state_ = State::scroll_multiplayer;
                pfrm.sleep(1);
                pfrm.load_tile1_texture("skyland_title_2_flattened");
                timer_ = 0;
            } else if (menu_selection_ == 3) {
                menu_selection_ = 1;
                put_menu_text(pfrm);
                state_ = State::scroll_from_end;
                timer_ = 0;
            }
        }
        break;

    case State::scroll_multiplayer: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = -240;
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = -240 * amount;
        }
        break;
    }

    case State::scroll_to_center: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            pfrm.sleep(1);
            pfrm.load_tile1_texture("skyland_title_1_flattened");
            x_scroll_ = 0;
        } else {
            const auto amount =
                -240 * (1.f - smoothstep(0.f, duration, timer_));
            x_scroll_ = amount;
        }
        break;
    }


    case State::scroll_to_end: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = 480;
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = 240 + 240 * amount;
        }
        break;
    }


    case State::scroll_from_end: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = 240;
            pfrm.sleep(1);
            pfrm.load_tile0_texture("skyland_title_0_flattened");
            window_image_hack(pfrm, 2);
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = 480 - 240 * amount;
        }
        break;
    }


    case State::scroll_right: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = 240;
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = 240 * amount;
        }
        break;
    }


    case State::scroll_left: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = 0;
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = 240 - 240 * amount;
        }
        break;
    }


    case State::fade_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1300);
        if (timer_ > fade_duration) {
            text_.reset();
            switch (menu_selection_) {
            case 0:
                app.game_mode() = App::GameMode::adventure;
                run_init_scripts(pfrm, app, true);
                return scene_pool::alloc<NewgameScene>();

            case 1: {
                app.game_mode() = App::GameMode::challenge;
                run_init_scripts(pfrm, app, true);
                return scene_pool::alloc<SelectChallengeScene>();
            }

            case 2:
                app.game_mode() = App::GameMode::multiplayer;
                run_init_scripts(pfrm, app, false);
                return scene_pool::alloc<MultiplayerConnectScene>();

            case 3:
                pfrm.fatal("logic error, this should be unreachable");
            }
        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);

            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }


    case State::fade_modules_backout: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(300);
        if (timer_ > fade_duration) {
            put_menu_text(pfrm);

            pfrm.screen().fade(0.f);

            state_ = State::wait;

        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);

            pfrm.screen().fade(0.7f - 0.7f * amount,
                               ColorConstant::rich_black,
                               {},
                               true,
                               true);
        }
        break;
    }

    case State::fade_modules_1: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(500);
        if (timer_ > fade_duration) {
            redraw_margins(pfrm);
            show_module_icons(pfrm, 0);
            state_ = State::show_modules;

            pfrm.screen().fade(0.7f, ColorConstant::rich_black, {}, true, true);

            module_cursor_ = {0, 0};

            put_module_text(pfrm);

        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);

            pfrm.screen().fade(
                0.7f * amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }



    case State::show_modules:
        selector_timer_ += delta;
        if (selector_timer_ > milliseconds(200)) {
            selector_timer_ -= milliseconds(200);
            selector_shaded_ = not selector_shaded_;
        }

        pfrm.screen().fade(0.69f, ColorConstant::rich_black, {}, false, false);
        if (app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::fade_modules_backout;
            timer_ = 0;
            pfrm.fill_overlay(0);
            redraw_margins(pfrm);
            module_cursor_.reset();
        }
        if (module_cursor_) {
            if (app.player().key_down(pfrm, Key::right) and
                module_cursor_->x < 2) {
                module_cursor_->x += 1;
                put_module_text(pfrm);
            }
            if (app.player().key_down(pfrm, Key::left) and
                module_cursor_->x > 0) {
                module_cursor_->x -= 1;
                put_module_text(pfrm);
            }
            if (app.player().key_down(pfrm, Key::up) and
                module_cursor_->y > 0) {
                module_cursor_->y -= 1;
                put_module_text(pfrm);
            }
            if (app.player().key_down(pfrm, Key::down) and
                module_cursor_->y < 1) {
                module_cursor_->y += 1;
                put_module_text(pfrm);
            }

            if (app.player().key_down(pfrm, Key::action_1)) {
                auto index = module_cursor_->x + module_cursor_->y * 3;
                if (auto f = detail::_Module::Factory::get(index)) {
                    pfrm.speaker().play_music("unaccompanied_wind", 0);
                    pfrm.fill_overlay(0);
                    pfrm.screen().fade(
                        1.f, ColorConstant::rich_black, {}, true, true);
                    app.game_mode() = App::GameMode::challenge;

                    if (f->run_scripts()) {
                        run_init_scripts(pfrm, app, f->enable_custom_scripts());
                    }

                    return f->create(pfrm);
                }
            }
        }
        break;

    case State::wait_2:
        break;
    }

    return null_scene();
}



void TitleScreenScene::show_module_icons(Platform& pfrm, int page)
{
    auto icon_vram = 181;

    auto alloc_icon = [&] {
        auto result = icon_vram;
        switch (icon_vram) {
        case 181:
            icon_vram = 197;
            break;
        case 197:
            icon_vram = 213;
            break;
        case 213:
            icon_vram = 229;
            break;
        case 229:
            icon_vram = 258;
            break;
        case 258:
            icon_vram = 274;
            break;
        }
        return result;
    };


    auto show_icon = [&](int x, int y) {
        auto x_start = 4 + x * 8;
        auto y_start = 2 + y * 8;

        const auto index = x + y * 3;
        if (auto f = detail::_Module::Factory::get(index)) {

            for (int x = 0; x < 6; ++x) {
                for (int y = 0; y < 6; ++y) {
                    pfrm.set_tile(
                        Layer::overlay, x_start + x, y_start + y, 470);
                }
            }

            pfrm.set_tile(Layer::overlay, x_start, y_start, 466);
            pfrm.set_tile(Layer::overlay, x_start + 5, y_start, 467);
            pfrm.set_tile(Layer::overlay, x_start, y_start + 5, 468);
            pfrm.set_tile(Layer::overlay, x_start + 5, y_start + 5, 469);

            auto icon = alloc_icon();
            pfrm.load_overlay_chunk(icon, f->icon(), 16);
            draw_image(
                pfrm, icon, x_start + 1, y_start + 1, 4, 4, Layer::overlay);
        }
    };

    for (int x = 0; x < 3; ++x) {
        for (int y = 0; y < 2; ++y) {
            show_icon(x, y);
        }
    }
}



void TitleScreenScene::display(Platform& pfrm, App& app)
{
    if (x_scroll_ > 160) {
        Sprite sprite;
        sprite.set_position({Float(135 - x_scroll_ / 3) + island_offset_,
                             Float(110 - 0.25f * (240 - x_scroll_))});
        sprite.set_priority(3);

        pfrm.screen().draw(sprite);
    } else if (x_scroll_ < 0) {
        pong_.display(pfrm, x_scroll_);
    }
    for (auto& birb : app.birbs()) {
        auto spr = birb->sprite();
        auto pos = spr.get_position();
        pos.x = pos.x - x_scroll_ / 2;

        const auto ambient_movement =
            8 *
            float(sine(((birb->speed() * 100000) / 2) * 3.14f * 0.0005f *
                           birb->age() +
                       180)) /
            std::numeric_limits<s16>::max();

        pos.y += ambient_movement;

        spr.set_position(pos);
        pfrm.screen().draw(spr);
    }

    if (module_cursor_) {
        Sprite sprite;
        sprite.set_texture_index(36);
        sprite.set_size(Sprite::Size::w16_h32);
        sprite.set_origin({8, 8});
        sprite.set_priority(0);


        sprite.set_position(
            {28 + 64.f * module_cursor_->x + (selector_shaded_ ? 1 : 0),
             ambient_movement_ + 60 + 8 + 64.f * module_cursor_->y +
                 (selector_shaded_ ? 1 : 0)});
        pfrm.screen().draw(sprite);


        sprite.set_flip({true, false});
        sprite.set_position(
            {83 + 64.f * module_cursor_->x - (selector_shaded_ ? 1 : 0),
             ambient_movement_ + 60 + 8 + 64.f * module_cursor_->y +
                 (selector_shaded_ ? 1 : 0)});
        pfrm.screen().draw(sprite);


        sprite.set_flip({false, true});
        sprite.set_position(
            {28 + 64.f * module_cursor_->x + (selector_shaded_ ? 1 : 0),
             ambient_movement_ + 60 + 8 + 64.f * module_cursor_->y + 40 -
                 (selector_shaded_ ? 1 : 0)});
        pfrm.screen().draw(sprite);


        sprite.set_flip({true, true});
        sprite.set_position(
            {83 + 64.f * module_cursor_->x - (selector_shaded_ ? 1 : 0),
             ambient_movement_ + 60 + 8 + 64.f * module_cursor_->y + 40 -
                 (selector_shaded_ ? 1 : 0)});
        pfrm.screen().draw(sprite);
    }
}



void TitleScreenScene::Pong::update()
{
    if (ball_.x >= 23) {
        ball_speed_.x *= -1;
    }
    if (ball_.y >= 20) {
        ball_speed_.y *= -1;
    }
    if (ball_.y < 1) {
        ball_speed_.y *= -1;
    }
    if (ball_.x < 1) {
        ball_speed_.x *= -1;
    }

    if (pad1_.pos_ >= 19) {
        pad1_.speed_ *= -1;
    }
    if (pad1_.pos_ < 0) {
        pad1_.speed_ *= -1;
    }

    if (pad2_.pos_ >= 20) {
        pad2_.speed_ *= -1;
    }
    if (pad2_.pos_ < 2) {
        pad2_.speed_ *= -1;
    }

    ball_.x += ball_speed_.x;
    ball_.y += ball_speed_.y;

    pad1_.pos_ += pad1_.speed_;
    pad2_.pos_ += pad2_.speed_;
}



void TitleScreenScene::Pong::display(Platform& pfrm, int x_scroll)
{
    auto view = pfrm.screen().get_view();
    auto c = view.get_center();

    const Vec2<Float> anchor = {73.f, (c.y - 64) + 148};

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(26);
    sprite.set_origin({1, 2});
    sprite.set_position(
        {(anchor.x) - (240 + x_scroll),
         anchor.y + clamp(interpolate(ball_.y, pad1_.pos_, 1.f - ball_.x / 22),
                          0.f,
                          19.f)});
    pfrm.screen().draw(sprite);

    sprite.set_position(
        {(anchor.x + 24) - (240 + x_scroll),
         anchor.y + interpolate(ball_.y, pad2_.pos_, ball_.x / 22)});

    pfrm.screen().draw(sprite);

    sprite.set_origin({});
    sprite.set_texture_index(27);
    sprite.set_position(
        {(ball_.x + anchor.x) - (240 + x_scroll), ball_.y + anchor.y});
    pfrm.screen().draw(sprite);
}



} // namespace skyland
