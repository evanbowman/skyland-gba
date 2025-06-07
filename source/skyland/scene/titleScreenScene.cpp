////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "titleScreenScene.hpp"
#include "boxedDialogScene.hpp"
#include "introCreditsScene.hpp"
#include "loadModuleScene.hpp"
#include "module.hpp"
#include "modules/fileBrowserModule.hpp"
#include "modules/macrocosmFreebuildModule.hpp"
#include "multiplayerConnectScene.hpp"
#include "platform/color.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/birds/smallBird.hpp"
#include "skyland/entity/misc/titleScreenMusicNote.hpp"
#include "skyland/keyCallbackProcessor.hpp"
#include "skyland/macrocosmEngine.hpp"
#include "skyland/macrocosmRaster.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "startAdventureScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



class EmberParticle : public Entity
{
public:
    EmberParticle(TitleScreenScene* parent,
                  const Vec2<Fixnum>& position,
                  u16 tile)
        : Entity({{}, {}}), parent_(parent)
    {
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(tile);
        sprite_.set_position(position);
        sprite_.set_origin({8, 8});
        sprite_.set_alpha(Sprite::Alpha::translucent);
        y_pos_ = position.y;
    }


    void update(Time delta)
    {
        delta *= 2;
        timer_ += delta;

        auto pos = sprite_.get_position();

        // pos.x -= +Float(delta) * 0.00001f;
        y_pos_ -= Fixnum(+Float(delta) * 0.00001f);
        pos.y = y_pos_ + Fixnum::from_integer(parent_->ambient_movement());

        Float interval = Float(timer_) / seconds(3);

        const s16 shrink_amount = interpolate(-450, -24, interval);

        sprite_.set_mix(
            {ColorConstant::silver_white, u8(255 - 255 * interval)});

        sprite_.set_scale({shrink_amount, shrink_amount});

        sprite_.set_position(pos);

        if (timer_ > seconds(3)) {
            this->kill();
        }
    }

private:
    TitleScreenScene* parent_;
    Time timer_ = 0;
    Fixnum y_pos_ = 0.0_fixed;
};



void init_clouds();



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer)
{
    u16 tile = start_tile;

    for (u16 y = start_y; y < start_y + height; ++y) {
        for (u16 x = start_x; x < start_x + width; ++x) {
            PLATFORM.set_raw_tile(layer, x, y, tile++);
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

    case 4:
        state_ = State::resume_macro;
        break;
    }
}



void plugin_rooms_unregister();



// Added when porting the game to different screen sizes.
static Vec2<int> scale_offset()
{
    int offset = 0;

    auto st = calc_screen_tiles();
    if (st.x > 30) {
        offset += ((30 - st.x) / 2) * 8;
    }

    int y_offset = 0;
    if (st.y not_eq 20) {
        y_offset = 1 * 8;
    }

    return {-offset, y_offset};
}



static Vec2<int> spr_scale_offset()
{
    if (PLATFORM.screen().size() == Vec2<u32>{240, 160}) {
        return scale_offset();
    } else {
        return {scale_offset().x, scale_offset().y + 8};
    }
}



static void set_scroll(Layer layer, int x_scroll, int y_scroll)
{
    auto offset = scale_offset();

    PLATFORM.set_scroll(layer, -offset.x + x_scroll, -offset.y + y_scroll);
}



void TitleScreenScene::enter(Scene& prev)
{
    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);

    PLATFORM.screen().schedule_fade(1.f);

    PLATFORM.screen().set_shader(passthrough_shader);
    APP.swap_environment<weather::ClearSkies>();

    APP.macrocosm().reset();

    APP.faction() = Faction::human;

    dev_ = APP.is_developer_mode();

    const int offset = 64;

    APP.camera().emplace<Camera>();

    APP.swap_player<PlayerP1>();

    init_clouds();
    PLATFORM_EXTENSION(vertical_parallax_enable, false);

    APP.player_island().clear();

    globals().room_pools_.create("room-mem");
    globals().entity_pools_.create("entity-mem");

    if (APP.opponent_island()) {
        APP.reset_opponent_island();
    }

    APP.key_callback_processor().clear();

    PLATFORM.speaker().set_music_speed(Platform::Speaker::MusicSpeed::regular);

    hide_translucence();

    APP.effects().clear();
    APP.birds().clear();

    // Back to the title screen! DLC plugins need to be unloaded...
    plugin_rooms_unregister();

    // By default, we do not allow recording for rewind. Enabled based on game
    // mode when fading into a level.
    APP.time_stream().enable_pushes(false);
    APP.time_stream().clear();

    APP.game_speed() = GameSpeed::normal;

    auto view = PLATFORM.screen().get_view();
    auto c = view.get_center();
    c.x = 0;
    c.y += offset;
    view.set_center(c);
    PLATFORM.screen().set_view(view);

    PLATFORM.load_overlay_texture("overlay");
    PLATFORM.load_tile1_texture("skyland_title_1_flattened");
    PLATFORM.load_tile0_texture("skyland_title_0_flattened");
    PLATFORM.load_background_texture("background_title_screen");

    if (not flower_effect_) {
        // (already loaded previously)
        PLATFORM.load_sprite_texture("spritesheet_title_screen");
    }

    {
        auto track = music_track();
        if (not PLATFORM.speaker().is_music_playing(track->c_str())) {
            PLATFORM.speaker().stream_music(track->c_str(), true);
        }
    }


    PLATFORM.fill_overlay(0);

    redraw_margins();

    for (int i = 0; i < 64; ++i) {
        for (int j = 0; j < 32; ++j) {
            PLATFORM.set_raw_tile(Layer::map_0, i, j, 0);
            PLATFORM.set_raw_tile(Layer::map_1, i, j, 0);
        }
    }


    if (PLATFORM.screen().size().y == 160) {
        // The original graphcs, for the gba
        __draw_image(31, 0, 3, 30, 14, Layer::map_1);
        __draw_image(31, 0, 3, 30, 14, Layer::map_0);
    } else {
        // The expanded background graphics, with new rows added to the
        // beginning and end. For the nds, with its taller screen.
        __draw_image(1, 0, 3, 30, 16, Layer::map_1);
        __draw_image(1, 0, 3, 30, 16, Layer::map_0);
    }

    set_scroll(Layer::map_1_ext, 0, -offset + 8);

    PLATFORM.set_overlay_origin(0, 4);

    APP.delete_backup();
}



void TitleScreenScene::redraw_margins()
{
    const auto screen_tiles = calc_screen_tiles();
    for (int i = 0; i < screen_tiles.x; ++i) {

        PLATFORM.set_tile(Layer::overlay, i, 0, 112);
        PLATFORM.set_tile(Layer::overlay, i, 1, 112);
        PLATFORM.set_tile(Layer::overlay, i, 2, 116);

        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 256);

        if (scale_offset().y / 8 > 0) {

            int y = 0;
            for (y = 0; y < 3; ++y) {
                PLATFORM.set_tile(Layer::overlay, i, y, 112);
            }

            for (; y < scale_offset().y / 8; ++y) {
                PLATFORM.set_tile(Layer::overlay, i, y, 112);
            }

            PLATFORM.set_tile(Layer::overlay, i, y, 116);

            // FIXME!
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 112);
            PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 5, 256);
        }
    }

    // Our images are 240p wide. Letterbox the graphics.
    if (screen_tiles.x > 30) {
        int overflow = screen_tiles.x - 30;
        int margin = overflow / 2;
        for (int y = 0; y < screen_tiles.y; ++y) {
            for (int x = 0; x < margin; ++x) {
                PLATFORM.set_tile(Layer::overlay, x, y, 112);
                PLATFORM.set_tile(
                    Layer::overlay, (screen_tiles.x - 1) - x, y, 112);
            }
        }
    }
}



void TitleScreenScene::exit(Scene& next)
{
    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);
    PLATFORM.speaker().set_sounds_volume(Platform::Speaker::music_volume_max);

    PLATFORM.set_overlay_origin(0, 0);

    init_clouds();

    text_.reset();

    PLATFORM_EXTENSION(vertical_parallax_enable, true);

    APP.effects().clear();
    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    PLATFORM.load_tile0_texture(APP.environment().player_island_texture());
    PLATFORM.load_tile1_texture(APP.environment().opponent_island_texture());
    PLATFORM.load_sprite_texture(APP.environment().sprite_texture());
    PLATFORM.load_background_texture(APP.environment().background_texture());

    write_custom_graphics();


    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            PLATFORM.set_tile(Layer::map_0_ext, x, y, 0);
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    PLATFORM.fill_overlay(0);

    PLATFORM.screen().set_view(View{});


    set_scroll(Layer::map_1_ext, 0, 8);
    set_scroll(Layer::map_0_ext, 0, 0);

    APP.birds().clear();
    APP.effects().clear();
}



static const SystemString menu_text[5] = {
    SystemString::menu_text_adventure,
    SystemString::menu_text_challenge,
    SystemString::menu_text_macro,
    SystemString::menu_text_extras,
    SystemString::menu_text_multiplayer,
};



static const int modules_per_row = 3;
static const int modules_per_page = modules_per_row * 2;



int text_offset()
{
    // FIXME...
    return 2;

    // FIXME! Use aspect ratio instead!
    if (PLATFORM.device_name() == "GameboyAdvance") {
        return 2;
    } else {
        return 3;
    }
}



void TitleScreenScene::put_module_text()
{
    text_.reset();
    redraw_margins();
    show_module_icons(module_page_);

    const auto st = calc_screen_tiles();
    StringBuffer<32> buffer;
    if (module_cursor_) {
        const auto index = module_page_ * modules_per_page + module_cursor_->x +
                           module_cursor_->y * modules_per_row;
        if (auto factory = detail::_Module::Factory::get(index, dev_)) {
            buffer += *loadstr(factory->name());
        } else {
            return;
        }
    } else {
        return;
    }

    const auto len = utf8::len(buffer.c_str());

    auto margin = centered_text_margins(buffer.length());
    text_.emplace(

        buffer.c_str(),
        OverlayCoord{u8(st.x - (len + margin)), u8(st.y - text_offset())});
}



void TitleScreenScene::put_menu_text()
{
    text_.reset();
    redraw_margins();

    const auto st = calc_screen_tiles();
    StringBuffer<32> buffer(SYS_CSTR(game_title));
    buffer += ":   ";
    const auto prefix_len = buffer.length();
    buffer += *loadstr(menu_text[menu_selection_]);

    const auto len = utf8::len(buffer.c_str());

    auto margin = centered_text_margins(utf8::len(buffer.c_str()));
    text_.emplace(
        buffer.c_str(),
        OverlayCoord{u8(st.x - (len + margin + 1)), u8(st.y - text_offset())});

    menu_selection_start_ = margin + 1 + prefix_len + (len % 2 ? 1 : 0);

    menu_selection_stop_ =
        margin + 1 + utf8::len(buffer.c_str()) + (len % 2 ? 1 : 0);

    PLATFORM.set_tile(
        Layer::overlay, menu_selection_start_ - 4, st.y - text_offset(), 375);

    PLATFORM.set_tile(
        Layer::overlay, menu_selection_stop_ - 1, st.y - text_offset(), 376);
}



void TitleScreenScene::run_init_scripts(bool allow_mods)
{
    // For some stuff, like the tutorial viewer, or multiplayer games,
    // everything would get messed up if we allowed users to run modded scripts.

    const bool use_rom_fs = (allow_mods == false);

    APP.invoke_script("/scripts/config/setup.lisp", use_rom_fs);

    if (allow_mods) {
        APP.invoke_ram_script("/mods/init.lisp");


        auto on_match = [&](const char* const path) {
            StringBuffer<64> path_str(path);
            StringBuffer<16> init_filename("init.lisp");

            if (ends_with(init_filename, path_str)) {
                StringBuffer<flash_filesystem::max_path> full_path("/dlc/");
                full_path += path_str;
                APP.invoke_ram_script(full_path.c_str());
            }
        };

        flash_filesystem::walk_directory("/dlc/", on_match);
    }
}



static int module_count(bool dev)
{
    int count = 0;

    auto current = detail::_Module::factories_;
    while (current) {
        ++count;
        current = current->next_;
    }

    if (dev) {
        current = detail::_Module::developer_mode_factories_;
        while (current) {
            ++count;
            current = current->next_;
        }
    }

    return count;
}



static int module_page_count(bool dev)
{
    const int count = module_count(dev);

    int full_pages = count / modules_per_page;
    int partial_page = (count % modules_per_page) not_eq 0;

    return full_pages + partial_page;
}



void TitleScreenScene::play_gust_sound()
{
    if (rng::choice<4>(rng::utility_state)) {
        if (PLATFORM.speaker().is_sound_playing("gust1")) {
            if (rng::choice<2>(rng::utility_state)) {
                PLATFORM.speaker().stop_sound("gust1");
                PLATFORM.speaker().play_sound("gust2", 1);
            } else {
                PLATFORM.speaker().stop_sound("gust1");
                PLATFORM.speaker().play_sound("gust1", 1);
            }
        } else {
            PLATFORM.speaker().play_sound("gust1", 1);
        }
    } else {
        if (PLATFORM.speaker().is_sound_playing("gust2")) {
            PLATFORM.speaker().stop_sound("gust2");
            PLATFORM.speaker().play_sound("gust1", 1);
        } else {
            PLATFORM.speaker().play_sound("gust2", 1);
        }
    }
}



u8 TitleScreenScene::module_page_;
Optional<Vec2<u8>> TitleScreenScene::module_cursor_;



static const int faded_music_volume = 2;



ScenePtr TitleScreenScene::update(Time delta)
{
    APP.update_parallax(delta);

    APP.player().update(delta);

    rng::get(rng::critical_state);
    rng::get(rng::utility_state);

    hover_timer_ += delta;

    const bool pong_sounds_on =
        (menu_selection_ == 4 or state_ == State::scroll_to_macro);
    pong_.update(pong_sounds_on);

    if (menu_selection_ == 3) {
        if (state_ not_eq State::scroll_archives and
            not PLATFORM.speaker().is_sound_playing("struttin")) {
            PLATFORM.speaker().stop_sound("creaking");
            PLATFORM.speaker().play_sound("struttin", 7);
        }
    } else if (not PLATFORM.speaker().is_sound_playing("creaking")) {
        PLATFORM.speaker().play_sound("creaking", 9);
    }


    {
        ambient_movement_ =
            4 * float(sine(1 * 3.14f * 0.0005f * hover_timer_ + 180)) /
            std::numeric_limits<s16>::max();

        const int offset = 60 + ambient_movement_;

        auto view = PLATFORM.screen().get_view();
        auto c = view.get_center();
        c.y = offset;
        view.set_center(c);
        PLATFORM.screen().set_view(view);

        bool macro_island_view = state_ == State::macro_island_enter or
                                 state_ == State::macro_island or
                                 state_ == State::macro_island_exit or
                                 state_ == State::macro_island_init;

        if (x_scroll_ < -240) {
            if (not macro_island_view) {
                set_scroll(Layer::map_0_ext,
                           x_scroll_ - 32,
                           -offset + 8 + v_scroll_1_);
            }
            set_scroll(
                Layer::map_1_ext, x_scroll_ - 272, -offset + 8 + v_scroll_2_);
        } else if (x_scroll_ < 0) {
            if (not macro_island_view) {
                set_scroll(
                    Layer::map_0_ext, x_scroll_, -offset + 8 + v_scroll_1_);
            }
            set_scroll(
                Layer::map_1_ext, x_scroll_ - 272, -offset + 8 + v_scroll_2_);
        } else if (x_scroll_ > 240) {
            if (not macro_island_view) {
                set_scroll(Layer::map_0_ext,
                           x_scroll_ + 32,
                           -offset + 8 + v_scroll_1_);
            }
            set_scroll(
                Layer::map_1_ext, x_scroll_ - 240, -offset + 8 + v_scroll_2_);
        } else {
            if (not macro_island_view) {
                set_scroll(
                    Layer::map_0_ext, x_scroll_, -offset + 8 + v_scroll_1_);
            }
            set_scroll(
                Layer::map_1_ext, x_scroll_ - 240, -offset + 8 + v_scroll_2_);
        }
    }


    if (menu_selection_ == 1) {
        island_mov_timer_ += delta;
        island_offset_ = 2 *
                         float(sine(2 * 3.14f * 0.0005f * hover_timer_ + 180)) /
                         std::numeric_limits<s16>::max();
        furnace_timer_ += delta;

        // Because the island in the background in the challenge mode room has a
        // specific priority set, sprites rendered at a higher priority will
        // cause overlapping background sprites to punch a hole in intermediary
        // tile layers. Do not create EmberParticle objects if they would
        // overlap with background sprites.
        const bool gba_sprite_priority_hardware_bug =
            (state_ == State::scroll_from_end and timer_ < milliseconds(600));

        if (furnace_timer_ > milliseconds(380) and
            not gba_sprite_priority_hardware_bug) {

            furnace_timer_ =
                -milliseconds(rng::choice<200>(rng::utility_state));
            Vec2<Fixnum> pos{Fixnum::from_integer(435),
                             Fixnum::from_integer(154)};
            pos = rng::sample<4>(pos, rng::utility_state);
            if (auto e = alloc_entity<EmberParticle>(this, pos, 51)) {
                APP.effects().push(std::move(e));
            }
        }
    } else if (menu_selection_ == 0) {
        bird_timer_ -= delta;
        if (bird_timer_ <= 0) {
            if (rng::choice<5>(rng::critical_state) == 0 and
                not PLATFORM.speaker().is_sound_playing("seagull_1.raw") and
                not PLATFORM.speaker().is_sound_playing("seagull_2.raw")) {
                if (rng::choice<2>(rng::critical_state)) {
                    PLATFORM.speaker().play_sound("seagull_1.raw", 0);
                } else {
                    PLATFORM.speaker().play_sound("seagull_2.raw", 0);
                }
            }
            bird_timer_ =
                seconds(12) + seconds(rng::choice<4>(rng::critical_state));
            if (rng::choice<6>(rng::critical_state)) {
                bird_timer_ =
                    milliseconds(1300) -
                    milliseconds(rng::choice<700>(rng::critical_state));
            }
            auto pos = Vec2<Fixnum>{Fixnum::from_integer(1),
                                    Fixnum::from_integer(130)};
            auto speed =
                (5 + rng::choice<4>(rng::critical_state)) / Float(100000);
            pos.y += Fixnum::from_integer(rng::choice<20>(rng::critical_state));
            if (auto e = alloc_entity<SmallBird>(pos, speed)) {
                APP.birds().push(std::move(e));
            }
        }
    } else if (menu_selection_ == 3) {
        if (state_ not_eq State::show_modules and
            state_ not_eq State::scroll_archives) {
            note_timer_ -= delta;
            if (note_timer_ < 0) {
                note_timer_ =
                    milliseconds(400) + milliseconds(500) -
                    milliseconds(rng::choice<700>(rng::critical_state));
                Vec2<Fixnum> pos{Fixnum::from_integer(552),
                                 Fixnum::from_integer(140)};
                u16 gfx = 62 + rng::choice<2>(rng::utility_state);
                if (auto e = alloc_entity<TitleScreenMusicNote>(pos, gfx)) {
                    APP.effects().push(std::move(e));
                }
            }
        }
    }

    for (auto& bird : APP.birds()) {
        if (bird->sprite().get_position().x >
            Fixnum::from_integer(
                (scale_offset().x + (197 + 16) - x_scroll_ / 2))) {
            bird->kill();
        }
    }
    update_entities(delta, APP.birds());
    update_entities(delta, APP.effects());


    switch (state_) {
    case State::macro_island_enter:
        timer_ += delta;

        if (PLATFORM.keyboard().pressed<Key::action_1>() or
            APP.player().tap_released()) {
            timer_ = 0;
            state_ = State::macro_island_exit;
            repeat_action1_ = true;
            PLATFORM.speaker().stream_music("unaccompanied_wind", 0);
            PLATFORM.speaker().play_sound("button_wooden", 3);

            const auto amount =
                1.f - smoothstep(0.f, milliseconds(1500), timer_);
            timer_ = amount * milliseconds(550);
        }

        if (APP.player().key_pressed(Key::left) or
            APP.player().key_pressed(Key::up)) {
            state_ = State::macro_island_exit;
            repeat_left_ = true;
            const auto amount =
                1.f - smoothstep(0.f, milliseconds(1500), timer_);
            timer_ = amount * milliseconds(550);
            break;
        }
        if (APP.player().key_pressed(Key::down) or
            APP.player().key_pressed(Key::right)) {
            state_ = State::macro_island_exit;
            repeat_right_ = true;
            const auto amount =
                1.f - smoothstep(0.f, milliseconds(1500), timer_);
            timer_ = amount * milliseconds(550);
            break;
        }

        if (timer_ > milliseconds(1500)) {
            state_ = State::macro_island;
            set_scroll(Layer::map_0_ext, -4, 192);
        } else {
            const auto amount =
                1.f - smoothstep(0.f, milliseconds(1500), timer_);
            auto scroll = 192 - 192 * amount;
            set_scroll(Layer::map_0_ext, -4, clamp((int)scroll, 40, 192));
        }

        break;

    case State::macro_island_exit:
        timer_ += delta;
        if (timer_ > milliseconds(400)) {
            state_ = State::wait;
            timer_ = 0;
            const auto offset = 60 + ambient_movement_;
            set_scroll(Layer::map_0_ext, x_scroll_, -offset + 8);
            PLATFORM.screen().set_shader(passthrough_shader);
            PLATFORM.screen().set_shader_argument(0);
            PLATFORM.override_priority(Layer::map_1, 2);
        } else {
            const auto amount = smoothstep(0.f, milliseconds(550), timer_);
            auto scroll = 192 - 192 * amount;
            set_scroll(Layer::map_0_ext, -4, clamp((int)scroll, 40, 192));
        }
        break;

    case State::macro_island:

        if (PLATFORM.keyboard().pressed<Key::action_1>() or
            APP.player().tap_released()) {
            timer_ = 0;
            state_ = State::macro_island_exit;
            repeat_action1_ = true;
            PLATFORM.speaker().stream_music("unaccompanied_wind", 0);
            PLATFORM.speaker().play_sound("button_wooden", 3);
        }

        if (APP.player().key_pressed(Key::left) or
            APP.player().key_pressed(Key::up)) {
            timer_ = 0;
            state_ = State::macro_island_exit;
            repeat_left_ = true;
        }
        if (APP.player().key_pressed(Key::down) or
            APP.player().key_pressed(Key::right)) {
            timer_ = 0;
            state_ = State::macro_island_exit;
            repeat_right_ = true;
        }

        selector_timer_ += delta;
        if (selector_timer_ > milliseconds(100)) {
            selector_timer_ = 0;
            selector_shaded_ = not selector_shaded_;

            const auto st = calc_screen_tiles();
            if (selector_shaded_) {
                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_start_ - 4,
                                  st.y - text_offset(),
                                  373);

                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_stop_ - 1,
                                  st.y - text_offset(),
                                  374);
            } else {
                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_start_ - 4,
                                  st.y - text_offset(),
                                  375);

                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_stop_ - 1,
                                  st.y - text_offset(),
                                  376);
            }
        }
        break;

    case State::resume_challenges:
        state_ = State::quick_fade_in;
        menu_selection_ = 1;
        x_scroll_ = 240;
        break;

    case State::resume_end:
        state_ = State::quick_fade_in;
        menu_selection_ = 3;
        PLATFORM.speaker().set_music_volume(faded_music_volume);
        PLATFORM.load_tile0_texture("skyland_title_3_flattened");
        x_scroll_ = 480;
        break;

    case State::resume_macro:
        state_ = State::quick_fade_in;
        menu_selection_ = 2;
        timer_ = 0;
        PLATFORM.load_tile1_texture("skyland_title_2_flattened");
        x_scroll_ = -240;
        break;


    case State::quick_fade_in: {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(700);
        if (timer_ > fade_duration) {
            PLATFORM.screen().schedule_fade(0.f);
            state_ = State::wait;
            put_menu_text();
            timer_ = 0;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }


    case State::fade_in: {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(1100);
        if (timer_ > fade_duration) {
            PLATFORM.screen().schedule_fade(0.f);
            state_ = State::wait;
            put_menu_text();
            timer_ = 0;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            if (flower_effect_) {
                PLATFORM.screen().schedule_fade(
                    amount, ColorConstant::rich_black, false, true);
            } else {
                PLATFORM.screen().schedule_fade(
                    amount, ColorConstant::rich_black, true, true);
            }
        }
        break;
    }

    case State::wait:

        selector_timer_ += delta;
        if (selector_timer_ > milliseconds(100)) {
            selector_timer_ = 0;
            selector_shaded_ = not selector_shaded_;

            const auto st = calc_screen_tiles();
            if (selector_shaded_) {
                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_start_ - 4,
                                  st.y - text_offset(),
                                  373);

                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_stop_ - 1,
                                  st.y - text_offset(),
                                  374);
            } else {
                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_start_ - 4,
                                  st.y - text_offset(),
                                  375);

                PLATFORM.set_tile(Layer::overlay,
                                  menu_selection_stop_ - 1,
                                  st.y - text_offset(),
                                  376);
            }
        }

        if (repeat_action1_ or PLATFORM.keyboard().pressed<Key::action_1>() or
            APP.player().tap_released()) {
            state_ = State::fade_out;
            if (menu_selection_ == 3) {
                state_ = State::fade_modules_1;
            } else {
                if (not repeat_action1_) {
                    PLATFORM.speaker().stream_music("unaccompanied_wind", 0);
                    PLATFORM.speaker().play_sound("button_wooden", 3);
                }
                module_cursor_.reset();
                module_page_ = 0;
            }
            repeat_action1_ = false;
        }

        if (menu_selection_ == 2) {
            timer_ += delta;
            if (timer_ > milliseconds(700)) {
                state_ = State::macro_island_init;
                timer_ = 0;
                break;
            }
        }

        if (repeat_right_ or APP.player().key_pressed(Key::right) or
            APP.player().key_pressed(Key::down) or
            (APP.player().touch_held(milliseconds(150)) and
             APP.player().touch_velocity().x and
             APP.player().touch_velocity().x * delta < -0.08f)) {
            repeat_right_ = false;
            if (menu_selection_ == 0) {
                menu_selection_ = 1;
                put_menu_text();
                play_gust_sound();
                state_ = State::scroll_right;
                timer_ = 0;
            } else if (menu_selection_ == 2) {
                menu_selection_ = 0;
                put_menu_text();
                play_gust_sound();
                state_ = State::scroll_to_center;
                PLATFORM_EXTENSION(force_vsync);
                PLATFORM.load_tile0_texture("skyland_title_0_flattened");
                __draw_image(31, 0, 3, 30, 14, Layer::map_0);
                timer_ = 0;
            } else if (menu_selection_ == 1) {
                menu_selection_ = 3;
                put_menu_text();
                state_ = State::scroll_to_end;
                play_gust_sound();
                timer_ = 0;
                PLATFORM_EXTENSION(force_vsync);
                PLATFORM.load_tile0_texture("skyland_title_3_flattened");
            } else if (menu_selection_ == 4) {
                menu_selection_ = 2;
                put_menu_text();
                state_ = State::scroll_to_macro;
                play_gust_sound();
                timer_ = 0;
            }
        }
        if (repeat_left_ or APP.player().key_pressed(Key::left) or
            APP.player().key_pressed(Key::up) or
            APP.player().touch_velocity().x * delta > 0.08f) {
            repeat_left_ = false;
            if (menu_selection_ == 1) {
                menu_selection_ = 0;
                put_menu_text();
                state_ = State::scroll_left;
                play_gust_sound();
                timer_ = 0;
            } else if (menu_selection_ == 0) {
                menu_selection_ = 2;
                put_menu_text();
                state_ = State::scroll_macro;
                play_gust_sound();
                PLATFORM_EXTENSION(force_vsync);
                PLATFORM.load_tile1_texture("skyland_title_2_flattened");
                timer_ = 0;
            } else if (menu_selection_ == 3) {
                menu_selection_ = 1;
                put_menu_text();
                play_gust_sound();
                state_ = State::scroll_from_end;
                timer_ = 0;
            } else if (menu_selection_ == 2) {
                menu_selection_ = 4;
                put_menu_text();
                play_gust_sound();
                state_ = State::scroll_multiplayer;
                PLATFORM_EXTENSION(force_vsync);
                PLATFORM.load_tile0_texture("skyland_title_4_flattened");
                __draw_image(31, 0, 3, 30, 14, Layer::map_0);
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
            x_scroll_ = -480;
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = -240 + -240 * amount;
        }
        break;
    }

    case State::scroll_to_macro: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = -240;
        } else {
            const auto amount = 1.f - smoothstep(0.f, duration, timer_);
            x_scroll_ = -240 + -240 * amount;
        }
        break;
    }

    case State::macro_island_init:
        timer_ = 0;
        state_ = State::macro_island_enter;

        PLATFORM.override_priority(Layer::map_1, 1);

        PLATFORM.screen().set_shader(
            [](ShaderPalette p, ColorConstant k, int var, int index) {
                if (index == 0) {
                    return k;
                }

                auto blend =
                    [](ColorConstant from, ColorConstant to, u8 interp) {
                        const Color input(from);
                        const Color k2(to);
                        Color result(fast_interpolate(input.r_, k2.r_, interp),
                                     fast_interpolate(input.g_, k2.g_, interp),
                                     fast_interpolate(input.b_, k2.b_, interp));
                        return result.hex();
                    };

                // 0x63b5e7;

                if (p == ShaderPalette::tile0) {
                    return blend(k, custom_color(0x63b5e7), 128);
                }

                return k;
            });
        PLATFORM.screen().set_shader_argument(1);

        PLATFORM.load_tile0_texture("macro_rendertexture");
        macro_gen_sample_island();
        break;

    case State::scroll_macro: {
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
            PLATFORM_EXTENSION(force_vsync);
            PLATFORM.load_tile1_texture("skyland_title_1_flattened");
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
            PLATFORM.speaker().set_music_volume(faded_music_volume);
            state_ = State::wait;
            x_scroll_ = 480;
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            x_scroll_ = 240 + 240 * amount;

            static const auto max_vol = Platform::Speaker::music_volume_max;
            const auto low = faded_music_volume;
            const auto vol_diff = Platform::Speaker::music_volume_max - low;
            PLATFORM.speaker().set_music_volume(max_vol - vol_diff * amount);
            PLATFORM.speaker().set_sounds_volume(max_vol * amount);
        }
        break;
    }


    case State::scroll_from_end: {
        timer_ += delta;
        static const auto duration = milliseconds(1250);
        static const auto max_vol = Platform::Speaker::music_volume_max;
        if (timer_ > duration) {
            timer_ = 0;
            state_ = State::wait;
            x_scroll_ = 240;
            PLATFORM_EXTENSION(force_vsync);
            PLATFORM.load_tile0_texture("skyland_title_0_flattened");
            PLATFORM.speaker().set_music_volume(max_vol);
            PLATFORM.speaker().set_sounds_volume(max_vol);
            PLATFORM.speaker().clear_sounds();
        } else {
            const auto amount = smoothstep(0.f, duration, timer_);
            const auto low = faded_music_volume;
            const auto vol_diff = Platform::Speaker::music_volume_max - low;
            PLATFORM.speaker().set_music_volume(low + vol_diff * amount);
            PLATFORM.speaker().set_sounds_volume(max_vol - max_vol * amount);
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
        constexpr auto fade_duration = milliseconds(600);
        if (timer_ > fade_duration) {

            PLATFORM.screen().set_shader(passthrough_shader);
            PLATFORM.screen().set_shader_argument(0);
            PLATFORM.override_priority(Layer::map_1, 2);

            text_.reset();
            if (menu_selection_ not_eq 0) {
                PLATFORM.speaker().clear_sounds();
            }
            switch (menu_selection_) {
            case 0: {
                APP.game_mode() = App::GameMode::adventure;
                run_init_scripts(true);

                auto tutorial_flag = GlobalPersistentData::tutorial_prompt;

                if (APP.gp_.stateflags_.get(tutorial_flag)) {
                    return make_scene<StartAdventureScene>();
                } else {

                    module_cursor_ = {0, 0};
                    module_page_ = 1;

                    PLATFORM.speaker().clear_sounds();
                    APP.gp_.stateflags_.set(tutorial_flag, true);

                    // Title Screen Graphical bugfix (1)
                    // Text box doesn't show up, need to cycle a fade to fix the
                    // palettes.
                    PLATFORM.screen().fade(0.95f);
                    PLATFORM.screen().fade(1.f);

                    save::store_global_data(APP.gp_);
                    APP.invoke_script("/scripts/reset_hooks.lisp");
                    auto dialog =
                        allocate_dynamic<DialogString>("dialog-buffer");
                    *dialog = SYS_CSTR(dialog_tutorial_prompt);

                    auto& cursor_loc = globals().near_cursor_loc_;
                    cursor_loc.x = 0;
                    cursor_loc.y = 14;

                    auto next =
                        make_scene<BoxedDialogSceneWS>(std::move(dialog));

                    return next;
                }
                break;
            }

            case 1: {
                APP.game_mode() = App::GameMode::challenge;
                run_init_scripts(true);
                return make_scene<SelectChallengeScene>();
            }

            case 2:
                // Macrocosm mode does not use the room pool memory created for
                // the 2d combat verison of skyland, unload the pool so that we
                // have more memory to work with.
                // We could create the room pool on demand only for game modes
                // that need the memory pool, but most game modes need the pool,
                // so easier to create it by default and destroy as needed.
                globals().room_pools_.destroy();

                // Clear out any references to entities before destroying the
                // entity pool, as the pool implementation will raise a fatal
                // error if you attempt to destroy a pool without returning all
                // elements allocated from it (to avoid dangling pointer
                // problems). Only the App class and the Island class store
                // EntityLists, and the Island wouldn't store any references to
                // entities, as we called Island::clear() when entering this
                // scene.
                APP.birds().clear();
                APP.effects().clear();

                globals().entity_pools_.create("macro-entity-pool", 1);

                APP.invoke_script("/scripts/reset_hooks.lisp");

                return make_scene<MacrocosmFreebuildModule>();

            case 3:
                PLATFORM.fatal("logic error, this should be unreachable");

            case 4: {
                APP.game_mode() = App::GameMode::multiplayer;
                run_init_scripts(false);

                return MultiplayerConnectScene::setup();
            }
            }
        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);


            // Erase text when text fade nears completion.
            if (amount > 0.9f) {
                u8 count = calc_screen_tiles().x - text_->coord().x;
                text_->assign(StringBuffer<32>(' ', count).c_str());
            }


            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }


    case State::fade_modules_backout: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(300);
        if (timer_ > fade_duration) {
            put_menu_text();

            PLATFORM.screen().schedule_fade(0.f);

            state_ = State::wait;
            timer_ = 0;

            PLATFORM.speaker().set_music_volume(faded_music_volume);

        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);

            PLATFORM.screen().schedule_fade(
                0.7f - 0.7f * amount, ColorConstant::rich_black, true, true);
        }
        break;
    }

    case State::fade_modules_1: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(500);

        if (timer_ > fade_duration) {
            APP.effects().clear();
            redraw_margins();
            show_module_icons(module_page_);
            state_ = State::show_modules;

            PLATFORM.speaker().set_music_volume(2);

            PLATFORM.screen().schedule_fade(
                0.7f, ColorConstant::rich_black, true, true);

            if (not module_cursor_) {
                module_cursor_ = {0, 0};
            }

            put_module_text();

        } else {
            auto amount = smoothstep(0.f, fade_duration, timer_);

            PLATFORM.screen().schedule_fade(
                0.7f * amount, ColorConstant::rich_black, true, true);
        }
        break;
    }


    case State::show_modules:
        selector_timer_ += delta;
        if (selector_timer_ > milliseconds(200)) {
            selector_timer_ -= milliseconds(200);
            selector_shaded_ = not selector_shaded_;
        }

        PLATFORM.screen().schedule_fade(
            0.69f, ColorConstant::rich_black, false, false);
        if (APP.player().key_down(Key::action_2)) {
            state_ = State::fade_modules_backout;
            timer_ = 0;
            PLATFORM.fill_overlay(0);
            redraw_margins();
        } else if (module_cursor_) {

            auto test_key = [&](Key k) {
                return APP.player().test_key(
                    k, milliseconds(500), milliseconds(100));
            };

            auto click_sound = [&] {
                PLATFORM.speaker().play_sound("click_wooden", 2);
            };
            if (test_key(Key::right)) {
                if (module_cursor_->x < 2) {
                    module_cursor_->x += 1;
                    click_sound();
                } else if (module_page_ < module_page_count(dev_) - 1) {
                    PLATFORM.fill_overlay(0);
                    redraw_margins();
                    module_cursor_->x = 0;
                    module_page_++;
                    click_sound();
                }
                put_module_text();
            } else if (test_key(Key::left)) {
                if (module_cursor_->x > 0) {
                    module_cursor_->x -= 1;
                    click_sound();
                } else if (module_page_ > 0) {
                    PLATFORM.fill_overlay(0);
                    redraw_margins();
                    module_cursor_->x = 2;
                    --module_page_;
                    click_sound();
                }
                put_module_text();
            }
            if (test_key(Key::up) and module_cursor_->y > 0) {
                module_cursor_->y -= 1;
                put_module_text();
                click_sound();
            } else if (test_key(Key::down) and module_cursor_->y < 1) {
                module_cursor_->y += 1;
                put_module_text();
                click_sound();
            }

            if (APP.player().key_down(Key::action_1)) {

                auto index = module_page_ * modules_per_page +
                             module_cursor_->x +
                             module_cursor_->y * modules_per_row;
                if (auto f = detail::_Module::Factory::get(index, dev_)) {
                    PLATFORM.speaker().clear_sounds();
                    PLATFORM.speaker().stream_music("unaccompanied_wind", 0);
                    PLATFORM.speaker().play_sound("button_wooden", 3);

                    if (f->name() == SystemString::module_cart_viewer) {
                        state_ = State::scroll_archives;
                        PLATFORM.screen().schedule_fade(0);
                        PLATFORM.fill_overlay(0);
                        redraw_margins();
                        PLATFORM.screen().schedule_fade(0.f);
                        timer_ = 0;
                        PLATFORM.load_tile1_texture(
                            "skyland_title_5_flattened");
                        PLATFORM.speaker().play_sound("gust2", 3);
                        break;
                    }

                    PLATFORM.fill_overlay(0);
                    PLATFORM_EXTENSION(force_vsync);
                    PLATFORM.screen().fade(
                        1.f, ColorConstant::rich_black, {}, true, true);
                    APP.game_mode() = App::GameMode::challenge;

                    if (f->run_scripts()) {
                        run_init_scripts(f->enable_custom_scripts());
                    }

                    return f->create();
                }
            }
        }
        break;

    case State::scroll_archives: {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(1500);
        const auto amount = smoothstep(0.f, fade_duration, timer_);
        PLATFORM.screen().schedule_fade(amount);

        v_scroll_1_ = 120 * amount;
        v_scroll_2_ = 120 * amount - 113;

        ambient_movement_ =
            4 * float(sine(1 * 3.14f * 0.0005f * hover_timer_ + 180)) /
            std::numeric_limits<s16>::max();

        const int offset = 60 + ambient_movement_;

        set_scroll(Layer::map_0_ext, x_scroll_ + 32, -offset + 8 + v_scroll_1_);
        set_scroll(Layer::map_1_ext, x_scroll_ + 32, -offset + 8 + v_scroll_2_);

        if (timer_ > fade_duration) {
            auto index = module_page_ * modules_per_page + module_cursor_->x +
                         module_cursor_->y * modules_per_row;
            if (auto f = detail::_Module::Factory::get(index, dev_)) {
                return f->create();
            }
        }
        break;
    }

    case State::wait_2:
        break;
    }

    return null_scene();
}



void TitleScreenScene::macro_gen_sample_island()
{
    APP.macrocosm().emplace();
    APP.macrocosm()->emplace<macro::EngineImpl>(&APP);

    macro::raster::globalstate::_upper_half_only = true;

    __draw_image(0, 0, 0, 30, 16, Layer::map_0);
    auto& m = macrocosm();
    m.make_sector({0, 1}, macro::terrain::Sector::Shape::pancake);
    m.bind_sector({0, 1});
    auto& s = m.sector();

    PLATFORM.set_background_task(parallax_background_task);

    APP.invoke_script("/scripts/config/title_screen_isle.lisp");
    s.render();

    PLATFORM.set_background_task(nullptr);

    macro::raster::globalstate::_upper_half_only = false;

    APP.macrocosm().reset();
}



void TitleScreenScene::show_module_icons(int page)
{
    // left arrow icon
    PLATFORM.set_tile(Layer::overlay, 1, 8, module_page_ == 0 ? 175 : 173);

    // right arrow icon
    PLATFORM.set_tile(Layer::overlay,
                      calc_screen_tiles().x - 2,
                      8,
                      module_page_ == module_page_count(dev_) - 1 ? 174 : 172);


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
        auto x_start = (4 + x * 8) + scale_offset().x / 8;
        auto y_start = (2 + y * 8) + scale_offset().y / 8;

        const auto index = page * modules_per_page + x + y * modules_per_row;
        if (auto f = detail::_Module::Factory::get(index, dev_)) {

            for (int x = 0; x < 6; ++x) {
                for (int y = 0; y < 6; ++y) {
                    PLATFORM.set_tile(
                        Layer::overlay, x_start + x, y_start + y, 470);
                }
            }

            PLATFORM.set_tile(Layer::overlay, x_start, y_start, 466);
            PLATFORM.set_tile(Layer::overlay, x_start + 5, y_start, 467);
            PLATFORM.set_tile(Layer::overlay, x_start, y_start + 5, 468);
            PLATFORM.set_tile(Layer::overlay, x_start + 5, y_start + 5, 469);

            auto icon = alloc_icon();
            PLATFORM.load_overlay_chunk(icon, f->icon(), 16);
            draw_image(icon, x_start + 1, y_start + 1, 4, 4, Layer::overlay);
        }
    };

    for (int x = 0; x < 3; ++x) {
        for (int y = 0; y < 2; ++y) {
            show_icon(x, y);
        }
    }
}



void TitleScreenScene::display()
{
    if (flower_effect_ and state_ == State::fade_in) {
        auto amount = smoothstep(milliseconds(-400), milliseconds(800), timer_);

        auto darken_amount =
            smoothstep(milliseconds(-400), milliseconds(200), timer_);

        IntroCreditsScene::show_sunflowers(32 * amount, darken_amount);
        return;
    }

    if (x_scroll_ > 160) {
        Sprite sprite;

        Vec2<Fixnum> pos{Fixnum(135 - x_scroll_ / 3) +
                             Fixnum::from_integer(island_offset_),
                         Fixnum(110 - 0.25f * (240 - x_scroll_))};
        auto scl = spr_scale_offset();
        pos.x += Fixnum::from_integer(scl.x);
        pos.y += Fixnum::from_integer(scl.y);
        sprite.set_position(pos);
        sprite.set_priority(3);
        sprite.set_texture_index(6);

        PLATFORM.screen().draw(sprite);
    } else if (x_scroll_ < -240) {
        pong_.display(x_scroll_);

        Sprite dog_spr;
        auto view = PLATFORM.screen().get_view();
        auto c = view.get_center();
        Vec2<Fixnum> anchor = {Fixnum(163.f), Fixnum((c.y - 55) + 177)};
        anchor.x -= Fixnum::from_integer(480 + x_scroll_);
        anchor.y += Fixnum::from_integer(x_scroll_ / 16);

        if (++dog_anim_cnt_ == 6) {
            dog_anim_cnt_ = 0;
            ++dog_head_frame_;
            ++dog_tail_frame_;

            if (dog_head_frame_ > 1) {
                dog_head_frame_ = 0;
            }

            if (dog_tail_frame_ > 5) {
                dog_tail_frame_ = 0;
            }
        }

        if (dog_) {
            dog_spr.set_position(anchor);
            dog_spr.set_size(Sprite::Size::w32_h32);
            dog_spr.set_texture_index(32 + dog_head_frame_);
            PLATFORM.screen().draw(dog_spr);

            anchor.x += 32.0_fixed;
            dog_spr.set_texture_index(34);
            dog_spr.set_position(anchor);
            PLATFORM.screen().draw(dog_spr);

            anchor.y += 32.0_fixed;
            dog_spr.set_texture_index(36);
            dog_spr.set_position(anchor);
            PLATFORM.screen().draw(dog_spr);

            anchor.x -= 32.0_fixed;
            dog_spr.set_texture_index(35);
            dog_spr.set_position(anchor);
            PLATFORM.screen().draw(dog_spr);

            anchor.y -= 28.0_fixed;
            anchor.x += 42.0_fixed;
            dog_spr.set_texture_index(37 + dog_tail_frame_);
            dog_spr.set_position(anchor);
            PLATFORM.screen().draw(dog_spr);
        }
    }
    for (auto& effect : APP.effects()) {
        auto spr = effect->sprite();
        auto pos = spr.get_position();
        pos.x = pos.x - Fixnum::from_integer(x_scroll_);
        spr.set_position(pos);
        PLATFORM.screen().draw(spr);
    }
    for (auto& bird : APP.birds()) {
        auto spr = bird->sprite();
        auto pos = spr.get_position();

        if (state_ == State::scroll_to_center and
            pos.x < Fixnum::from_integer(-x_scroll_)) {
            continue;
        }

        pos.x = pos.x - Fixnum::from_integer(x_scroll_ / 2);

        // FIXME!!!! checked cast.
        auto b = static_cast<SmallBird*>(bird.get());

        const auto ambient_movement =
            8 *
            float(
                sine(((b->speed() * 100000) / 2) * 3.14f * 0.0005f * b->age() +
                     180)) /
            std::numeric_limits<s16>::max();

        pos.y += Fixnum(ambient_movement);
        auto scl = spr_scale_offset();
        pos.x += Fixnum::from_integer(scl.x);
        pos.y += Fixnum::from_integer(scl.y);

        spr.set_position(pos);
        PLATFORM.screen().draw(spr);
    }

    if (module_cursor_ and (state_ == State::show_modules)) {
        Sprite sprite;
        sprite.set_texture_index(36);
        sprite.set_size(Sprite::Size::w16_h32);
        sprite.set_origin({8, 8});
        sprite.set_priority(0);


        Vec2<Fixnum> scl;
        scl.x = scale_offset().x;
        scl.y = scale_offset().y;


        sprite.set_position(Vec2<Fixnum>{Fixnum(28 + 64.f * module_cursor_->x +
                                                (selector_shaded_ ? 1 : 0)),
                                         Fixnum(ambient_movement_ + 60 + 8 +
                                                64.f * module_cursor_->y +
                                                (selector_shaded_ ? 1 : 0))} +
                            scl);
        PLATFORM.screen().draw(sprite);


        sprite.set_flip({true, false});
        sprite.set_position(Vec2<Fixnum>{Fixnum(83 + 64.f * module_cursor_->x -
                                                (selector_shaded_ ? 1 : 0)),
                                         Fixnum(ambient_movement_ + 60 + 8 +
                                                64.f * module_cursor_->y +
                                                (selector_shaded_ ? 1 : 0))} +
                            scl);
        PLATFORM.screen().draw(sprite);


        sprite.set_flip({false, true});
        sprite.set_position(Vec2<Fixnum>{Fixnum(28 + 64.f * module_cursor_->x +
                                                (selector_shaded_ ? 1 : 0)),
                                         Fixnum(ambient_movement_ + 60 + 8 +
                                                64.f * module_cursor_->y + 40 -
                                                (selector_shaded_ ? 1 : 0))} +
                            scl);
        PLATFORM.screen().draw(sprite);


        sprite.set_flip({true, true});
        sprite.set_position(Vec2<Fixnum>{Fixnum(83 + 64.f * module_cursor_->x -
                                                (selector_shaded_ ? 1 : 0)),
                                         Fixnum(ambient_movement_ + 60 + 8 +
                                                64.f * module_cursor_->y + 40 -
                                                (selector_shaded_ ? 1 : 0))} +
                            scl);
        PLATFORM.screen().draw(sprite);
    }
}



void TitleScreenScene::Pong::update(bool sound_effects)
{
    if (ball_.x >= 23) {
        if (sound_effects) {
            PLATFORM.speaker().play_sound("pong_blip_1", 0);
        }
        ball_speed_.x *= -1;
    }
    if (ball_.y >= 20) {
        if (sound_effects) {
            PLATFORM.speaker().play_sound("pong_blip_2", 0);
        }
        ball_speed_.y *= -1;
    }
    if (ball_.y < 1) {
        if (sound_effects) {
            PLATFORM.speaker().play_sound("pong_blip_2", 0);
        }
        ball_speed_.y *= -1;
    }
    if (ball_.x < 1) {
        if (sound_effects) {
            PLATFORM.speaker().play_sound("pong_blip_1", 0);
        }
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

    pad_trail_timer_ += milliseconds(16);
    if (pad_trail_timer_ > milliseconds(50)) {
        pad_trail_timer_ -= milliseconds(50);

        if (pad1_trail_.full()) {
            pad1_trail_.erase(pad1_trail_.begin());
        }

        if (pad2_trail_.full()) {
            pad2_trail_.erase(pad2_trail_.begin());
        }

        pad1_trail_.push_back(pad1_.pos_);
        pad2_trail_.push_back(pad2_.pos_);
    }

    trail_timer_ += milliseconds(16);
    if (trail_timer_ > milliseconds(30)) {
        trail_timer_ -= milliseconds(30);

        if (ball_trail_.full()) {
            ball_trail_.erase(ball_trail_.begin());
        }
        ball_trail_.push_back(ball_);
    }


    ball_.x += ball_speed_.x;
    ball_.y += ball_speed_.y;

    pad1_.pos_ += pad1_.speed_;
    pad2_.pos_ += pad2_.speed_;
}



void TitleScreenScene::Pong::display(int x_scroll)
{
    auto view = PLATFORM.screen().get_view();
    auto c = view.get_center();

    const Vec2<Fixnum> anchor = {Fixnum(73.f), Fixnum((c.y - 64) + 148)};

    Vec2<Fixnum> scl;
    scl.x = spr_scale_offset().x;
    scl.y = spr_scale_offset().y;

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(26);
    sprite.set_origin({1, 2});


    sprite.set_position(
        Vec2<Fixnum>{
            Fixnum((anchor.x) - Fixnum::from_integer(480 + x_scroll)),
            Fixnum(anchor.y +
                   Fixnum::from_integer(clamp(
                       interpolate(ball_.y, pad1_.pos_, 1.f - ball_.x / 22),
                       0.f,
                       18.f)))} +
        scl);
    PLATFORM.screen().draw(sprite);

    u8 blend = 0;
    for (auto& p : reversed(pad1_trail_)) {
        blend += 48;
        sprite.set_position(
            Vec2<Fixnum>{
                Fixnum((anchor.x) - Fixnum::from_integer(480 + x_scroll)),
                Fixnum(anchor.y +
                       Fixnum::from_integer(
                           clamp(interpolate(ball_.y, p, 1.f - ball_.x / 22),
                                 0.f,
                                 18.f)))} +
            scl);
        sprite.set_mix({custom_color(0x236f5b), blend});
        PLATFORM.screen().draw(sprite);
    }

    sprite.set_mix({});

    sprite.set_position(
        Vec2<Fixnum>{Fixnum((anchor.x + 24.0_fixed) -
                            Fixnum::from_integer(480 + x_scroll)),
                     anchor.y + Fixnum::from_integer(interpolate(
                                    ball_.y, pad2_.pos_, ball_.x / 22))} +
        scl);

    PLATFORM.screen().draw(sprite);

    blend = 0;
    for (auto& p : reversed(pad2_trail_)) {
        blend += 48;
        sprite.set_position(
            Vec2<Fixnum>{Fixnum((anchor.x + 24.0_fixed) -
                                Fixnum::from_integer(480 + x_scroll)),
                         anchor.y + Fixnum::from_integer(interpolate(
                                        ball_.y, p, ball_.x / 22))} +
            scl);

        sprite.set_mix({custom_color(0x236f5b), blend});
        PLATFORM.screen().draw(sprite);
    }

    sprite.set_mix({});

    sprite.set_origin({});
    sprite.set_texture_index(27);
    sprite.set_position(
        Vec2<Fixnum>{(Fixnum::from_integer(ball_.x) + anchor.x) -
                         Fixnum::from_integer(480 + x_scroll),
                     Fixnum::from_integer(ball_.y) + anchor.y} +
        scl);
    PLATFORM.screen().draw(sprite);

    blend = 0;
    for (auto& b : reversed(ball_trail_)) {
        blend += 48;
        sprite.set_position(
            Vec2<Fixnum>{(Fixnum::from_integer(b.x) + anchor.x) -
                             Fixnum::from_integer(480 + x_scroll),
                         Fixnum::from_integer(b.y) + anchor.y} +
            scl);
        sprite.set_mix({custom_color(0x236f5b), blend});
        PLATFORM.screen().draw(sprite);
    }
}



Conf::String TitleScreenScene::music_track()
{
    auto fd = PLATFORM.load_file("scripts/data", "environment.ini");
    if (not fd.second) {
        PLATFORM.fatal("missing music config file!");
    }

    Conf c;
    auto v = c.get(fd.first, "title_screen", "music");

    return std::move(*std::get_if<Conf::String>(&v));
}



} // namespace skyland
