#include "titleScreenScene.hpp"
#include "multiplayerConnectScene.hpp"
#include "newgameScene.hpp"
#include "script/lisp.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/birbs/smolBirb.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "zoneImageScene.hpp"



namespace skyland {



void init_clouds(Platform& pfrm);



// I needed to cram the cloud texture into a little gap in another one of the
// textures, which meant adjusting the positions of certain vram tiles. I ended
// up copy pasting the other function and changing the tile indices, because I'm
// making this for a game jam and I'm running out of time.
static void init_clouds2(Platform& pfrm);



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



void TitleScreenScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().fade(1.f);
    const int offset = 64;

    init_clouds2(pfrm);
    pfrm.enable_feature("v-parallax", false);

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

    __draw_image(pfrm, 1, 0, 3, 30, 14, Layer::map_1);
    __draw_image(pfrm, 1, 0, 3, 30, 14, Layer::map_0);

    window_image_hack(pfrm);

    pfrm.set_scroll(Layer::map_1_ext, 0, -offset + 8);

    pfrm.set_overlay_origin(0, 4);
}



void TitleScreenScene::window_image_hack(Platform& pfrm)
{
    // We needed to cram the textures for the scrolling background image into
    // the texture for another background layer (in this case, into the empty
    // space inside the window image). But, we draw the layer as an image, and
    // we need to mask out all of the bits that we don't want to be visible.
    const int empty_tile = 2;
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

    pfrm.enable_feature("v-parallax", true);

    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");
    pfrm.load_sprite_texture("spritesheet");


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



[[maybe_unused]] static const char* menu_text[2]{"adventure", "challenge"};



void TitleScreenScene::put_menu_text(Platform& pfrm)
{
    redraw_margins(pfrm);

    const auto st = calc_screen_tiles(pfrm);
    StringBuffer<32> buffer("SKYLAND:   ");
    const auto prefix_len = buffer.length();
    buffer += menu_text[menu_selection_];
    auto margin = centered_text_margins(pfrm, buffer.length());
    text_.emplace(
        pfrm,
        buffer.c_str(),
        OverlayCoord{u8(st.x - (buffer.length() + margin + 1)), u8(st.y - 2)});

    menu_selection_start_ = margin + 1 + prefix_len;
    menu_selection_stop_ = margin + 1 + buffer.length();

    pfrm.set_tile(Layer::overlay, menu_selection_start_ - 4, st.y - 2, 375);
    pfrm.set_tile(Layer::overlay, menu_selection_stop_ - 1, st.y - 2, 376);
}



ScenePtr<Scene>
TitleScreenScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    rng::get(rng::critical_state);

    hover_timer_ += delta;

    {
        const auto ambient_movement =
            4 * float(sine(1 * 3.14f * 0.0005f * hover_timer_ + 180)) /
            std::numeric_limits<s16>::max();

        const int offset = 60 + ambient_movement;

        auto view = pfrm.screen().get_view();
        auto c = view.get_center();
        c.y = offset;
        view.set_center(c);
        pfrm.screen().set_view(view);

        pfrm.set_scroll(Layer::map_1_ext, x_scroll_ - 240, -offset + 8);
        pfrm.set_scroll(Layer::map_0_ext, x_scroll_, -offset + 8);
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


    if (key_down<Key::start>(pfrm)) {
        return scene_pool::alloc<MultiplayerConnectScene>();
    }


    switch (state_) {
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
            pfrm.speaker().stop_music();
        }

        // if (pfrm.keyboard().down_transition<Key::start>()) {
        //     pfrm.speaker().stop_music();
        //     return scene_pool::alloc<MultiplayerConnectScene>();
        // }

        if (key_down<Key::right>(pfrm) or key_down<Key::down>(pfrm)) {
            if (menu_selection_ == 0) {
                menu_selection_ = 1;
                put_menu_text(pfrm);
                // pfrm.speaker().play_sound("scroll", 1);
                state_ = State::scroll_right;
                timer_ = 0;
            }
        }
        if (key_down<Key::left>(pfrm) or key_down<Key::up>(pfrm)) {
            if (menu_selection_ == 1) {
                menu_selection_ = 0;
                put_menu_text(pfrm);
                // pfrm.speaker().play_sound("scroll", 1);
                state_ = State::scroll_left;
                timer_ = 0;
            }
        }
        break;

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
                app.challenge_mode() = false;
                return scene_pool::alloc<NewgameScene>();

            case 1: {
                app.challenge_mode() = true;
                return scene_pool::alloc<SelectChallengeScene>();
            }
            }
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::wait_2:
        break;
    }

    return null_scene();
}



void TitleScreenScene::display(Platform& pfrm, App& app)
{
    if (x_scroll_ > 160) {
        Sprite sprite;
        sprite.set_position({Float(135 - x_scroll_ / 3) + island_offset_,
                             Float(110 - 0.25f * (240 - x_scroll_))});
        sprite.set_priority(3);

        pfrm.screen().draw(sprite);
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
}



static void init_clouds2(Platform& pfrm)
{
    // Some of the worst code I've ever written. Again, this is for a game
    // jam. I keep adding misc. comments apologizing for the quality of the code
    // throughout the project. I am embarassed by my work here, but what to do?
    // Deadlines looming...

    pfrm.enable_feature("parallax-clouds", true);

    for (int i = 0; i < 32; ++i) {
        for (int j = 0; j < 32; ++j) {
            pfrm.set_tile(Layer::background, i, j, 248);
        }
    }

    for (int i = 0; i < 32; ++i) {
        pfrm.set_tile(Layer::background, i, 18, 222);
        pfrm.set_tile(Layer::background, i, 19, 222);
    }

    auto put_cloud_block = [&](int x, int y, int offset) {
        int start_offset = offset;
        pfrm.set_tile(Layer::background, x, y, offset++);
        pfrm.set_tile(Layer::background, x + 1, y, offset++);
        if (start_offset < 205 and offset > 204) {
            offset += 12;
        }
        pfrm.set_tile(Layer::background, x, y + 1, offset++);
        pfrm.set_tile(Layer::background, x + 1, y + 1, offset);
    };

    auto put_cloud_block2 = [&](int x, int y, int offset) {
        int start_offset = offset;
        if (start_offset <= 235 and offset > 234) {
            offset += 12;
        }
        pfrm.set_tile(Layer::background, x, y, offset++);
        pfrm.set_tile(Layer::background, x + 1, y, offset++);
        if (offset == 249) {
            offset = 277;
        }
        pfrm.set_tile(Layer::background, x, y + 1, offset++);
        pfrm.set_tile(Layer::background, x + 1, y + 1, offset);
    };

    auto put_fg_cloud_type_n = [&](int x, int type) {
        const int start = 187 + type * 4;
        int offset = start;
        if (offset > 204) {
            offset += 12;
        }
        put_cloud_block(x * 2, 16, offset);
    };

    auto put_bg_cloud_type_n = [&](int x, int type) {
        put_cloud_block2(x * 2, 14, 223 + type * 4);
    };

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 6;
        put_fg_cloud_type_n(offset + 0, 0);
        put_fg_cloud_type_n(offset + 1, 1);
        put_fg_cloud_type_n(offset + 2, 2);
        put_fg_cloud_type_n(offset + 3, 3);
        put_fg_cloud_type_n(offset + 4, 4);
        put_fg_cloud_type_n(offset + 5, 5);
    }

    for (int i = 0; i < 4; ++i) {
        const int offset = i * 4;
        put_bg_cloud_type_n(offset + 0, 0);
        put_bg_cloud_type_n(offset + 1, 1);
        put_bg_cloud_type_n(offset + 2, 2);
        put_bg_cloud_type_n(offset + 3, 3);
    }
}



} // namespace skyland
