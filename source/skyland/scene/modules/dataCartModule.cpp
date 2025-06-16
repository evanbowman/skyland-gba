#include "dataCartModule.hpp"
#include "checkersModule.hpp"
#include "fileBrowserModule.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene/textviewScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void DataCartModule::show_cart(int index)
{
    if (not carts_) {
        return;
    }

    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }

    // left arrow icon
    PLATFORM.set_tile(Layer::overlay, 1, 8, index == 0 ? 107 : 105);

    // right arrow icon
    PLATFORM.set_tile(Layer::overlay,
                      calc_screen_tiles().x - 2,
                      8,
                      index == carts_->max_carts() - 1 ? 106 : 104);


    static const Text::OptColors colors{
        {ColorConstant::silver_white, custom_color(0x000818)}};

    PLATFORM_EXTENSION(force_vsync);
    Text tmp({});
    tmp.append("(", colors);
    tmp.append(index + 1, colors);
    tmp.append("/", colors);
    tmp.append(carts_->max_carts(), colors);
    tmp.append(")", colors);

    draw_image(332, 5, 3, 20, 2, Layer::overlay);
    for (int i = 5; i < 13; ++i) {
        PLATFORM.set_tile(Layer::overlay, 6, i, 384);
        PLATFORM.set_tile(Layer::overlay, 23, i, 372);
    }
    for (int i = 7; i < 23; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 13, 375);
    }
    PLATFORM.set_tile(Layer::overlay, 6, 13, 373);
    PLATFORM.set_tile(Layer::overlay, 23, 13, 374);

    auto cart = carts_->load(index);
    if (not cart) {
        DataCart missing(index);
        tmp.append(" location: ", colors);
        tmp.append(missing.get_label_string("location").c_str(), colors);

        draw_image(376, 14, 8, 2, 4, Layer::overlay);

        Text::print(format("cart_%", index + 1).c_str(), {12, 6}, colors);

    } else {
        tmp.append(" found at ", colors);
        auto exact = cart->get_label_string("exact_location");
        for (char& c : exact) {
            // FIXME: ini conf library ignoring whitespace in strings. Fix conf.cpp.
            if (c == '_') {
                c = ' ';
            }
        }
        tmp.append(exact.c_str(), colors);

        auto name = cart->name();

        Text::print(name.c_str(), {8, 6}, colors);
        Text::print(cart->subheading().c_str(), {8, 8}, colors);

        auto type = cart->expect_content_string("icon");
        if (*type == "image") {
            PLATFORM.set_tile(Layer::overlay, 8, 10, 386);
        } else if (*type == "files") {
            PLATFORM.set_tile(Layer::overlay, 8, 10, 387);
        } else if (*type == "exe") {
            PLATFORM.set_tile(Layer::overlay, 8, 10, 385);
        }
    }

    tmp.__detach();
}



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void DataCartModule::enter(Scene& prev)
{
    PLATFORM.screen().set_view(View{});

    PLATFORM.load_overlay_texture("overlay_datacart");
    // PLATFORM.load_background_texture("background");
    // PLATFORM.screen().schedule_fade(0.65f);
    // PLATFORM.set_overlay_origin(0, -4);


    PLATFORM.load_tile1_texture("datacarts_flattened");
    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 21; ++y) {
            PLATFORM.set_raw_tile(Layer::map_1, x, y, 1);
        }
    }

    __draw_image(1, 0, 3, 30, 14, Layer::map_1);
}



void DataCartModule::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);
    PLATFORM.speaker().stop_sound("archivist");
}



ScenePtr DataCartModule::update(Time delta)
{
    auto prompt_flag = GlobalPersistentData::datacarts_prompt;

    if (not APP.gp_.stateflags_.get(prompt_flag)) {
        APP.gp_.stateflags_.set(prompt_flag, true);
        save::store_global_data(APP.gp_);
        auto next = []() -> ScenePtr {
            auto ret = make_scene<DataCartModule>(true);
            ret->skip_dialog_ = true;
            return ret;
        };
        return dialog_prompt(
            SystemString::dialog_datacarts_prompt, next, "archivist");
    } else if (not skip_dialog_) {
        auto next = []() -> ScenePtr {
            auto ret = make_scene<DataCartModule>(true);
            ret->skip_dialog_ = true;
            return ret;
        };
        return dialog_prompt(
            SystemString::dialog_datacarts_return, next, "archivist");
    }

    APP.player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (not PLATFORM.speaker().is_sound_playing("archivist")) {
        PLATFORM.speaker().play_sound("archivist", 9);
    }


    APP.update_parallax(delta);

    switch (state_) {
    case State::init: {
        // carts_.emplace();
        // show_cart(cart_index_);
        // state_ = State::select;
        state_ = State::fade_in;
        if (cart_index_ not_eq 0) {
            state_ = State::select;
            PLATFORM.screen().schedule_fade(0.65f);
            PLATFORM.set_overlay_origin(0, -4);
            carts_.emplace();
            show_cart(cart_index_);
        }
        break;
    }

    case State::fade_in:
        timer_ += delta;
        {
            constexpr auto fade_duration = milliseconds(500);
            auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);

            if (skip_intro_) {
                amount = 1.f - ((1.f - 0.65f) *
                                smoothstep(0.f, fade_duration, timer_));
            }
            PLATFORM.screen().schedule_fade(amount);

            if (APP.player().key_down(Key::action_1) or
                APP.player().key_down(Key::action_2) or
                timer_ > fade_duration) {
                timer_ = 0;
                state_ = State::wait_0;
                PLATFORM.screen().schedule_fade(0);
                if (skip_intro_) {
                    state_ = State::select;
                    PLATFORM.screen().schedule_fade(0.65f);
                    PLATFORM.set_overlay_origin(0, -4);
                    carts_.emplace();
                    show_cart(cart_index_);
                }
            }
        }
        break;

    case State::wait_0:
        timer_ += delta;
        if (APP.player().key_down(Key::action_1) or
            APP.player().key_down(Key::action_2) or
            timer_ > milliseconds(1000)) {
            timer_ = 0;
            state_ = State::fade_partial;
        }
        break;

    case State::fade_partial: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1100);
        const auto amount = 0.65f * smoothstep(0.f, fade_duration, timer_);

        PLATFORM.screen().schedule_fade(amount);

        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::select;
            PLATFORM.screen().schedule_fade(0.65f);
            PLATFORM.set_overlay_origin(0, -4);
            carts_.emplace();
            show_cart(cart_index_);
        }
        break;
    }

    case State::select:
        if (test_key(Key::right)) {
            if (cart_index_ < carts_->max_carts() - 1) {
                ++cart_index_;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                show_cart(cart_index_);
            }
        }
        if (test_key(Key::left)) {
            if (cart_index_ > 0) {
                --cart_index_;
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                show_cart(cart_index_);
            }
        }
        if (APP.player().key_down(Key::action_2)) {
            PLATFORM.fill_overlay(0);
            PLATFORM.screen().schedule_fade(
                1.f, ColorConstant::rich_black, {}, true, true);
            state_ = State::exit;
        } else if (APP.player().key_down(Key::action_1)) {
            if (auto cart = carts_->load(cart_index_)) {
                state_ = State::anim_out;
                timer_ = 0;
                for (int x = 0; x < 30; ++x) {
                    PLATFORM.set_tile(Layer::overlay, x, 0, 0);
                    PLATFORM.set_tile(Layer::overlay, x, 20, 0);
                }
                for (int y = 0; y < 20; ++y) {
                    PLATFORM.set_tile(Layer::overlay, 1, y, 0);
                    PLATFORM.set_tile(Layer::overlay, 28, y, 0);
                }
                draw_image(112, 5, 3, 20, 11, Layer::overlay);

                auto name = cart->name();

                const auto colors = Text::OptColors{
                    {ColorConstant::rich_black, custom_color(0xd9dee6)}};

                Text::print(name.c_str(), {8, 6}, colors);
                Text::print(cart->subheading().c_str(), {8, 8}, colors);

                auto type = cart->expect_content_string("icon");
                if (*type == "image") {
                    PLATFORM.set_tile(Layer::overlay, 8, 10, 389);
                } else if (*type == "files") {
                    PLATFORM.set_tile(Layer::overlay, 8, 10, 390);
                } else if (*type == "exe") {
                    PLATFORM.set_tile(Layer::overlay, 8, 10, 388);
                }

                PLATFORM.speaker().play_sound("button_wooden", 3);

                for (int y = 21; y < 30; ++y) {
                    for (int x = 0; x < 30; ++x) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 0);
                    }
                }

            } else {
                PLATFORM.speaker().play_sound("beep_error", 1);
            }
        }
        break;

    case State::exit:
        return make_scene<TitleScreenScene>(3);

    case State::anim_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(800);
        cart_scroll_down_ += 0.000015f * delta;
        PLATFORM.set_overlay_origin(0, -4 + -1 * cart_scroll_down_);
        if (timer_ > fade_duration) {
            state_ = State::drop;
            timer_ = 0;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(0.65f + (1 - 0.65f) * amount);
        }
        break;
    }

    case State::drop:
        cart_scroll_down_ += 0.00035f * delta;
        if (cart_scroll_down_ > 138) {
            PLATFORM.fill_overlay(0);
            state_ = State::done;
        }
        PLATFORM.set_overlay_origin(0, -1 * cart_scroll_down_);
        break;

    case State::done:
        PLATFORM.speaker().play_sound("insert_cart", 2);
        PLATFORM.set_overlay_origin(0, 0);
        state_ = State::wait;
        wait_time_ = milliseconds(200);
        timer_ = 0;
        break;

    case State::wait:
        timer_ += delta;
        if (timer_ > wait_time_) {
            timer_ = 0;
            if (auto cart = carts_->load(cart_index_)) {
                auto str = format("booting %...", cart->name().c_str());
                auto tp = cart->expect_content_string("type");
                if (*tp == "files") {
                    str = "reading archive...";
                } else if (*tp == "image") {
                    str = "developing photos...";
                } else if (*tp == "textview") {
                    str = "printing booklet...";
                }
                auto margin = centered_text_margins(utf8::len(str.c_str()));
                Text::print(str.c_str(), {(u8)margin, 9});
            }
            state_ = State::booting;
        }
        break;

    case State::booting:
        timer_ += delta;
        if (test_key(Key::action_2) or timer_ > milliseconds(700)) {
            PLATFORM.fill_overlay(0);
            state_ = State::boot;
        }
        break;

    case State::boot:
        PLATFORM.load_overlay_texture("overlay");
        return boot_cart(cart_index_);
    }

    return null_scene();
}



class CartPhotoViewScene : public Scene
{
public:
    CartPhotoViewScene(int cart_id) : cart_id_(cart_id)
    {
    }


    void enter(Scene&) override
    {
        PLATFORM.enable_glyph_mode(false);
        PLATFORM.fill_overlay(0);
        PLATFORM.load_overlay_texture(
            DataCart(cart_id_).expect_content_string("img")->c_str());
        draw_image(1, 4, 1, 22, 17, Layer::overlay);
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1);

        PLATFORM.speaker().set_music_volume(15);
        PLATFORM.speaker().set_sounds_volume(13);
    }


    void exit(Scene&) override
    {
        PLATFORM.enable_glyph_mode(true);
        PLATFORM.speaker().set_music_volume(
            Platform::Speaker::music_volume_max);
        PLATFORM.speaker().set_sounds_volume(
            Platform::Speaker::music_volume_max);
    }


    ScenePtr update(Time) override
    {
        if (APP.player().key_down(Key::action_2)) {
            PLATFORM.fill_overlay(0);
            auto next = make_scene<DataCartModule>();
            next->skip_dialog_ = true;
            next->set_index(cart_id_);
            return next;
        }

        if (APP.player().key_down(Key::action_1)) {
            DataCart cart(cart_id_);
            if (auto c = cart.get_content_string("inscription")) {
                for (int x = 4; x < 26; ++x) {
                    for (int y = 1; y < 18; ++y) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 309);
                    }
                }
                PLATFORM.screen().clear();
                PLATFORM.screen().display();

                if (not flipped_) {
                    PLATFORM.load_overlay_texture("overlay_textview");
                    PLATFORM.enable_glyph_mode(true);
                    text_.emplace();
                    text_->assign((*c)->c_str(), {5, 2}, {20, 16});
                    PLATFORM.speaker().play_sound("page_flip", 3);

                } else {
                    PLATFORM.enable_glyph_mode(false);
                    PLATFORM.fill_overlay(0);
                    PLATFORM.load_overlay_texture(
                        DataCart(cart_id_)
                            .expect_content_string("img")
                            ->c_str());
                    draw_image(1, 4, 1, 22, 17, Layer::overlay);
                    PLATFORM.speaker().play_sound("page_flip", 3);
                }
                flipped_ = not flipped_;
            }
        }

        if (not PLATFORM.speaker().is_sound_playing("archivist")) {
            PLATFORM.speaker().play_sound("archivist", 9);
        }

        return null_scene();
    }

private:
    Optional<TextView> text_;
    int cart_id_;
    bool flipped_ = false;
};



ScenePtr DataCartModule::boot_cart(int cart_index)
{
    DataCart cart(cart_index);

    auto type = cart.expect_content_string("type");

    if (*type == "reboot") {
        PLATFORM_EXTENSION(restart);
    } else if (*type == "checkers") {
        return make_scene<CheckersModule>();
    } else if (*type == "image") {
        PLATFORM.speaker().play_sound("tw_bell", 2);
        return make_scene<CartPhotoViewScene>(cart_index);
    } else if (*type == "files") {
        PLATFORM.speaker().play_sound("tw_bell", 2);
        UserContext ctx;
        ctx.hide_path_ = 3;
        ctx.allow_backtrack_ = false;
        ctx.readonly_ = true;
        ctx.browser_exit_scene_ = [cart_index] {
            auto next = make_scene<DataCartModule>(true);
            next->skip_dialog_ = true;
            next->set_index(cart_index);
            PLATFORM.screen().schedule_fade(1);
            return next;
        };
        auto path = cart.expect_content_string("dir");
        auto next =
            make_scene<FileBrowserModule>(std::move(ctx), path->c_str(), true);
        return next;

    } else if (*type == "textview") {
        PLATFORM.speaker().play_sound("tw_bell", 2);
        auto tv = make_scene<TextviewScene>(
            cart.expect_content_string("text")->c_str());
        tv->next_ = [cart_index]() {
            auto ret = make_scene<DataCartModule>();
            ret->skip_dialog_ = true;
            ret->set_index(cart_index);
            return ret;
        };
        return tv;
    }

    return make_scene<TitleScreenScene>(3);
}



DataCartModule::Factory DataCartModule::factory_;



} // namespace skyland
