#include "dataCartModule.hpp"
#include "checkersModule.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void DataCartModule::show_cart(Platform& pfrm, int index)
{
    if (not carts_) {
        return;
    }

    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            pfrm.set_tile(Layer::overlay, x, y, 0);
        }
    }

    // left arrow icon
    pfrm.set_tile(Layer::overlay, 1, 8, index == 0 ? 107 : 105);

    // right arrow icon
    pfrm.set_tile(Layer::overlay,
                  calc_screen_tiles(pfrm).x - 2,
                  8,
                  index == carts_->max_carts() - 1 ? 106 : 104);


    static const Text::OptColors colors{
        {ColorConstant::silver_white, custom_color(0x18395a)}};

    pfrm.system_call("vsync", nullptr);
    Text tmp(pfrm, {});
    tmp.append("(", colors);
    tmp.append(index + 1, colors);
    tmp.append("/", colors);
    tmp.append(carts_->max_carts(), colors);
    tmp.append(")", colors);

    draw_image(pfrm, 332, 5, 3, 20, 2, Layer::overlay);
    for (int i = 5; i < 13; ++i) {
        pfrm.set_tile(Layer::overlay, 6, i, 384);
        pfrm.set_tile(Layer::overlay, 23, i, 372);
    }
    for (int i = 7; i < 23; ++i) {
        pfrm.set_tile(Layer::overlay, i, 13, 375);
    }
    pfrm.set_tile(Layer::overlay, 6, 13, 373);
    pfrm.set_tile(Layer::overlay, 23, 13, 374);

    auto cart = carts_->load(index);
    if (not cart) {
        DataCart missing(index);
        tmp.append(" location: ", colors);
        tmp.append(missing.get_label_string(pfrm, "location").c_str(), colors);

        draw_image(pfrm, 376, 14, 8, 2, 4, Layer::overlay);

        Text::print(pfrm, format("cart_%", index + 1).c_str(), {12, 6}, colors);

    } else {
        tmp.append(" found at ", colors);
        auto exact = cart->get_label_string(pfrm, "exact_location");
        for (char& c : exact) {
            // FIXME: ini conf library ignoring whitespace in strings. Fix conf.cpp.
            if (c == '_') {
                c = ' ';
            }
        }
        tmp.append(exact.c_str(), colors);

        auto name = cart->name(pfrm);

        Text::print(pfrm, name.c_str(), {8, 6}, colors);
        Text::print(pfrm, cart->subheading(pfrm).c_str(), {8, 8}, colors);
    }

    tmp.__detach();
}



void DataCartModule::enter(Platform& pfrm, App&, Scene& prev)
{
    Text::platform_retain_alphabet(pfrm);

    pfrm.load_overlay_texture("overlay_datacart");
    pfrm.load_background_texture("background");
    pfrm.screen().schedule_fade(0.65f);
    pfrm.set_overlay_origin(0, -4);
}



void DataCartModule::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
}



ScenePtr<Scene>
DataCartModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto prompt_flag = GlobalPersistentData::datacarts_prompt;

    if (not app.gp_.stateflags_.get(prompt_flag)) {
        app.gp_.stateflags_.set(prompt_flag, true);
        save::store_global_data(pfrm, app.gp_);
        auto next = scene_pool::make_deferred_scene<DataCartModule>();
        return dialog_prompt(pfrm, SystemString::dialog_datacarts_prompt, next);
    }

    app.player().update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };


    app.update_parallax(delta);

    switch (state_) {
    case State::init: {
        carts_.emplace(pfrm);
        show_cart(pfrm, cart_index_);
        state_ = State::select;
        break;
    }

    case State::select:
        if (test_key(Key::right)) {
            if (cart_index_ < carts_->max_carts() - 1) {
                ++cart_index_;
                pfrm.speaker().play_sound("cursor_tick", 0);
                show_cart(pfrm, cart_index_);
            }
        }
        if (test_key(Key::left)) {
            if (cart_index_ > 0) {
                --cart_index_;
                pfrm.speaker().play_sound("cursor_tick", 0);
                show_cart(pfrm, cart_index_);
            }
        }
        if (app.player().key_down(pfrm, Key::action_2)) {
            pfrm.fill_overlay(0);
            pfrm.screen().schedule_fade(
                1.f, ColorConstant::rich_black, {}, true, true);
            state_ = State::exit;
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            if (auto cart = carts_->load(cart_index_)) {
                state_ = State::anim_out;
                for (int x = 0; x < 30; ++x) {
                    pfrm.set_tile(Layer::overlay, x, 0, 0);
                    pfrm.set_tile(Layer::overlay, x, 20, 0);
                }
                for (int y = 0; y < 20; ++y) {
                    pfrm.set_tile(Layer::overlay, 1, y, 0);
                    pfrm.set_tile(Layer::overlay, 28, y, 0);
                }
                draw_image(pfrm, 112, 5, 3, 20, 11, Layer::overlay);

                auto name = cart->name(pfrm);

                const auto colors = Text::OptColors{
                    {ColorConstant::rich_black, custom_color(0xd9dee6)}};

                Text::print(pfrm, name.c_str(), {8, 6}, colors);
                Text::print(
                    pfrm, cart->subheading(pfrm).c_str(), {8, 8}, colors);

                pfrm.speaker().play_sound("button_wooden", 3);

            } else {
                pfrm.speaker().play_sound("beep_error", 1);
            }
        }
        break;

    case State::exit:
        return scene_pool::alloc<TitleScreenScene>(3);

    case State::anim_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(800);
        cart_scroll_down_ += 0.000015f * delta;
        pfrm.set_overlay_origin(0, -4 + -1 * cart_scroll_down_);
        if (timer_ > fade_duration) {
            state_ = State::drop;
            timer_ = 0;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(0.65f + (1 - 0.65f) * amount);
        }
        break;
    }

    case State::drop:
        cart_scroll_down_ += 0.00035f * delta;
        if (cart_scroll_down_ > 138) {
            pfrm.fill_overlay(0);
            state_ = State::done;
        }
        pfrm.set_overlay_origin(0, -1 * cart_scroll_down_);
        break;

    case State::done:
        pfrm.speaker().play_sound("insert_cart", 2);
        pfrm.set_overlay_origin(0, 0);
        state_ = State::wait;
        wait_time_ = milliseconds(900);
        timer_ = 0;
        break;

    case State::wait:
        timer_ += delta;
        if (timer_ > wait_time_) {
            if (auto cart = carts_->load(cart_index_)) {
                auto str = format("booting %...", cart->name(pfrm).c_str());
                if (cart->get_content_string(pfrm, "type") == "image") {
                    str = "developing photos...";
                }
                auto margin =
                    centered_text_margins(pfrm, utf8::len(str.c_str()));
                Text::print(pfrm, str.c_str(), {(u8)margin, 9});
            }
            timer_ = 0;
            state_ = State::booting;
        }
        break;

    case State::booting:
        timer_ += delta;
        if (timer_ > milliseconds(1500)) {
            pfrm.fill_overlay(0);
            state_ = State::boot;
        }
        break;

    case State::boot:
        pfrm.load_overlay_texture("overlay");
        return boot_cart(pfrm, cart_index_);
    }

    return null_scene();
}



class CartPhotoViewScene : public Scene
{
public:
    CartPhotoViewScene(int cart_id) : cart_id_(cart_id)
    {
    }


    void enter(Platform& pfrm, App&, Scene&) override
    {
        pfrm.enable_glyph_mode(false);
        pfrm.fill_overlay(0);
        pfrm.load_overlay_texture(
            DataCart(cart_id_).get_content_string(pfrm, "img").c_str());
        draw_image(pfrm, 1, 4, 1, 22, 17, Layer::overlay);
        pfrm.screen().schedule_fade(0);
        pfrm.screen().schedule_fade(1);
    }


    void exit(Platform& pfrm, App&, Scene&) override
    {
        pfrm.enable_glyph_mode(true);
    }


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds) override
    {
        if (app.player().key_down(pfrm, Key::action_2)) {
            pfrm.fill_overlay(0);
            auto next = scene_pool::alloc<DataCartModule>();
            next->set_index(cart_id_);
            return next;
        }

        return null_scene();
    }

private:
    int cart_id_;
};



ScenePtr<Scene> DataCartModule::boot_cart(Platform& pfrm, int cart_index)
{
    DataCart cart(cart_index);

    auto type = cart.get_content_string(pfrm, "type");

    if (type == "reboot") {
        pfrm.system_call("restart", nullptr);
    } else if (type == "checkers") {
        return scene_pool::alloc<CheckersModule>();
    } else if (type == "image") {
        pfrm.speaker().play_sound("tw_bell", 2);
        return scene_pool::alloc<CartPhotoViewScene>(cart_index);
    }

    return scene_pool::alloc<TitleScreenScene>(3);
}



DataCartModule::Factory DataCartModule::factory_;



} // namespace skyland
