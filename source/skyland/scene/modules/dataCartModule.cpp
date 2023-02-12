#include "dataCartModule.hpp"
#include "skyland/skyland.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "checkersModule.hpp"



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

    static const Text::OptColors colors{{ColorConstant::silver_white,
                                         custom_color(0x294a6b)}};

    pfrm.system_call("vsync", nullptr);
    Text tmp(pfrm, {});
    tmp.append("(", colors);
    tmp.append(index + 1, colors);
    tmp.append("/", colors);
    tmp.append(carts_->max_carts(), colors);
    tmp.append(")", colors);

    auto cart = carts_->load(index);
    if (not cart) {
        DataCart missing(index);
        tmp.append(" location: ", colors);
        tmp.append(missing.get_label_string(pfrm, "location").c_str(),
                   colors);
    } else {
        draw_image(pfrm, 112, 5, 4, 20, 11, Layer::overlay);

        tmp.append(" found at: ", colors);
        auto exact = cart->get_label_string(pfrm, "exact_location");
        for (char& c : exact) {
            // FIXME: ini conf library ignoring whitespace in strings. Fix conf.cpp.
            if (c == '_') {
                c = ' ';
            }
        }
        tmp.append(exact.c_str(),
                   colors);

        auto name = cart->name(pfrm);

        const auto colors =
            Text::OptColors{{
                             ColorConstant::rich_black,
                             custom_color(0xd9dee6)
            }};

        Text::print(pfrm, name.c_str(), {8, 7}, colors);
        Text::print(pfrm, cart->subheading(pfrm).c_str(), {8, 9}, colors);
    }

    tmp.__detach();
}



void DataCartModule::enter(Platform& pfrm, App&, Scene& prev)
{
    Text::platform_retain_alphabet(pfrm);

    pfrm.load_overlay_texture("overlay_datacart");
    pfrm.screen().schedule_fade(0.55f);
    pfrm.set_overlay_origin(0, -4);
}



void DataCartModule::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.fill_overlay(0);
    pfrm.screen().schedule_fade(1.f, ColorConstant::rich_black, {}, true, true);
}



ScenePtr<Scene> DataCartModule::update(Platform& pfrm,
                                       App& app,
                                       Microseconds delta)
{
    app.player().update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return app.player().test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };


    app.update_parallax(delta);

    switch (state_) {
    case State::init: {
        carts_.emplace(pfrm);
        show_cart(pfrm, 0);
        state_ = State::select;
        break;
    }

    case State::select:
        if (test_key(Key::right)) {
            if (cart_index_ < carts_->max_carts() - 1) {
                ++cart_index_;
                show_cart(pfrm, cart_index_);
            }

        }
        if (test_key(Key::left)) {
            if (cart_index_ > 0) {
                --cart_index_;
                show_cart(pfrm, cart_index_);
            }
        }
        if (app.player().key_down(pfrm, Key::action_2)) {
            pfrm.fill_overlay(0);
            pfrm.screen().schedule_fade(1.f, ColorConstant::rich_black, {}, true, true);
            state_ = State::exit;
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            if (carts_->load(cart_index_)) {
                state_ = State::anim_out;
                for (int x = 0; x < 30; ++x) {
                    pfrm.set_tile(Layer::overlay, x, 0, 0);
                    pfrm.set_tile(Layer::overlay, x, 20, 0);
                }
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
            pfrm.screen().schedule_fade(0.55f + (1 - 0.55f) * amount);
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
        timer_ = 0;
        break;

    case State::wait:
        timer_ += delta;
        if (timer_ > milliseconds(900)) {
            if (auto cart = carts_->load(cart_index_)) {
                auto str = format("booting %...", cart->name(pfrm).c_str());
                auto margin = centered_text_margins(pfrm,
                                                    utf8::len(str.c_str()));
                Text::print(pfrm, str.c_str(), {(u8)margin, 9});
            }
            timer_ = 0;
            state_ = State::booting;
        }
        break;

    case State::booting:
        timer_ += delta;
        if (timer_ > milliseconds(1500)) {
            return boot_cart(pfrm, cart_index_);
        }
        break;
    }

    return null_scene();
}



ScenePtr<Scene> DataCartModule::boot_cart(Platform& pfrm, int cart_index)
{
    DataCart cart(cart_index);

    auto type = cart.get_content_string(pfrm, "type");

    if (type == "reboot") {
        pfrm.system_call("restart", nullptr);
    } else if (type == "checkers") {
        return scene_pool::alloc<CheckersModule>();
    }

    return scene_pool::alloc<TitleScreenScene>(3);
}



DataCartModule::Factory DataCartModule::factory_;



}
