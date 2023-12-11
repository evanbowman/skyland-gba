////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "selectTutorialScene.hpp"
#include "fadeInScene.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



static const Float default_fade = 0.6f;



int SelectTutorialScene::tutorial_count()
{
    if (auto script = PLATFORM.load_file_contents("scripts",
                                                  "tutorials/tutorials.lisp")) {
        lisp::BasicCharSequence seq(script);
        auto result = lisp::dostring(seq, [](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            PLATFORM.fatal(p.data_.c_str());
        });
        return lisp::length(result);
    }
    return 0;
}



void SelectTutorialScene::quick_select(int tutorial_number)
{
    while (tutorial_number > 5) {
        tutorial_number -= 5;
        ++page_;
    }

    cursor_ = tutorial_number;

    state_ = State::quickselect;
}



void SelectTutorialScene::enter(Scene& prev)
{
    PLATFORM.screen().set_shader(passthrough_shader);

    PLATFORM.speaker().play_music("unaccompanied_wind", 0);

    APP.set_coins(0);

    APP.player_island().projectiles().clear();

    PLATFORM.fill_overlay(0);

    APP.stat_timer().reset(0);

    APP.effects().clear();

    for (u8 x = 0; x < 16; ++x) {
        for (u8 y = 0; y < 16; ++y) {
            APP.player_island().fire_extinguish({x, y});
        }
    }

    // In case we came from a previous tutorial, give control back to the
    // player.
    APP.swap_player<PlayerP1>();

    if (state_ not_eq State::quickselect) {
        PLATFORM.load_overlay_texture("overlay_challenges");

        PLATFORM.system_call("v-parallax", (void*)false);
    }

    APP.game_mode() = App::GameMode::tutorial;

    if (auto script = PLATFORM.load_file_contents("scripts",
                                                  "tutorials/tutorials.lisp")) {
        lisp::BasicCharSequence seq(script);
        auto result = lisp::dostring(seq, [](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            PLATFORM.fatal(p.data_.c_str());
        });
        tutorials_ = result;
        const auto tutorial_count = lisp::length(result);
        page_count_ = tutorial_count / 5 + (tutorial_count % 5 ? 1 : 0);

    } else {
        PLATFORM.fatal("missing file tutorial.lisp");
    }


    if (state_ not_eq State::quickselect) {
        show_options();

        for (int i = 0; i < 16; ++i) {
            for (int j = 0; j < 16; ++j) {
                PLATFORM.set_tile(Layer::map_0_ext, i, j, 0);
                PLATFORM.set_tile(Layer::map_1_ext, i, j, 0);
            }
        }

        PLATFORM.screen().set_view({});

        PLATFORM.screen().fade(
            default_fade, ColorConstant::rich_black, {}, false);
    }

    PLATFORM.delta_clock().reset();
}



void SelectTutorialScene::show_options()
{
    PLATFORM.screen().clear();
    text_.clear();
    PLATFORM.screen().display();

    PLATFORM.fill_overlay(0);

    PLATFORM.set_tile(Layer::overlay, 1, 2, 90);
    PLATFORM.set_tile(Layer::overlay, 28, 2, 92);
    PLATFORM.set_tile(Layer::overlay, 1, 15, 94);
    PLATFORM.set_tile(Layer::overlay, 28, 15, 96);
    for (int x = 2; x < 28; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 2, 91);
        PLATFORM.set_tile(Layer::overlay, x, 15, 95);
    }
    for (int y = 3; y < 15; ++y) {
        PLATFORM.set_tile(Layer::overlay, 1, y, 93);
        PLATFORM.set_tile(Layer::overlay, 28, y, 97);
    }

    if (not tutorials_) {
        return;
    }

    int index = 0;
    int start_index = page_ * 5;

    lisp::foreach (*tutorials_, [&](lisp::Value* val) {
        if (val->type() not_eq lisp::Value::Type::cons) {
            PLATFORM.fatal("tutorial list format invalid");
        }

        auto name = val->cons().car();
        if (name->type() not_eq lisp::Value::Type::string) {
            PLATFORM.fatal("tutorial list format invalid");
        }

        bool completed = APP.gp_.watched_tutorials_.get() & (1 << index);

        if (index++ < start_index) {
            return;
        }

        text_.emplace_back(name->string().value(),
                           OverlayCoord{4, u8(4 + text_.size() * 2)});

        if (completed) {
            PLATFORM.set_tile(Layer::overlay,
                              text_.back().coord().x + text_.back().len() + 1,
                              text_.back().coord().y,
                              84);
        }
    });


    if (page_count_ > 1) {
        int margin = (calc_screen_tiles().x - page_count_ * 2) / 2;
        for (int i = 0; i < page_count_; ++i) {
            if (i == page_) {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 83);
            } else {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 82);
            }
        }
    }
}



void prep_level();



void SelectTutorialScene::exit(Scene& next)
{
    text_.clear();
    PLATFORM.fill_overlay(0);
    PLATFORM.load_overlay_texture("overlay");

    PLATFORM.system_call("v-parallax", (void*)true);
}



void SelectTutorialScene::display()
{
    if (state_ not_eq State::idle) {
        return;
    }
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(59);

    Vec2<Fixnum> origin;

    auto ambient_movement = 2 * float(sine(4 * 3.14f * 0.004f * timer_ + 180)) /
                            std::numeric_limits<s16>::max();

    origin.x += 16.0_fixed + Fixnum(ambient_movement);
    origin.y += 32.0_fixed + Fixnum::from_integer(cursor_ * 16 - 1);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);
}



ScenePtr<Scene> SelectTutorialScene::update(Time delta)
{
    if (exit_) {
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    timer_ += delta;

    switch (state_) {
    case State::fade_in:
        break;

    case State::quickselect: {
        auto index = page_ * 5 + cursor_;
        auto choice = lisp::get_list(*tutorials_, index);

        if (not(APP.gp_.watched_tutorials_.get() & (1 << index))) {
            APP.gp_.watched_tutorials_.set(APP.gp_.watched_tutorials_.get() |
                                           (1 << index));
            save::store_global_data(APP.gp_);
        }

        auto file_name = choice->cons().cdr();
        if (file_name->type() not_eq lisp::Value::Type::string) {
            PLATFORM.fatal("tutorial list format invalid");
        }

        if (auto script = PLATFORM.load_file_contents(
                "scripts", file_name->string().value())) {

            lisp::BasicCharSequence seq(script);
            lisp::dostring(seq, [](lisp::Value& err) {
                lisp::DefaultPrinter p;
                lisp::format(&err, p);
                PLATFORM.fatal(p.data_.c_str());
            });
            prep_level();
            APP.player_island().repaint();
            APP.player_island().render_exterior();

            rng::critical_state = 42;

            PLATFORM.speaker().play_music(APP.environment().music(), 0);

            APP.time_stream().enable_pushes(true);
            APP.time_stream().clear();

            time_stream::event::Initial e;
            APP.time_stream().push(APP.level_timer(), e);

            return scene_pool::alloc<FadeInScene>();
        } else {
            StringBuffer<32> err("file ");
            err += file_name->string().value();
            err += " missing";
            PLATFORM.fatal(err.c_str());
        }
        break;
    }

    case State::idle: {
        if (not tutorials_) {
            return null_scene();
        }

        if (APP.player().key_down(Key::down)) {
            if ((u32)cursor_ < text_.size() - 1) {
                cursor_++;
            }
        }

        if (APP.player().key_down(Key::up)) {
            if (cursor_) {
                cursor_--;
            }
        }

        if (APP.player().key_down(Key::right)) {
            if (page_ < page_count_ - 1) {
                ++page_;
                show_options();
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (APP.player().key_down(Key::left)) {
            if (page_ > 0) {
                --page_;
                show_options();
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::fade_out;
            timer_ = 0;
            text_.clear();
            PLATFORM.fill_overlay(0);
        } else if (APP.player().key_down(Key::action_2)) {
            text_.clear();
            PLATFORM.fill_overlay(0);
            exit_ = true;
            PLATFORM.screen().fade(
                1.f, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }

    case State::fade_out: {
        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            APP.camera()->reset();
            PLATFORM.screen().fade(
                1.f, ColorConstant::rich_black, {}, true, true);

            state_ = State::quickselect;

        } else {
            const auto amount =
                default_fade +
                (1.f - default_fade) * smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().fade(amount);
        }
        break;
    }
    }

    APP.update_parallax(delta);

    return null_scene();
}



SelectTutorialScene::Factory SelectTutorialScene::factory_;



} // namespace skyland
