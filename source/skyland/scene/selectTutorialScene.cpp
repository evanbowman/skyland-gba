#include "selectTutorialScene.hpp"
#include "fadeInScene.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland {



static const Float default_fade = 0.6f;



void SelectTutorialScene::quick_select(int tutorial_number)
{
    while (tutorial_number > 5) {
        tutorial_number -= 5;
        ++page_;
    }

    cursor_ = tutorial_number;

    state_ = State::quickselect;
}



void SelectTutorialScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.speaker().play_music("unaccompanied_wind", 0);

    app.set_coins(pfrm, 0);

    app.player_island().projectiles().clear();

    pfrm.fill_overlay(0);

    // In case we came from a previous tutorial, give control back to the
    // player.
    app.swap_player<PlayerP1>();

    if (state_ not_eq State::quickselect) {
        pfrm.load_overlay_texture("overlay_challenges");

        pfrm.system_call("v-parallax", (void*)false);
    }

    app.game_mode() = App::GameMode::tutorial;

    if (auto script = pfrm.load_file_contents("scripts", "tutorials.lisp")) {
        lisp::BasicCharSequence seq(script);
        auto result = lisp::dostring(seq, [&pfrm](lisp::Value& err) {
            lisp::DefaultPrinter p;
            lisp::format(&err, p);
            pfrm.fatal(p.fmt_.c_str());
        });
        tutorials_ = result;
        const auto tutorial_count = lisp::length(result);
        page_count_ = tutorial_count / 5 + (tutorial_count % 5 ? 1 : 0);

    } else {
        pfrm.fatal("missing file tutorial.lisp");
    }


    if (state_ not_eq State::quickselect) {
        show_options(pfrm);

        for (int i = 0; i < 16; ++i) {
            for (int j = 0; j < 16; ++j) {
                pfrm.set_tile(Layer::map_0_ext, i, j, 0);
                pfrm.set_tile(Layer::map_1_ext, i, j, 0);
            }
        }

        pfrm.screen().set_view({});

        pfrm.screen().fade(default_fade, ColorConstant::rich_black, {}, false);
    }

    pfrm.delta_clock().reset();

}



void SelectTutorialScene::show_options(Platform& pfrm)
{
    pfrm.screen().clear();
    text_.clear();
    pfrm.screen().display();

    if (not tutorials_) {
        return;
    }

    int index = 0;
    int start_index = page_ * 5;

    lisp::foreach (*tutorials_, [&](lisp::Value* val) {
        if (val->type() not_eq lisp::Value::Type::cons) {
            pfrm.fatal("tutorial list format invalid");
        }

        auto name = val->cons().car();
        if (name->type() not_eq lisp::Value::Type::string) {
            pfrm.fatal("tutorial list format invalid");
        }

        if (index++ < start_index) {
            return;
        }

        text_.emplace_back(pfrm,
                           name->string().value(),
                           OverlayCoord{4, u8(4 + text_.size() * 2)});
    });


    if (page_count_ > 1) {
        int margin = (calc_screen_tiles(pfrm).x - page_count_ * 2) / 2;
        for (int i = 0; i < page_count_; ++i) {
            if (i == page_) {
                pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 83);
            } else {
                pfrm.set_tile(Layer::overlay, margin + i * 2, 18, 82);
            }
        }
    }
}



void prep_level(Platform& pfrm, App& app);



void SelectTutorialScene::exit(Platform& pfrm, App&, Scene& next)
{
    text_.clear();
    pfrm.fill_overlay(0);
    pfrm.load_overlay_texture("overlay");

    pfrm.system_call("v-parallax", (void*)true);
}



void SelectTutorialScene::display(Platform& pfrm, App& app)
{
    if (state_ not_eq State::idle) {
        return;
    }
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(59);

    Vec2<Float> origin;

    auto ambient_movement = 2 * float(sine(4 * 3.14f * 0.004f * timer_ + 180)) /
                            std::numeric_limits<s16>::max();

    origin.x += 16 + ambient_movement;
    origin.y += 32 + cursor_ * 16 - 1;

    cursor.set_position(origin);

    pfrm.screen().draw(cursor);
}



ScenePtr<Scene>
SelectTutorialScene::update(Platform& pfrm, App& app, Microseconds delta)
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

        auto file_name = choice->cons().cdr();
        if (file_name->type() not_eq lisp::Value::Type::string) {
            pfrm.fatal("tutorial list format invalid");
        }

        if (auto script = pfrm.load_file_contents(
                "scripts", file_name->string().value())) {

            lisp::BasicCharSequence seq(script);
            lisp::dostring(seq, [&pfrm](lisp::Value& err) {
                lisp::DefaultPrinter p;
                lisp::format(&err, p);
                pfrm.fatal(p.fmt_.c_str());
            });
            prep_level(pfrm, app);
            app.player_island().repaint(pfrm, app);
            app.player_island().render_exterior(pfrm, app);

            rng::critical_state = 42;

            pfrm.speaker().play_music("sb_solecism", 0);

            return scene_pool::alloc<FadeInScene>();
        } else {
            StringBuffer<32> err("file ");
            err += file_name->string().value();
            err += " missing";
            pfrm.fatal(err.c_str());
        }
        break;
    }

    case State::idle: {
        if (not tutorials_) {
            return null_scene();
        }

        if (app.player().key_down(pfrm, Key::down)) {
            if ((u32)cursor_ < text_.size() - 1) {
                cursor_++;
            }
        }

        if (app.player().key_down(pfrm, Key::up)) {
            if (cursor_) {
                cursor_--;
            }
        }

        if (app.player().key_down(pfrm, Key::right)) {
            if (page_ < page_count_ - 1) {
                ++page_;
                show_options(pfrm);
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::left)) {
            if (page_ > 0) {
                --page_;
                show_options(pfrm);
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::fade_out;
            timer_ = 0;
            text_.clear();
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            text_.clear();
            pfrm.fill_overlay(0);
            exit_ = true;
        }
        break;
    }

    case State::fade_out: {
        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            app.camera()->reset();
            pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

            state_ = State::quickselect;

        } else {
            const auto amount =
                default_fade +
                (1.f - default_fade) * smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(amount);
        }
        break;
    }
    }

    app.update_parallax(delta);

    return null_scene();
}



SelectTutorialScene::Factory SelectTutorialScene::factory_;



} // namespace skyland
