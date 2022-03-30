#include "selectChallengeScene.hpp"
#include "achievementNotificationScene.hpp"
#include "fadeInScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



static const Float default_fade = 0.6f;



void SelectChallengeScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.load_overlay_texture("overlay_challenges");

    pfrm.system_call("v-parallax", (void*)false);

    app.player_island().clear_rooms(pfrm, app);
    app.effects().clear();

    challenges_ = app.invoke_script(pfrm, "/scripts/challenge.lisp");
    // lisp::DefaultPrinter p;
    // lisp::format(*challenges_, p);
    // pfrm.fatal(p.fmt_.c_str());

    const auto challenge_count = lisp::length(*challenges_);
    if (challenge_count not_eq 4) {
        pfrm.fatal("FIXME: update completionist achievement with new count!");
    }

    page_count_ = challenge_count / 5 + (challenge_count % 5 ? 1 : 0);

    show_options(pfrm, app);

    for (int i = 0; i < 16; ++i) {
        for (int j = 0; j < 16; ++j) {
            pfrm.set_tile(Layer::map_0_ext, i, j, 0);
            pfrm.set_tile(Layer::map_1_ext, i, j, 0);
        }
    }

    pfrm.screen().set_view({});

    pfrm.screen().schedule_fade(
        default_fade, ColorConstant::rich_black, {}, false);

    if (not pfrm.speaker().is_music_playing("unaccompanied_wind")) {
        pfrm.speaker().play_music("unaccompanied_wind", 0);
    }
}



void SelectChallengeScene::show_options(Platform& pfrm, App& app)
{
    pfrm.screen().clear();
    text_.clear();
    pfrm.screen().display();

    pfrm.fill_overlay(0);

    if (not challenges_) {
        return;
    }

    int index = 0;
    int start_index = page_ * 5;

    lisp::foreach (*challenges_, [&](lisp::Value* val) {
        if (val->type() not_eq lisp::Value::Type::cons) {
            pfrm.fatal("challenge list format invalid");
        }

        auto name = val->cons().car();
        if (name->type() not_eq lisp::Value::Type::string) {
            pfrm.fatal("challenge list format invalid");
        }

        bool completed = app.gp_.challenge_flags_.get() & (1 << index);

        if (index++ < start_index) {
            return;
        }

        text_.emplace_back(pfrm,
                           name->string().value(),
                           OverlayCoord{4, u8(4 + text_.size() * 2)});

        if (completed) {
            pfrm.set_tile(Layer::overlay,
                          text_.back().coord().x + text_.back().len() + 1,
                          text_.back().coord().y,
                          84);
        }
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



void SelectChallengeScene::exit(Platform& pfrm, App&, Scene& next)
{
    text_.clear();
    pfrm.fill_overlay(0);
    pfrm.load_overlay_texture("overlay");

    pfrm.system_call("v-parallax", (void*)true);

    pfrm.screen().fade(1.f);
}



void SelectChallengeScene::display(Platform& pfrm, App& app)
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
SelectChallengeScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (exit_) {
        return scene_pool::alloc<TitleScreenScene>(2);
    }

    timer_ += delta;

    switch (state_) {
    case State::fade_in:
        for (int i = 0; i < 64; ++i) {
            const auto achievement = achievements::update(pfrm, app);
            if (achievement not_eq achievements::Achievement::none) {
                achievements::award(pfrm, app, achievement);

                pfrm.screen().fade(1.f);

                auto next =
                    scene_pool::make_deferred_scene<SelectChallengeScene>();
                return scene_pool::alloc<AchievementNotificationScene>(
                    achievement, next, true);
            }
        }
        state_ = State::idle;
        break;

    case State::idle: {
        if (not challenges_) {
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
                show_options(pfrm, app);
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::left)) {
            if (page_ > 0) {
                --page_;
                show_options(pfrm, app);
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (app.player().key_down(pfrm, Key::action_1)) {
            state_ = State::fade_out;
            timer_ = 0;
            text_.clear();
            pfrm.fill_overlay(0);
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
            auto index = page_ * 5 + cursor_;
            auto choice = lisp::get_list(*challenges_, index);

            auto file_name = choice->cons().cdr();
            if (file_name->type() not_eq lisp::Value::Type::string) {
                pfrm.fatal("challenge list format invalid");
            }

            app.set_coins(pfrm, 0);

            StringBuffer<100> path("/scripts/");
            path += file_name->string().value();
            app.invoke_script(pfrm, path.c_str());

            prep_level(pfrm, app);

            show_island_exterior(pfrm, app, &app.player_island());

            pfrm.speaker().play_music("sb_solecism", 0);
            return scene_pool::alloc<FadeInScene>();

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



} // namespace skyland
