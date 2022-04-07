#include "achievementNotificationScene.hpp"
#include "readyScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene> AchievementNotificationScene::update(Platform& pfrm,
                                                     App& app,
                                                     Microseconds delta)
{
    // NOTE: do not call WorldScene::update, the game should be considered
    // paused.

    constexpr auto fade_duration = milliseconds(300);

    timer_ += delta;

    switch (state_) {
    case State::fade_in: {
        if (skip_fade_ or timer_ > fade_duration) {
            pfrm.screen().schedule_fade(0.5f);
            timer_ = 0;
            state_ = State::animate_box_sweep;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(0.5f * amount);
        }
        break;
    }

    case State::animate_box_sweep: {
        static const auto sweep_duration = milliseconds(100);

        const auto st = calc_screen_tiles(pfrm);

        if (timer_ > sweep_duration) {
            timer_ = 0;
            state_ = State::wait;

            for (int x = 3; x < st.x - 3; ++x) {
                for (int y = 3; y < st.y - 4; ++y) {
                    pfrm.set_tile(Layer::overlay, x, y, 112);
                }
            }

            for (int x = 4; x < st.x - 4; ++x) {
                pfrm.set_tile(Layer::overlay, x, 8, 377);
            }

            pfrm.set_tile(Layer::overlay, st.x - 5, 4, 378);

            auto mt = load_metaclass(achievements::reward(achievement_));
            if (not mt) {
                Platform::fatal("missing metaclass for achievement");
            }

            achievement_text_.emplace(pfrm, OverlayCoord{4, 4});
            achievement_text_->assign(SYSTR(achievement_msg_title)->c_str());

            achievement_name_.emplace(pfrm, OverlayCoord{4, 6});
            achievement_name_->assign(
                loadstr(pfrm, achievements::name(achievement_))->c_str());

            unlocked_text_.emplace(pfrm, OverlayCoord{4, 9});
            unlocked_text_->assign(SYSTR(achievement_msg_unlocked)->c_str());

            StringBuffer<17> temp;

            item_name_.emplace(pfrm, OverlayCoord{9, 11});

            make_format(temp,
                        "% (%,%)",
                        (*mt)->ui_name(pfrm)->c_str(),
                        (*mt)->size().x,
                        (*mt)->size().y);

            item_name_->assign(temp.c_str());

            temp.clear();

            make_format(temp,
                        "%@ %` %hp",
                        (*mt)->cost(),
                        (*mt)->consumes_power(),
                        (*mt)->full_health());

            item_details_.emplace(pfrm, OverlayCoord{9, 13});
            item_details_->assign(temp.c_str());

            auto icon = (*mt)->unsel_icon();
            draw_image(pfrm, 181, 4, 11, 4, 4, Layer::overlay);
            pfrm.load_overlay_chunk(181, icon, 16);

        } else {

            const int total = st.y - 7;
            const int progress =
                total * smoothstep(0.f, sweep_duration, timer_);

            for (int x = 3; x < st.x - 3; ++x) {
                for (int y = 3; y < st.y - 4; ++y) {
                    if (y <= progress + 3) {
                        pfrm.set_tile(Layer::overlay, x, y, 112);
                    }
                }
            }
        }

        break;
    }

    case State::wait:
        if (timer_ > seconds(2) and
            (app.player().key_down(pfrm, Key::action_1) or
             app.player().key_down(pfrm, Key::action_2))) {

            timer_ = 0;
            state_ = State::fade_out;

            achievement_name_.reset();
            achievement_text_.reset();

            pfrm.fill_overlay(0);
        }
        break;

    case State::fade_out:
        if (skip_fade_) {
            return next_scene_();
        }
        if (timer_ > fade_duration) {
            pfrm.screen().fade(0.f);
            return next_scene_();
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(0.5f * amount);
        }
        break;
    }

    return null_scene();
}



void AchievementNotificationScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    // pfrm.screen().schedule_fade(0.5f);
}



void AchievementNotificationScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);
}



} // namespace skyland
