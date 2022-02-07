#include "levelCompleteOptionsScene.hpp"
#include "skyland/skyland.hpp"
#include "zoneImageScene.hpp"
#include "readyScene.hpp"



namespace skyland {



static const Float partial_fade_amt = 0.76f;



void LevelCompleteOptionsScene::show_cursor(Platform& pfrm)
{
    int tiles[2] = {0, 0};
    tiles[cursor_] = 436;

    pfrm.set_tile(Layer::overlay, options_[0].coord().x - 2, options_[0].coord().y, tiles[0]);
    pfrm.set_tile(Layer::overlay, options_[1].coord().x - 2, options_[1].coord().y, tiles[1]);
}



ScenePtr<Scene> LevelCompleteOptionsScene::update(Platform& pfrm,
                                                  App& app,
                                                  Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    switch (state_) {
    case State::select:
        timer_ += delta;
        if (app.player().key_down(pfrm, Key::action_1)) {
            timer_ = 0;
            switch (cursor_) {
            case 0:
                state_ = State::fade_out;
                break;

            case 1:
                state_ = State::fade_resume;
                break;

            case 2:
                break;
            }
            options_.clear();
            pfrm.fill_overlay(0);
        } else if (app.player().key_down(pfrm, Key::up) and cursor_ > 0) {
            --cursor_;
            show_cursor(pfrm);

        } else if (app.player().key_down(pfrm, Key::down) and cursor_ < 1) {
            ++cursor_;
            show_cursor(pfrm);
        }
        break;

    case State::fade_resume: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(400);
        if (timer_ > fade_duration) {
            pfrm.screen().schedule_fade(0.f);
            pfrm.load_overlay_texture("overlay");
            pfrm.screen().pixelate(0, false);
            return scene_pool::alloc<ReadyScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().pixelate(amount * 28, false);
            pfrm.screen().schedule_fade(partial_fade_amt - amount * partial_fade_amt);
        }
        break;
    }

    case State::fade_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            pfrm.load_overlay_texture("overlay");

            return scene_pool::alloc<ZoneImageScene>();
        } else {
            const auto amount =
                partial_fade_amt + (1.f - partial_fade_amt) *
                                       smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(amount);
        }
        break;
    }

    case State::fade_in:
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(500);
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::select;
            show_cursor(pfrm);
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(amount * partial_fade_amt);
            pfrm.screen().pixelate(amount * 28, false);
        }
        break;
    }

    return null_scene();
}



void LevelCompleteOptionsScene::enter(Platform& pfrm,
                                      App& app,
                                      Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    pfrm.load_overlay_texture("overlay_island_destroyed");

    auto st = calc_screen_tiles(pfrm);

    const char* resume = "resume";
    const char* world_map = "world map";

    options_.emplace_back(pfrm, world_map,
                          OverlayCoord{u8((st.x - utf8::len(world_map)) / 2), 5});

    options_.emplace_back(pfrm, resume,
                          OverlayCoord{u8((st.x - utf8::len(resume)) / 2), 7});

    if (state_ not_eq State::fade_in) {
        pfrm.screen().pixelate(partial_fade_amt * 28, false);
        show_cursor(pfrm);
    }

}



void LevelCompleteOptionsScene::exit(Platform& pfrm,
                                     App& app,
                                     Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    pfrm.load_overlay_texture("overlay");
    pfrm.screen().pixelate(0.f);
}



}
