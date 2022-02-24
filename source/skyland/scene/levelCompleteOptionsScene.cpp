#include "levelCompleteOptionsScene.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "zoneImageScene.hpp"



namespace skyland {



static const Float partial_fade_amt = 0.76f;



void update_confetti(Platform& pfrm,
                     App& app,
                     ConfettiBuffer& confetti,
                     Microseconds delta);



void LevelCompleteOptionsScene::show_cursor(Platform& pfrm)
{
    int tiles[2] = {0, 0};
    tiles[cursor_] = 436;

    pfrm.set_tile(Layer::overlay,
                  options_[0].coord().x - 2,
                  options_[0].coord().y,
                  tiles[0]);
    pfrm.set_tile(Layer::overlay,
                  options_[1].coord().x - 2,
                  options_[1].coord().y,
                  tiles[1]);
}



ScenePtr<Scene>
LevelCompleteOptionsScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    if (confetti_ and *confetti_) {
        update_confetti(pfrm, app, **confetti_, delta);
    }

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
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            state_ = State::fade_resume;
            options_.clear();
            pfrm.fill_overlay(0);
            timer_ = 0;
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
            pfrm.screen().schedule_fade(partial_fade_amt -
                                        amount * partial_fade_amt);
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



void LevelCompleteOptionsScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (confetti_ and *confetti_) {
        for (auto& c : **confetti_) {
            Sprite spr_;
            spr_.set_priority(0);
            spr_.set_position({c.x_, c.y_});
            spr_.set_size(Sprite::Size::w16_h32);
            spr_.set_mix({[&] {
                              switch (c.clr_) {
                              default:
                              case 0:
                                  return ColorConstant::spanish_crimson;
                              case 1:
                                  return custom_color(0xbdef84);
                              case 2:
                                  return custom_color(0x006bff);
                              }
                          }(),
                          255});
            spr_.set_texture_index(c.img_ + c.anim_ * 4);
            pfrm.screen().draw(spr_);
        }
    }
}



void LevelCompleteOptionsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    pfrm.load_overlay_texture("overlay_island_destroyed");

    auto st = calc_screen_tiles(pfrm);

    StringBuffer<32> resume = SYSTR(start_menu_resume)->c_str();
    StringBuffer<32> sky_map = SYSTR(start_menu_sky_map)->c_str();

    options_.emplace_back(
        pfrm,
        sky_map.c_str(),
        OverlayCoord{u8((st.x - utf8::len(sky_map.c_str())) / 2), 5});

    options_.emplace_back(
        pfrm,
        resume.c_str(),
        OverlayCoord{u8((st.x - utf8::len(resume.c_str())) / 2), 7});

    if (state_ not_eq State::fade_in) {
        pfrm.screen().pixelate(partial_fade_amt * 28, false);
        show_cursor(pfrm);
    }
}



void LevelCompleteOptionsScene::exit(Platform& pfrm, App& app, Scene& next)
{
    WorldScene::exit(pfrm, app, next);

    pfrm.load_overlay_texture("overlay");
    pfrm.screen().pixelate(0.f);
}



} // namespace skyland
