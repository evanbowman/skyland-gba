#include "playerIslandDestroyedScene.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/serial.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland {



ScenePtr<Scene>
PlayerIslandDestroyedScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    auto pos = island_->get_position();
    if (pos.y < 700) {
        pos.y += sink_speed_ * delta;
    }
    island_->set_position(pos);


    auto origin = island_->origin();
    origin.x += 16 * (island_->terrain().size() / 2);

    timer_ += delta;


    switch (anim_state_) {
    case AnimState::init: {
        pfrm.speaker().stop_music();

        big_explosion(pfrm, app, origin);

        const auto off = 50.f;

        big_explosion(pfrm, app, {origin.x - off, origin.y - off});
        big_explosion(pfrm, app, {origin.x + off, origin.y + off});
        timer_ = 0;

        for (auto& room : app.player_island().rooms()) {
            room->unset_target();
        }

        anim_state_ = AnimState::explosion_wait1;
        break;
    }

    case AnimState::explosion_wait1:
        if (timer_ > milliseconds(300)) {
            big_explosion(pfrm, app, origin);
            const auto off = -50.f;

            big_explosion(pfrm, app, {origin.x - off, origin.y + off});
            big_explosion(pfrm, app, {origin.x + off, origin.y - off});

            anim_state_ = AnimState::explosion_wait2;

            timer_ = 0;
        }
        break;

    case AnimState::explosion_wait2:
        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::fade;
        }
        break;

    case AnimState::fade: {
        constexpr auto fade_duration = seconds(3) + milliseconds(500);
        // if (timer_ > seconds(1) + milliseconds(700) and
        //     not pushed_notification_) {
        //     pushed_notification_ = true;
        //     push_notification(
        //         pfrm, this, locale_string(pfrm, defeated_text_)->c_str());
        // }
        sink_speed_ += 0.0000013f;
        if (timer_ > fade_duration) {
            pfrm.screen().fade(0.f);
            timer_ = 0;
            if (island_ not_eq &app.player_island()) {
                anim_state_ = AnimState::wait_1;
            } else {
                anim_state_ = AnimState::fade_out;
            }

        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(amount, ColorConstant::silver_white);
        }
        break;
    }

    case AnimState::wait_1: {
        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::add_score;
        }
        break;
    }

    case AnimState::add_score: {
        app.coins() += app.victory_coins();
        app.victory_coins() = 0;
        anim_state_ = AnimState::wait_2;

        info(pfrm, serialize(pfrm, app.player_island())->c_str());

        break;
    }

    case AnimState::wait_2: {
        if (timer_ > milliseconds(1000)) {
            timer_ = 0;
            anim_state_ = AnimState::fade_out;
        }
        break;
    }

    case AnimState::fade_out: {
        constexpr auto fade_duration = milliseconds(500);
        if (timer_ > fade_duration) {
            timer_ = 0;
            anim_state_ = AnimState::idle;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_) * 0.6f;
            pfrm.screen().fade(amount);
        }
        break;
    }

    case AnimState::idle:
        if (pfrm.keyboard().down_transition<Key::action_1>()) {
            if (island_ not_eq &app.player_island()) {
                if (app.current_map_location() == Vec2<u8>{7, 1}) {
                    // We're at the exit node. Let's adjust stuff, so that we're at
                    // the beginning of the next zone.
                    app.current_map_location().x = 0;
                    app.world_map().generate();
                    app.zone() += 1;
                }

                app.opponent_island().reset();

                return scene_pool::alloc<WorldMapScene>();
            }
        }
        break;
    }

    return null_scene();
}



} // namespace skyland
