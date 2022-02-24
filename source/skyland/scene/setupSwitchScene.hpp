#pragma once


#include "globals.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/rooms/switch.hpp"
#include "worldScene.hpp"



namespace skyland {



class SetupSwitchScene : public ActiveWorldScene {
public:
    SetupSwitchScene(const Vec2<u8>& switch_location)
        : switch_location_(switch_location)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        ActiveWorldScene::enter(pfrm, app, prev);

        text_.emplace(
            pfrm, SYSTR(switch_connect_on)->c_str(), OverlayCoord{0, 19});

        auto st = calc_screen_tiles(pfrm);

        for (int i = 0; i < text_->len(); ++i) {
            pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
        }
    }


    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        ActiveWorldScene::exit(pfrm, app, next);

        pfrm.fill_overlay(0);
        text_.reset();
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
            return next;
        }

        cursor_anim_timer_ += delta;
        if (cursor_anim_timer_ > milliseconds(200)) {
            cursor_anim_timer_ -= milliseconds(200);
            cursor_anim_frame_ = not cursor_anim_frame_;
        }

        auto test_key = [&](Key k) {
            return player(app).test_key(
                pfrm, k, milliseconds(500), milliseconds(100));
        };

        player(app).key_held_distribute(pfrm);

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        if (test_key(Key::left)) {
            if (cursor_loc.x > 0) {
                --cursor_loc.x;
            }
        } else if (test_key(Key::right)) {
            if (cursor_loc.x < player_island(app).terrain().size()) {
                ++cursor_loc.x;
            }
        } else if (test_key(Key::up)) {
            if (cursor_loc.y > 6) {
                --cursor_loc.y;
            }
        } else if (test_key(Key::down)) {
            if (cursor_loc.y < 14) {
                ++cursor_loc.y;
            }
        }

        switch (state_) {
        case State::pick_first:
            if (player(app).key_down(pfrm, Key::action_1)) {
                state_ = State::pick_second;
                branch_1_ = cursor_loc;

                pfrm.fill_overlay(0);
                text_.emplace(pfrm,
                              SYSTR(switch_connect_off)->c_str(),
                              OverlayCoord{0, 19});

                auto st = calc_screen_tiles(pfrm);

                for (int i = 0; i < text_->len(); ++i) {
                    pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
                }
            }
            break;

        case State::pick_second:
            if (player(app).key_down(pfrm, Key::action_1)) {
                branch_2_ = cursor_loc;
                cursor_loc = switch_location_;
                if (auto room = player_island(app).get_room(cursor_loc)) {
                    if (auto s = dynamic_cast<Switch*>(room)) {
                        s->branch_1_ = branch_1_;
                        s->branch_2_ = branch_2_;
                        s->setup_ = true;
                    }
                }
                return scene_pool::alloc<ReadyScene>();
            }
            break;
        }

        return null_scene();
    }


    void display(Platform& pfrm, App& app) override
    {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_texture_index(15 + cursor_anim_frame_);

        auto origin = player_island(app).visual_origin();
        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        origin.x += cursor_loc.x * 16;
        origin.y += cursor_loc.y * 16;

        cursor.set_position(origin);
        pfrm.screen().draw(cursor);

        if (state_ == State::pick_second) {
            Sprite icon;
            icon.set_size(Sprite::Size::w16_h32);

            auto pos1 = player_island(app).visual_origin();
            pos1.x += branch_1_.x * 16;
            pos1.y += branch_1_.y * 16;
            icon.set_position(pos1);
            icon.set_texture_index(49);
            pfrm.screen().draw(icon);
        }

        ActiveWorldScene::display(pfrm, app);
    }

private:
    enum class State {
        pick_first,
        pick_second,
    } state_ = State::pick_first;

    std::optional<Text> text_;
    Vec2<u8> switch_location_;
    Vec2<u8> branch_1_;
    Vec2<u8> branch_2_;
    Microseconds cursor_anim_timer_ = 0;

    bool cursor_anim_frame_ = false;
};



} // namespace skyland
